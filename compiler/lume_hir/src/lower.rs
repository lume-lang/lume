use std::collections::HashMap;
use std::sync::Arc;

use arc::Project;
use error_snippet::Result;
use lume_parser::Parser;
use lume_span::{Location, SourceFile, hash_id};
use lume_types::SymbolName;

use crate::stdlib::Assets;
use crate::{self as hir};
use crate::{SELF_TYPE_NAME, errors::*};
use lume_ast::{self as ast, Node};

const ARRAY_STD_TYPE: &str = "Array";
const ARRAY_WITH_CAPACITY_FUNC: &str = "with_capacity";

const RANGE_STD_TYPE: &str = "Range";
const RANGE_INCLUSIVE_STD_TYPE: &str = "RangeInclusive";
const RANGE_NEW_FUNC: &str = "new";

const DEFAULT_STD_IMPORTS: &[&str] = &[
    "Boolean",
    "String",
    "Int8",
    "UInt8",
    "Int16",
    "UInt16",
    "Int32",
    "UInt32",
    "Int64",
    "UInt64",
    "IntPtr",
    "UIntPtr",
    "Float",
    "Double",
    "Array",
    "Pointer",
    "Range",
    "RangeInclusive",
];

macro_rules! err {
    (
        $self:expr,
        $location:expr,
        $kind:ident $(,)?
        $(
            $field: ident,
            $value: expr
        ),*
    ) => {
        $kind {
            source: $self.file.clone(),
            range: $location.index,
            $( $field: $value ),*
        }
        .into()
    };
}

pub struct LowerState<'a> {
    /// Defines the project of the current package project.
    project: &'a Project,

    /// Defines the state being lowered.
    state: &'a mut lume_state::State,
}

impl<'a> LowerState<'a> {
    /// Creates a new [`LowerState`] instance.
    pub fn new(project: &'a Project, state: &'a mut lume_state::State) -> Self {
        Self { project, state }
    }

    /// Lowers the given project and state into a HIR map.
    pub fn lower(project: &'a Project, state: &'a mut lume_state::State) -> Result<hir::map::Map> {
        let mut lower = LowerState::new(project, state);

        lower.lower_into()
    }

    /// Lowers the current project and state into a HIR map.
    pub fn lower_into(&mut self) -> Result<hir::map::Map> {
        // Create a new HIR map for the module.
        let mut hir = hir::map::Map::empty(self.project.id);

        // Read all the sources files before parsing, so if any of them
        // are inaccessible, we don't waste effort parsing them.
        let source_files = self.source_files()?;

        for source_file in source_files {
            // Register source file in the state.
            self.state.source_map.insert(source_file.clone());

            // Parse the contents of the source file.
            let expressions = Parser::parse_src(self.state, source_file.id)?;

            // Lowers the parsed module expressions down to HIR.
            LowerModule::lower(&mut hir, source_file, expressions)?;
        }

        Ok(hir)
    }

    /// Gets all the source files to include in the compilation.
    fn source_files(&self) -> Result<Vec<Arc<SourceFile>>> {
        let package_id = self.project.id;

        // Add all the source files within the standard library.
        let mut sources_files = Assets::as_sources(package_id)?;

        // As well as all the files within the project itself
        let project_file_names = self.project.files()?;

        sources_files.extend(
            project_file_names
                .into_iter()
                .map(|path| {
                    // We get the relative path of the file within the project,
                    // so error messages don't use the full path to a file.
                    let relative_path = self.project.relative_source_path(&path).to_string_lossy().to_string();

                    let content = std::fs::read_to_string(path)?;

                    Ok(Arc::new(SourceFile::new(package_id, relative_path, content)))
                })
                .collect::<Result<Vec<_>>>()?,
        );

        Ok(sources_files)
    }
}

pub struct LowerModule<'a> {
    /// Defines the file is being lowered.
    file: Arc<SourceFile>,

    /// Defines the type map to register types to.
    map: &'a mut hir::map::Map,

    /// Defines all the local symbols within the current scope.
    locals: hir::symbols::SymbolTable,

    /// Mapping between all imported items and their corresponding item IDs.
    imports: HashMap<String, hir::SymbolName>,

    /// Defines the currently containing namespace expressions exist within, if any.
    namespace: hir::NamespacePath,

    /// Defines the currently containing class expressions exist within, if any.
    self_type: Option<SymbolName>,

    /// Defines the current counter for [`LocalId`] instances, so they can stay unique.
    local_id_counter: u64,

    /// Defines the current counter for [`ItemId`] instances, which refer to implementation blocks.
    impl_id_counter: u64,
}

impl<'a> LowerModule<'a> {
    /// Creates a new lowerer for creating HIR maps from AST.
    pub fn new(map: &mut hir::map::Map, file: Arc<SourceFile>) -> LowerModule {
        LowerModule {
            file,
            map,
            locals: hir::symbols::SymbolTable::new(),
            imports: HashMap::new(),
            namespace: hir::NamespacePath::empty(),
            self_type: None,
            local_id_counter: 0,
            impl_id_counter: 0,
        }
    }

    /// Lowers the single given source module into HIR.
    pub fn lower(
        map: &'a mut hir::map::Map,
        file: Arc<SourceFile>,
        expressions: Vec<ast::TopLevelExpression>,
    ) -> Result<()> {
        let mut lower = LowerModule::new(map, file);
        lower.insert_implicit_imports()?;

        for expr in expressions {
            lower.top_level_expression(expr)?;
        }

        Ok(())
    }

    /// Adds implicit imports to the module.
    fn insert_implicit_imports(&mut self) -> Result<()> {
        let import_item = ast::Import::std(DEFAULT_STD_IMPORTS);

        self.top_import(import_item)
    }

    /// Converts the given value into an [`hir::ItemId`].
    fn item_id<T: std::hash::Hash + Sized>(&self, value: T) -> hir::ItemId {
        let id = hash_id(&value);

        hir::ItemId(self.map.package, id)
    }

    /// Converts the given value into an [`hir::ItemId`].
    ///
    /// Since implementations often use the parent type as the hash value,
    /// there *will* be key collisions where an implementation will get the same [`hir::ItemId`]
    /// as the parent type, which would override the original type. So, the given Lume file:
    ///
    /// ```lm
    /// // uses `Foo` as it's hash value
    /// struct Foo {}
    ///
    /// // also uses `Foo` as it's hash value (!!!)
    /// impl Foo {}
    /// ```
    fn impl_id<T: std::hash::Hash + Sized>(&mut self, value: T) -> hir::ItemId {
        let id = hash_id(&hash_id(&value).wrapping_add(self.impl_id_counter));

        self.impl_id_counter += 1;

        hir::ItemId(self.map.package, id)
    }

    /// Generates the next [`hir::NodeId`] instance in the chain.
    ///
    /// Local IDs are simply incremented over the last used ID, starting from 0.
    fn next_expr_id(&mut self) -> hir::ExpressionId {
        let id = self.local_id_counter;
        self.local_id_counter += 1;

        hir::ExpressionId(self.map.package, hir::LocalId(id))
    }

    /// Generates the next [`hir::NodeId`] instance in the chain.
    ///
    /// Local IDs are simply incremented over the last used ID, starting from 0.
    fn next_stmt_id(&mut self) -> hir::StatementId {
        let id = self.local_id_counter;
        self.local_id_counter += 1;

        hir::StatementId(self.map.package, hir::LocalId(id))
    }

    /// Gets the [`hir::SymbolName`] for the item with the given name.
    fn resolve_symbol_name(&self, path: &ast::Path) -> hir::SymbolName {
        if let Some(symbol) = self.resolve_imported_symbol(path) {
            return symbol.clone();
        }

        // Since all names hash to the same value, we can compute what the item ID
        // would be, if the symbol is registered within the module.
        hir::SymbolName {
            namespace: self.identifier_path(path.root.clone()),
            name: self.identifier(path.name.clone()),
            location: self.location(path.location.clone()),
        }
    }

    /// Attemps to resolve a [`hir::SymbolName`] for an imported symbol.
    fn resolve_imported_symbol(&self, path: &ast::Path) -> Option<hir::SymbolName> {
        for (import, symbol) in &self.imports {
            // Match against imported paths, which match the first segment of the imported path.
            //
            // This handles situations where a subpath was imported, which is then referenced later, such as:
            //
            // ```lume
            //     import std (io);
            //
            //     io::File::from_path("foo.txt");
            // ```
            if let Some(name) = path.root.path.first() {
                if name.name == *import {
                    // Since we only matched a subset of the imported symbol,
                    // we need to merge the two paths together, so it forms a fully-qualified path.
                    let mut merged_symbol = SymbolName {
                        namespace: lume_types::NamespacePath::empty(),
                        name: lume_types::Identifier::from(&path.name.name),
                        location: self.location(path.location.clone()),
                    };

                    for segment in &symbol.namespace.path {
                        merged_symbol
                            .namespace
                            .path
                            .push(lume_types::Identifier::from(&segment.name));
                    }

                    for segment in &path.root.path {
                        merged_symbol
                            .namespace
                            .path
                            .push(lume_types::Identifier::from(&segment.name));
                    }

                    return Some(merged_symbol);
                }
            }
            // Match against the the imported symbol directly, such as:
            //
            // ```lume
            //     import std.io (File);
            //
            //     File::from_path("foo.txt");
            // ```
            else if path.name.name == *import {
                return Some(symbol.clone());
            }
        }

        None
    }

    fn symbol_name(&self, name: ast::Identifier) -> hir::SymbolName {
        let location = self.location(name.location.clone());

        hir::SymbolName {
            name: self.identifier(name),
            namespace: self.namespace.clone(),
            location,
        }
    }

    fn identifier(&self, expr: ast::Identifier) -> hir::Identifier {
        let location = self.location(expr.location.clone());

        hir::Identifier {
            name: expr.name,
            location,
        }
    }

    fn identifier_path(&self, expr: ast::NamespacePath) -> hir::NamespacePath {
        let location = self.location(expr.location.clone());
        let path = expr.path.into_iter().map(|p| self.identifier(p)).collect::<Vec<_>>();

        hir::NamespacePath { path, location }
    }

    fn location(&self, expr: ast::Location) -> Location {
        Location {
            file: self.file.clone(),
            index: expr.0,
        }
    }

    fn top_namespace(&mut self, expr: ast::Namespace) -> Result<()> {
        self.namespace = self.identifier_path(expr.path);

        Ok(())
    }

    fn top_level_expression(&mut self, expr: ast::TopLevelExpression) -> Result<()> {
        let hir_ast = match expr {
            ast::TopLevelExpression::Import(i) => return self.top_import(*i),
            ast::TopLevelExpression::Namespace(i) => return self.top_namespace(*i),
            ast::TopLevelExpression::TypeDefinition(t) => self.def_type(*t)?,
            ast::TopLevelExpression::FunctionDefinition(f) => self.def_function(*f)?,
            ast::TopLevelExpression::Use(f) => self.def_use(*f)?,
            ast::TopLevelExpression::Impl(f) => self.def_impl(*f)?,
        };

        let id = hir_ast.id();

        // Ensure that the ID doesn't overwrite an existing entry.
        debug_assert!(!self.map.items.contains_key(&id));

        self.map.items.insert(id, hir_ast.clone());

        Ok(())
    }

    fn top_import(&mut self, expr: ast::Import) -> Result<()> {
        for imported_name in &expr.names {
            let location = self.location(imported_name.location.clone());

            let imported_symbol_name = hir::SymbolName {
                name: self.identifier(imported_name.clone()),
                namespace: self.identifier_path(expr.path.clone()),
                location,
            };

            self.imports.insert(imported_name.name.clone(), imported_symbol_name);
        }

        Ok(())
    }

    /// Lowers the given AST block into a HIR block, within an nested scope.
    fn block(&mut self, expr: ast::Block) -> Result<hir::Block> {
        self.locals.push_frame();

        let statements = self.statements(expr.statements)?;
        let location = self.location(expr.location);

        self.locals.pop_frame();

        Ok(hir::Block { statements, location })
    }

    /// Lowers the given AST block into a HIR block, within an isolated scope.
    ///
    /// This functions much like [`block`], but pushes a boundary into the symbol table,
    /// separating local variables within the block from the parent scope.
    fn isolated_block(&mut self, expr: ast::Block) -> Result<hir::Block> {
        self.locals.push_boundary();

        let statements = self.statements(expr.statements)?;
        let location = self.location(expr.location);

        self.locals.pop_boundary();

        Ok(hir::Block { statements, location })
    }

    fn statements(&mut self, statements: Vec<ast::Statement>) -> Result<Vec<hir::Statement>> {
        statements
            .into_iter()
            .map(|s| self.statement(s))
            .collect::<Result<Vec<_>>>()
    }

    fn statement(&mut self, expr: ast::Statement) -> Result<hir::Statement> {
        let stmt = match expr {
            ast::Statement::VariableDeclaration(s) => self.stmt_variable(*s)?,
            ast::Statement::Break(s) => self.stmt_break(*s)?,
            ast::Statement::Continue(s) => self.stmt_continue(*s)?,
            ast::Statement::Return(s) => self.stmt_return(*s)?,
            ast::Statement::If(e) => self.stmt_if(*e)?,
            ast::Statement::Unless(e) => self.stmt_unless(*e)?,
            ast::Statement::InfiniteLoop(e) => self.stmt_infinite_loop(*e)?,
            ast::Statement::IteratorLoop(e) => self.stmt_iterator_loop(*e)?,
            ast::Statement::PredicateLoop(e) => self.stmt_predicate_loop(*e)?,
            ast::Statement::Expression(s) => {
                let id = self.next_stmt_id();
                let expr = self.expression(*s)?;

                hir::Statement {
                    id,
                    location: expr.location.clone(),
                    kind: hir::StatementKind::Expression(Box::new(expr)),
                }
            }
        };

        self.map.statements.insert(stmt.id, stmt.clone());

        Ok(stmt)
    }

    fn stmt_variable(&mut self, statement: ast::VariableDeclaration) -> Result<hir::Statement> {
        let id = self.next_stmt_id();
        let name = self.identifier(statement.name);
        let value = self.expression(statement.value)?;
        let location = self.location(statement.location);

        let declared_type = match statement.variable_type {
            Some(t) => Some(self.type_ref(t)?),
            None => None,
        };

        let decl = hir::VariableDeclaration {
            id,
            name,
            declared_type,
            value,
        };

        self.locals.define(decl.clone());

        let statement = hir::Statement {
            id,
            location,
            kind: hir::StatementKind::Variable(Box::new(decl)),
        };

        Ok(statement)
    }

    fn stmt_break(&mut self, statement: ast::Break) -> Result<hir::Statement> {
        let id = self.next_stmt_id();
        let location = self.location(statement.location);

        Ok(hir::Statement {
            id,
            location,
            kind: hir::StatementKind::Break(Box::new(hir::Break { id })),
        })
    }

    fn stmt_continue(&mut self, statement: ast::Continue) -> Result<hir::Statement> {
        let id = self.next_stmt_id();
        let location = self.location(statement.location);

        Ok(hir::Statement {
            id,
            location,
            kind: hir::StatementKind::Continue(Box::new(hir::Continue { id })),
        })
    }

    fn stmt_return(&mut self, statement: ast::Return) -> Result<hir::Statement> {
        let id = self.next_stmt_id();
        let value = self.opt_expression(statement.value)?;
        let location = self.location(statement.location);

        Ok(hir::Statement {
            id,
            location,
            kind: hir::StatementKind::Return(Box::new(hir::Return { id, value })),
        })
    }

    fn stmt_if(&mut self, expr: ast::IfCondition) -> Result<hir::Statement> {
        let id = self.next_stmt_id();
        let location = self.location(expr.location);

        let cases = expr
            .cases
            .into_iter()
            .map(|c| self.stmt_condition(c))
            .collect::<Result<Vec<_>>>()?;

        Ok(hir::Statement {
            id,
            location: location.clone(),
            kind: hir::StatementKind::If(Box::new(hir::If { id, cases, location })),
        })
    }

    fn stmt_unless(&mut self, expr: ast::UnlessCondition) -> Result<hir::Statement> {
        let id = self.next_stmt_id();
        let location = self.location(expr.location);

        let cases = expr
            .cases
            .into_iter()
            .map(|c| self.stmt_condition(c))
            .collect::<Result<Vec<_>>>()?;

        Ok(hir::Statement {
            id,
            location: location.clone(),
            kind: hir::StatementKind::Unless(Box::new(hir::Unless { id, cases, location })),
        })
    }

    fn stmt_condition(&mut self, expr: ast::Condition) -> Result<hir::Condition> {
        let id = self.next_stmt_id();
        let location = self.location(expr.location);

        let condition = if let Some(cond) = expr.condition {
            Some(self.expression(cond)?)
        } else {
            None
        };

        let block = self.block(expr.block)?;

        Ok(hir::Condition {
            id,
            location,
            condition,
            block,
        })
    }

    fn stmt_infinite_loop(&mut self, expr: ast::InfiniteLoop) -> Result<hir::Statement> {
        let id = self.next_stmt_id();
        let location = self.location(expr.location);
        let block = self.block(expr.block)?;

        Ok(hir::Statement {
            id,
            location: location.clone(),
            kind: hir::StatementKind::InfiniteLoop(Box::new(hir::InfiniteLoop { id, block, location })),
        })
    }

    fn stmt_iterator_loop(&mut self, expr: ast::IteratorLoop) -> Result<hir::Statement> {
        let id = self.next_stmt_id();
        let location = self.location(expr.location);
        let collection = self.expression(expr.collection)?;
        let block = self.block(expr.block)?;

        Ok(hir::Statement {
            id,
            location: location.clone(),
            kind: hir::StatementKind::IteratorLoop(Box::new(hir::IteratorLoop {
                id,
                collection,
                block,
                location,
            })),
        })
    }

    fn stmt_predicate_loop(&mut self, expr: ast::PredicateLoop) -> Result<hir::Statement> {
        let id = self.next_stmt_id();
        let location = self.location(expr.location);
        let condition = self.expression(expr.condition)?;
        let block = self.block(expr.block)?;

        Ok(hir::Statement {
            id,
            location: location.clone(),
            kind: hir::StatementKind::PredicateLoop(Box::new(hir::PredicateLoop {
                id,
                condition,
                block,
                location,
            })),
        })
    }

    fn expressions(&mut self, expressions: Vec<ast::Expression>) -> Result<Vec<hir::Expression>> {
        expressions.into_iter().map(|expr| self.expression(expr)).collect()
    }

    fn expression(&mut self, statement: ast::Expression) -> Result<hir::Expression> {
        let expr = match statement {
            ast::Expression::Array(e) => self.expr_array(*e)?,
            ast::Expression::Assignment(e) => self.expr_assignment(*e)?,
            ast::Expression::Call(e) => self.expr_call(*e)?,
            ast::Expression::Literal(e) => self.expr_literal(*e)?,
            ast::Expression::Member(e) => self.expr_member(*e)?,
            ast::Expression::Range(e) => self.expr_range(*e)?,
            ast::Expression::Variable(e) => self.expr_variable(*e)?,
        };

        self.map.expressions.insert(expr.id, expr.clone());

        Ok(expr)
    }

    fn opt_expression(&mut self, statement: Option<ast::Expression>) -> Result<Option<hir::Expression>> {
        match statement {
            Some(expr) => Ok(Some(self.expression(expr)?)),
            None => Ok(None),
        }
    }

    fn expr_array(&mut self, expr: ast::Array) -> Result<hir::Expression> {
        // TODO: Implement proper array expression lowering
        let array_path = lume_ast::Path {
            name: lume_ast::Identifier {
                name: String::from(ARRAY_WITH_CAPACITY_FUNC),
                location: expr.location.clone(),
            },
            root: lume_ast::NamespacePath::new(&["std", ARRAY_STD_TYPE]),
            location: expr.location.clone(),
        };

        let id = self.next_expr_id();
        let location = self.location(expr.location);

        Ok(hir::Expression {
            id,
            location: location.clone(),
            kind: hir::ExpressionKind::StaticCall(Box::new(hir::StaticCall {
                id,
                name: self.resolve_symbol_name(&array_path),
                type_arguments: vec![hir::TypeArgument::Implicit {
                    location: hir::Location::empty(),
                }],
                arguments: vec![hir::Expression {
                    id: self.next_expr_id(),
                    kind: hir::ExpressionKind::Literal(Box::new(hir::Literal {
                        id: self.next_expr_id(),
                        kind: hir::LiteralKind::Int(Box::new(hir::IntLiteral {
                            id: self.next_expr_id(),
                            value: usize::cast_signed(expr.values.len()) as i64,
                            kind: hir::IntKind::U64,
                        })),
                        location: hir::Location::empty(),
                    })),
                    location: hir::Location::empty(),
                }],
            })),
        })
    }

    fn expr_assignment(&mut self, expr: ast::Assignment) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let location = self.location(expr.location);
        let target = self.expression(expr.target)?;
        let value = self.expression(expr.value)?;

        Ok(hir::Expression {
            id,
            location,
            kind: hir::ExpressionKind::Assignment(Box::new(hir::Assignment { id, target, value })),
        })
    }

    fn expr_call(&mut self, expr: ast::Call) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let name = self.resolve_symbol_name(&expr.name);
        let type_arguments = self.type_arguments(expr.type_arguments)?;
        let arguments = self.expressions(expr.arguments)?;
        let location = self.location(expr.location);

        let kind = if let Some(callee) = expr.callee {
            let callee = self.expression(callee)?;

            hir::ExpressionKind::InstanceCall(Box::new(hir::InstanceCall {
                id,
                callee,
                name: name.name,
                type_arguments,
                arguments,
            }))
        } else {
            hir::ExpressionKind::StaticCall(Box::new(hir::StaticCall {
                id,
                name,
                type_arguments,
                arguments,
            }))
        };

        Ok(hir::Expression { id, location, kind })
    }

    fn expr_literal(&mut self, expr: ast::Literal) -> Result<hir::Expression> {
        let literal = self.literal(expr)?;

        Ok(hir::Expression {
            id: literal.id,
            location: literal.location.clone(),
            kind: hir::ExpressionKind::Literal(Box::new(literal)),
        })
    }

    fn expr_member(&mut self, expr: ast::Member) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let location = self.location(expr.location);
        let callee = self.expression(expr.callee)?;

        Ok(hir::Expression {
            id,
            location: location.clone(),
            kind: hir::ExpressionKind::Member(Box::new(hir::Member {
                id,
                callee,
                name: expr.name,
                location,
            })),
        })
    }

    fn expr_range(&mut self, expr: ast::Range) -> Result<hir::Expression> {
        let range_type_name = if expr.inclusive {
            RANGE_INCLUSIVE_STD_TYPE
        } else {
            RANGE_STD_TYPE
        };

        let range_type = lume_ast::Path {
            name: lume_ast::Identifier {
                name: String::from(range_type_name),
                location: expr.location.clone(),
            },
            root: lume_ast::NamespacePath::new(&["std", RANGE_NEW_FUNC]),
            location: expr.location.clone(),
        };

        let id = self.next_expr_id();
        let location = self.location(expr.location);
        let lower = self.expression(expr.lower)?;
        let upper = self.expression(expr.upper)?;

        Ok(hir::Expression {
            id,
            location,
            kind: hir::ExpressionKind::StaticCall(Box::new(hir::StaticCall {
                id,
                name: self.resolve_symbol_name(&range_type),
                type_arguments: vec![hir::TypeArgument::Implicit {
                    location: hir::Location::empty(),
                }],
                arguments: vec![lower, upper],
            })),
        })
    }

    fn expr_variable(&mut self, expr: ast::Variable) -> Result<hir::Expression> {
        let id = self.next_expr_id();

        let location = self.location(expr.location().clone());
        let local_id = match self.locals.retrieve(&expr.name.name) {
            Some(id) => id,
            None => return Err(err!(self, location, UndeclaredVariable, name, expr.name.name)),
        };

        Ok(hir::Expression {
            id,
            location: location.clone(),
            kind: hir::ExpressionKind::Variable(Box::new(hir::Variable {
                id,
                reference: local_id.id,
                name: self.identifier(expr.name.clone()),
                location,
            })),
        })
    }

    fn literal(&mut self, expr: ast::Literal) -> Result<hir::Literal> {
        match expr {
            ast::Literal::Int(t) => self.lit_int(*t),
            ast::Literal::Float(t) => self.lit_float(*t),
            ast::Literal::String(t) => self.lit_string(*t),
            ast::Literal::Boolean(t) => self.lit_boolean(*t),
        }
    }

    fn lit_int(&mut self, expr: ast::IntLiteral) -> Result<hir::Literal> {
        let id = self.next_expr_id();
        let value = expr.value;
        let kind = expr.kind.into();
        let location = self.location(expr.location);

        Ok(hir::Literal {
            id,
            location,
            kind: hir::LiteralKind::Int(Box::new(hir::IntLiteral { id, value, kind })),
        })
    }

    fn lit_float(&mut self, expr: ast::FloatLiteral) -> Result<hir::Literal> {
        let id = self.next_expr_id();
        let value = expr.value;
        let kind = expr.kind.into();
        let location = self.location(expr.location);

        Ok(hir::Literal {
            id,
            location,
            kind: hir::LiteralKind::Float(Box::new(hir::FloatLiteral { id, value, kind })),
        })
    }

    fn lit_string(&mut self, expr: ast::StringLiteral) -> Result<hir::Literal> {
        let id = self.next_expr_id();
        let value = expr.value.clone();
        let location = self.location(expr.location);

        Ok(hir::Literal {
            id,
            location,
            kind: hir::LiteralKind::String(Box::new(hir::StringLiteral { id, value })),
        })
    }

    fn lit_boolean(&mut self, expr: ast::BooleanLiteral) -> Result<hir::Literal> {
        let id = self.next_expr_id();
        let value = expr.value;
        let location = self.location(expr.location);

        Ok(hir::Literal {
            id,
            location,
            kind: hir::LiteralKind::Boolean(Box::new(hir::BooleanLiteral { id, value })),
        })
    }

    fn def_type(&mut self, expr: ast::TypeDefinition) -> Result<hir::Symbol> {
        match expr {
            ast::TypeDefinition::Struct(t) => self.def_struct(*t),
            ast::TypeDefinition::Trait(t) => self.def_trait(*t),
            ast::TypeDefinition::Enum(t) => self.def_enum(*t),
            ast::TypeDefinition::Alias(t) => self.def_alias(*t),
        }
    }

    fn def_struct(&mut self, expr: ast::StructDefinition) -> Result<hir::Symbol> {
        let name = self.symbol_name(expr.name);
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let location = self.location(expr.location);
        let id = self.item_id(&name);

        self.self_type = Some(name.clone());

        let properties = expr
            .properties
            .into_iter()
            .map(|m| self.def_property(m))
            .collect::<Result<Vec<hir::Property>>>()?;

        self.self_type = None;

        Ok(hir::Symbol::Type(Box::new(hir::TypeDefinition::Struct(Box::new(
            hir::StructDefinition {
                id,
                type_id: None,
                name,
                builtin: expr.builtin,
                type_parameters,
                properties,
                methods: Vec::new(),
                location,
            },
        )))))
    }

    fn def_property(&mut self, expr: ast::Property) -> Result<hir::Property> {
        let visibility = self.visibility(expr.visibility)?;
        let name = self.identifier(expr.name);
        let property_type = self.type_ref(expr.property_type)?;
        let location = self.location(expr.location);

        let default_value = if let Some(def) = expr.default_value {
            Some(self.expression(def)?)
        } else {
            None
        };

        Ok(hir::Property {
            prop_id: None,
            name,
            visibility,
            property_type,
            default_value,
            location,
        })
    }

    fn def_impl(&mut self, expr: ast::Implementation) -> Result<hir::Symbol> {
        let target = self.type_ref(*expr.name)?;
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let location = self.location(expr.location);

        let id = self.impl_id(&target);

        self.self_type = Some(target.name.clone());

        let methods = expr
            .methods
            .into_iter()
            .map(|m| self.def_impl_method(m))
            .collect::<Result<Vec<hir::MethodDefinition>>>()?;

        self.self_type = None;

        Ok(hir::Symbol::Impl(Box::new(hir::Implementation {
            id,
            impl_id: None,
            target: Box::new(target),
            methods,
            type_parameters,
            location,
        })))
    }

    fn def_impl_method(&mut self, expr: ast::MethodDefinition) -> Result<hir::MethodDefinition> {
        let visibility = self.visibility(expr.visibility)?;
        let name = self.identifier(expr.name);
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let parameters = self.parameters(expr.parameters, true)?;
        let return_type = self.opt_type_ref(expr.return_type.map(|f| *f))?;
        let location = self.location(expr.location);

        let block = if expr.external {
            None
        } else {
            Some(self.isolated_block(expr.block)?)
        };

        Ok(hir::MethodDefinition {
            method_id: None,
            name,
            visibility,
            type_parameters,
            parameters,
            return_type,
            block,
            location,
        })
    }

    fn def_trait(&mut self, expr: ast::TraitDefinition) -> Result<hir::Symbol> {
        let name = self.symbol_name(expr.name);
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let location = self.location(expr.location);
        let id = self.item_id(&name);

        self.self_type = Some(name.clone());

        let methods = expr
            .methods
            .into_iter()
            .map(|m| self.def_trait_methods(m))
            .collect::<Result<Vec<hir::TraitMethodDefinition>>>()?;

        self.self_type = None;

        Ok(hir::Symbol::Type(Box::new(hir::TypeDefinition::Trait(Box::new(
            hir::TraitDefinition {
                id,
                type_id: None,
                name,
                type_parameters,
                methods,
                location,
            },
        )))))
    }

    fn def_trait_methods(&mut self, expr: ast::TraitMethodDefinition) -> Result<hir::TraitMethodDefinition> {
        let visibility = self.visibility(expr.visibility)?;
        let name = self.identifier(expr.name);
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let parameters = self.parameters(expr.parameters, true)?;
        let return_type = self.opt_type_ref(expr.return_type.map(|f| *f))?;
        let location = self.location(expr.location);

        let block = if let Some(block) = expr.block {
            Some(self.isolated_block(block)?)
        } else {
            None
        };

        Ok(hir::TraitMethodDefinition {
            method_id: None,
            name,
            visibility,
            type_parameters,
            parameters,
            return_type,
            block,
            location,
        })
    }

    fn def_enum(&self, expr: ast::EnumDefinition) -> Result<hir::Symbol> {
        let name = self.symbol_name(expr.name);
        let location = self.location(expr.location);
        let id = self.item_id(&name);

        let cases = expr
            .cases
            .into_iter()
            .map(|c| self.def_enum_case(c))
            .collect::<Result<Vec<hir::EnumDefinitionCase>>>()?;

        Ok(hir::Symbol::Type(Box::new(hir::TypeDefinition::Enum(Box::new(
            hir::EnumDefinition {
                id,
                type_id: None,
                name,
                cases,
                location,
            },
        )))))
    }

    fn def_enum_case(&self, expr: ast::EnumDefinitionCase) -> Result<hir::EnumDefinitionCase> {
        let name = self.symbol_name(expr.name);
        let location = self.location(expr.location);
        let id = self.item_id(&name);

        let parameters = expr
            .parameters
            .into_iter()
            .map(|c| self.type_ref(*c))
            .collect::<Result<Vec<hir::Type>>>()?;

        let symbol = hir::EnumDefinitionCase {
            id,
            name,
            parameters: parameters.into_iter().map(Box::new).collect(),
            location,
        };

        Ok(symbol)
    }

    fn def_alias(&self, expr: ast::AliasDefinition) -> Result<hir::Symbol> {
        let name = self.symbol_name(expr.name);
        let definition = self.type_ref(*expr.definition)?;
        let location = self.location(expr.location);
        let id = self.item_id(&name);

        Ok(hir::Symbol::Type(Box::new(hir::TypeDefinition::Alias(Box::new(
            hir::AliasDefinition {
                id,
                type_id: None,
                name,
                definition: Box::new(definition),
                location,
            },
        )))))
    }

    fn def_function(&mut self, expr: ast::FunctionDefinition) -> Result<hir::Symbol> {
        let visibility = self.visibility(expr.visibility)?;
        let name = self.symbol_name(expr.name);
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let parameters = self.parameters(expr.parameters, false)?;
        let return_type = self.opt_type_ref(expr.return_type.map(|f| *f))?;
        let location = self.location(expr.location);
        let id = self.item_id(&name);

        let block = if expr.external {
            None
        } else {
            Some(self.isolated_block(expr.block)?)
        };

        Ok(hir::Symbol::Function(Box::new(hir::FunctionDefinition {
            id,
            func_id: None,
            visibility,
            name,
            type_parameters,
            parameters,
            return_type,
            block,
            location,
        })))
    }

    fn visibility(&self, expr: ast::Visibility) -> Result<hir::Visibility> {
        match expr {
            ast::Visibility::Public { .. } => Ok(hir::Visibility::Public),
            ast::Visibility::Private { .. } => Ok(hir::Visibility::Private),
        }
    }

    fn parameters(&mut self, params: Vec<ast::Parameter>, allow_self: bool) -> Result<Vec<hir::Parameter>> {
        params
            .into_iter()
            .enumerate()
            .map(|(index, param)| {
                // Make sure that `self` is the first parameter.
                //
                // While it doesn't change must in the view of the compiler,
                // using `self` as the first parameter is a best practice, since it's
                // so much easier to see whether a method is an instance method or a static method.
                if index > 0 && param.param_type.is_self() {
                    return Err(SelfNotFirstParameter {
                        source: self.file.clone(),
                        range: param.location.0.clone(),
                        ty: String::from(SELF_TYPE_NAME),
                    }
                    .into());
                }

                // Using `self` outside of an object context is not allowed, such as functions.
                if !allow_self && param.param_type.is_self() {
                    return Err(SelfOutsideObjectContext {
                        source: self.file.clone(),
                        range: param.location.0.clone(),
                        ty: String::from(SELF_TYPE_NAME),
                    }
                    .into());
                }

                self.parameter(param)
            })
            .collect::<Result<Vec<_>>>()
    }

    fn parameter(&mut self, param: ast::Parameter) -> Result<hir::Parameter> {
        let name = self.identifier(param.name);
        let param_type = self.type_ref(param.param_type)?;
        let location = self.location(param.location);

        Ok(hir::Parameter {
            id: self.next_expr_id(),
            name,
            param_type,
            location,
        })
    }

    fn def_use(&mut self, expr: ast::UseTrait) -> Result<hir::Symbol> {
        let name = self.type_ref(*expr.name)?;
        let target = self.type_ref(*expr.target)?;
        let methods = self.def_use_methods(expr.methods)?;
        let location = self.location(expr.location);

        let id = self.item_id(&name);

        Ok(hir::Symbol::Use(Box::new(hir::TraitImplementation {
            id,
            name: Box::new(name),
            target: Box::new(target),
            methods,
            location,
        })))
    }

    fn def_use_methods(
        &mut self,
        methods: Vec<ast::TraitMethodImplementation>,
    ) -> Result<Vec<hir::TraitMethodImplementation>> {
        methods
            .into_iter()
            .map(|m| self.def_use_method(m))
            .collect::<Result<Vec<_>>>()
    }

    fn def_use_method(&mut self, expr: ast::TraitMethodImplementation) -> Result<hir::TraitMethodImplementation> {
        let visibility = self.visibility(expr.visibility)?;
        let name = self.symbol_name(expr.name);
        let parameters = self.parameters(expr.parameters, true)?;
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let return_type = self.opt_type_ref(expr.return_type.map(|f| *f))?;
        let block = self.isolated_block(expr.block)?;
        let location = self.location(expr.location);

        Ok(hir::TraitMethodImplementation {
            visibility,
            name,
            parameters,
            type_parameters,
            return_type,
            block,
            location,
        })
    }

    fn type_parameters(&self, params: Vec<ast::TypeParameter>) -> Result<Vec<hir::TypeParameter>> {
        params
            .into_iter()
            .map(|param| {
                let location = self.location(param.name.location.clone());
                let name = self.identifier(param.name);

                let constraints = param
                    .constraints
                    .into_iter()
                    .map(|ty| Ok(Box::new(self.type_ref(*ty)?)))
                    .collect::<Result<Vec<_>>>()?;

                Ok(hir::TypeParameter {
                    name,
                    type_id: None,
                    type_param_id: None,
                    constraints,
                    location,
                })
            })
            .collect::<Result<Vec<_>>>()
    }

    fn type_arguments(&self, params: Vec<ast::TypeArgument>) -> Result<Vec<hir::TypeArgument>> {
        params
            .into_iter()
            .map(|param| {
                let location = self.location(param.ty.location().clone());
                let ty = self.type_ref(param.ty)?;

                Ok(hir::TypeArgument::Named { ty, location })
            })
            .collect::<Result<Vec<_>>>()
    }

    fn type_ref(&self, expr: ast::Type) -> Result<hir::Type> {
        match expr {
            ast::Type::Scalar(t) => self.type_scalar(*t),
            ast::Type::Array(t) => self.type_array(*t),
            ast::Type::Generic(t) => self.type_generic(*t),
            ast::Type::SelfType(t) => self.type_self(*t),
        }
    }

    fn opt_type_ref(&self, expr: Option<ast::Type>) -> Result<Option<hir::Type>> {
        match expr {
            Some(e) => Ok(Some(self.type_ref(e)?)),
            None => Ok(None),
        }
    }

    fn type_scalar(&self, expr: ast::ScalarType) -> Result<hir::Type> {
        let name = self.resolve_symbol_name(&expr.name);
        let location = self.location(expr.name.location);

        Ok(hir::Type {
            name,
            type_params: Vec::new(),
            location,
        })
    }

    fn type_array(&self, expr: ast::ArrayType) -> Result<hir::Type> {
        self.type_std(ARRAY_STD_TYPE, vec![*expr.element_type])
    }

    fn type_generic(&self, expr: ast::GenericType) -> Result<hir::Type> {
        let location = self.location(expr.location);
        let name = self.resolve_symbol_name(&expr.name);

        let type_params = expr
            .type_params
            .into_iter()
            .map(|c| self.type_ref(*c))
            .collect::<Result<Vec<hir::Type>>>()?;

        Ok(hir::Type {
            name,
            type_params: type_params.into_iter().map(Box::new).collect(),
            location,
        })
    }

    fn type_self(&self, expr: ast::SelfType) -> Result<hir::Type> {
        let location = self.location(expr.location);
        let name = match &self.self_type {
            Some(ty) => ty.clone(),
            None => {
                return Err(SelfOutsideClass {
                    source: self.file.clone(),
                    range: location.index,
                    ty: String::from(SELF_TYPE_NAME),
                }
                .into());
            }
        };

        Ok(hir::Type {
            name,
            type_params: Vec::new(),
            location,
        })
    }

    fn type_std(&self, name: &str, type_params: Vec<ast::Type>) -> Result<hir::Type> {
        let type_params = type_params
            .into_iter()
            .map(|ty| Ok(Box::new(self.type_ref(ty)?)))
            .collect::<Result<Vec<_>>>()?;

        Ok(hir::Type {
            name: SymbolName::from_parts(["std"], name),
            type_params,
            location: hir::Location::empty(),
        })
    }
}

#[cfg(test)]
mod tests {
    use lume_parser::Parser;
    use lume_span::PackageId;

    use super::*;

    #[track_caller]
    fn lower(input: &str) -> Result<hir::map::Map> {
        let source = Arc::new(SourceFile::internal(input));
        let mut state = lume_state::State::default();

        state.source_map.insert(source.clone());

        let expressions = Parser::parse_src(&state, source.id).unwrap();

        let module_id = PackageId::empty();
        let mut map = hir::map::Map::empty(module_id);

        LowerModule::lower(&mut map, source, expressions)?;

        Ok(map)
    }

    #[track_caller]
    fn lower_expr(input: &str) -> Result<Vec<hir::Statement>> {
        let source = Arc::new(SourceFile::internal(input));
        let mut state = lume_state::State::default();

        state.source_map.insert(source.clone());

        let mut parser = Parser::new_with_str(input);
        parser.prepare()?;

        let statements = parser.parse_statements()?;

        let module_id = PackageId::empty();
        let mut map = hir::map::Map::empty(module_id);
        let mut lower = LowerModule::new(&mut map, source);

        lower.statements(statements)
    }

    macro_rules! set_snapshot_suffix {
        ($($expr:expr),*) => {
            let mut settings = insta::Settings::clone_current();
            settings.set_snapshot_suffix(format!($($expr,)*));
            let _guard = settings.bind_to_scope();
        }
    }

    macro_rules! assert_snap_eq {
        (
            $input: expr,
            $($expr:expr),+
        ) => {
            set_snapshot_suffix!( $($expr),+ );

            insta::with_settings!({
                description => $input,
                omit_expression => true
            }, {
                insta::assert_debug_snapshot!(lower($input).unwrap());
            });
        };
    }

    macro_rules! assert_err_snap_eq {
        (
            $input: expr,
            $($expr:expr),+
        ) => {
            set_snapshot_suffix!( $($expr),+ );

            insta::with_settings!({
                description => $input,
                omit_expression => true
            }, {
                insta::assert_debug_snapshot!(lower($input).unwrap_err());
            });
        };
    }

    macro_rules! assert_expr_snap_eq {
        (
            $input: expr,
            $($expr:expr),+
        ) => {
            set_snapshot_suffix!( $($expr),+ );

            insta::with_settings!({
                description => $input,
                omit_expression => true
            }, {
                insta::assert_debug_snapshot!(lower_expr($input).unwrap());
            });
        };
    }

    #[test]
    fn test_implicit_imports() {
        for ty in &[
            "Boolean",
            "String",
            "Int8",
            "UInt8",
            "Int16",
            "UInt16",
            "Int32",
            "UInt32",
            "Int64",
            "UInt64",
            "IntPtr",
            "UIntPtr",
            "Float",
            "Double",
            "Array<Int32>",
            "Pointer",
            "Range<Int32>",
            "RangeInclusive<Int32>",
        ] {
            assert_snap_eq!(&format!("fn foo() -> {} {{ }}", ty), "{}", ty);
        }
    }

    #[test]
    fn test_function_definition_snapshots() {
        assert_snap_eq!("fn main() -> void {}", "empty");
        assert_snap_eq!("fn main() -> void { let a = 0; }", "statement");
        assert_snap_eq!("fn main() -> void { let a = 0; let b = 1; }", "statements");
        assert_snap_eq!("fn main() {}", "no_return_type");
        assert_snap_eq!("fn main(argc: u8) -> void { }", "parameter");
        assert_snap_eq!("fn main(argc: u8, arcv: [String]) -> void { }", "parameters");
        assert_snap_eq!("fn external main() -> void", "external");
        assert_snap_eq!("pub fn main() -> void {}", "pub_modifier");
        assert_snap_eq!("fn loop() -> void {}", "reserved_keyword");
    }

    #[test]
    fn test_literal_snapshots() {
        assert_expr_snap_eq!("\"\";", "string_empty");
        assert_expr_snap_eq!("\"string\";", "string_content");
        assert_expr_snap_eq!("true;", "bool_true");
        assert_expr_snap_eq!("false;", "bool_false");
        assert_expr_snap_eq!("let ident = 0;", "ident");
        assert_expr_snap_eq!("let IDENT = 0;", "ident_case");
        assert_expr_snap_eq!("let __IDENT__ = 0;", "ident_underscore");
        assert_expr_snap_eq!("0;", "int");
        assert_expr_snap_eq!("55;", "int_positive");
        assert_expr_snap_eq!("-55;", "int_negative");
        assert_expr_snap_eq!("0x55;", "int_hex_positive");
        assert_expr_snap_eq!("-0x55;", "int_hex_negative");
        assert_expr_snap_eq!("0b01010101;", "int_bin_positive");
        assert_expr_snap_eq!("-0b01010101;", "int_bin_negative");
        assert_expr_snap_eq!("0o125;", "int_oct_positive");
        assert_expr_snap_eq!("-0o125;", "int_oct_negative");
    }

    #[test]
    fn test_conditional_snapshots() {
        assert_expr_snap_eq!("if true { }", "if_empty");
        assert_expr_snap_eq!("if true { let a = 1; }", "if_statement");
        assert_expr_snap_eq!("if true { } else if false { }", "if_else_if_empty");
        assert_expr_snap_eq!("if true { } else { }", "if_else_empty");
        assert_expr_snap_eq!("if true { } else if false { } else { }", "if_else_if_else_empty");
        assert_expr_snap_eq!("unless true { }", "unless_empty");
        assert_expr_snap_eq!("unless true { } else { }", "unless_else_empty");
        assert_expr_snap_eq!("let a = 0; if a == 1 { }", "equality_empty");
        assert_expr_snap_eq!("let a = 0; if a != 1 { }", "inequality_empty");
        assert_expr_snap_eq!("if true { let a = 0; }", "if_statement");
        assert_expr_snap_eq!("if true { let a = 0; let b = 0; }", "if_statements");
        assert_expr_snap_eq!(
            "if true { let a = 0; } else if false { let a = 0; }",
            "else_if_statements"
        );
        assert_expr_snap_eq!("unless true { let a = 0; }", "unless_statement");
    }

    #[test]
    fn test_loop_snapshots() {
        assert_expr_snap_eq!("loop { }", "inf_loop_empty");
        assert_expr_snap_eq!("loop { let a = 0; }", "inf_loop_statement");
        assert_expr_snap_eq!("loop { break; }", "inf_loop_break");
        assert_expr_snap_eq!("loop { continue; }", "inf_loop_continue");

        assert_expr_snap_eq!("while true { }", "pred_loop_empty");
        assert_expr_snap_eq!("while true { let a = 0; }", "pred_loop_statement");
        assert_expr_snap_eq!("while true { break; }", "pred_loop_break");
        assert_expr_snap_eq!("while true { continue; }", "pred_loop_continue");

        assert_expr_snap_eq!("for pattern in [1, 2, 3] { }", "iter_loop_empty");
        assert_expr_snap_eq!("for pattern in [1, 2, 3] { let a = 0; }", "iter_loop_statement");
        assert_expr_snap_eq!("for pattern in [1, 2, 3] { break; }", "iter_loop_break");
        assert_expr_snap_eq!("for pattern in [1, 2, 3] { continue; }", "iter_loop_continue");

        assert_expr_snap_eq!(
            r#"
            let collection = [1, 2, 3];
            for pattern in collection { }"#,
            "iter_loop_empty_var"
        );

        assert_expr_snap_eq!(
            r#"
            let collection = [1, 2, 3];
            for pattern in collection { let a = 0; }"#,
            "iter_loop_statement_var"
        );

        assert_expr_snap_eq!(
            r#"
            let collection = [1, 2, 3];
            for pattern in collection { break; }"#,
            "iter_loop_break_var"
        );

        assert_expr_snap_eq!(
            r#"
            let collection = [1, 2, 3];
            for pattern in collection { continue; }"#,
            "iter_loop_continue_var"
        );
    }

    #[test]
    fn test_range_snapshots() {
        assert_expr_snap_eq!("let _ = (0..1);", "literal_exclusive");
        assert_expr_snap_eq!("let _ = (0..=1);", "literal_inclusive");
        assert_expr_snap_eq!(
            r#"
            let a = 0;
            let b = 1;
            let _ = (a..b);"#,
            "expr_exclusive"
        );

        assert_expr_snap_eq!(
            r#"
            let a = 0;
            let b = 1;
            let _ = (a..=b);"#,
            "expr_inclusive"
        );

        assert_expr_snap_eq!(
            r#"
            let a = 0;
            let b = 1;
            let _ = ((a + b)..(a + b + 1));"#,
            "expr_nested_exclusive"
        );

        assert_expr_snap_eq!(
            r#"
            let a = 0;
            let b = 1;
            let _ = ((a + b)..=(a + b + 1));"#,
            "expr_nested_inclusive"
        );
    }

    #[test]
    fn test_call_snapshots() {
        assert_expr_snap_eq!("let _ = call();", "function_empty");
        assert_expr_snap_eq!("let _ = call(0);", "function_param_1");
        assert_expr_snap_eq!("let _ = call(0, 1);", "function_param_2");
        assert_expr_snap_eq!("let _ = call<T>(0, 1);", "function_generic");

        assert_expr_snap_eq!("let _ = 1.call();", "method_empty");
        assert_expr_snap_eq!("let _ = 1.call(0);", "method_param_1");
        assert_expr_snap_eq!("let _ = 1.call(0, 1);", "method_param_2");
        assert_expr_snap_eq!("let _ = 1.call<T>(0, 1);", "method_generic");
    }

    #[test]
    fn test_return_snapshots() {
        assert_expr_snap_eq!("return;", "empty");
        assert_expr_snap_eq!("return 1;", "scalar");
        assert_expr_snap_eq!(
            r#"
            let a = 1;
            let b = 1;
            let c = 1;

            return a.b(c);"#,
            "call"
        );
    }

    #[test]
    fn test_generic_function_snapshots() {
        assert_snap_eq!("fn test() -> void {}", "no_generics");
        assert_snap_eq!("fn test<>() -> void {}", "empty_generics");
        assert_snap_eq!("fn test<T>() -> void {}", "single_generic");
        assert_snap_eq!("fn test<T1, T2>() -> void {}", "multiple_generics");
        assert_snap_eq!("fn test<T: Numeric>() -> void {}", "constrained_generic");
        assert_snap_eq!("fn test<T1: Numeric, T2: Numeric>() -> void {}", "constrained_generics");
        assert_snap_eq!(
            "fn test<T: Numeric + Floating>() -> void {}",
            "multiple_constrained_types"
        );
        assert_snap_eq!(
            "fn test<T1: Numeric + Floating, T2: Numeric + Floating>() -> void {}",
            "multiple_types_with_multiple_constraints"
        );
    }

    #[test]
    fn test_struct_snapshots() {
        assert_snap_eq!("struct Int32 {}", "empty_decl");
        assert_snap_eq!("impl Int32 {}", "empty_impl");

        assert_snap_eq!(
            r#"
                impl Foo {
                    fn bar() -> Int32 {
                        return 0;
                    }
                }"#,
            "method"
        );

        assert_snap_eq!(
            r#"
                impl Foo {
                    fn bar() { }
                }"#,
            "method_no_ret"
        );

        assert_snap_eq!(
            r#"
                impl Foo {
                    pub fn bar() -> Int32 {
                        return 0;
                    }
                }"#,
            "pub_method"
        );

        assert_snap_eq!(
            r#"
                impl Foo {
                    fn external bar() -> Int32
                }"#,
            "ext_method"
        );

        assert_snap_eq!(
            r#"
                impl Foo {
                    pub fn ==() -> bool {
                        return true;
                    }
                }"#,
            "operator_method"
        );

        assert_snap_eq!(
            r#"
                impl Foo {
                    fn bar<T>() -> Int32 { }
                }"#,
            "generic_method"
        );

        assert_snap_eq!(
            r#"
                struct Foo {
                    x: Int32 = 0;
                }"#,
            "property"
        );

        assert_snap_eq!(
            r#"
                struct Foo {
                    x: Int32;
                }"#,
            "property_no_default"
        );

        assert_snap_eq!(
            r#"
                struct Foo {
                    pub x: Int32 = 1;
                }"#,
            "pub_property"
        );
    }

    #[test]
    fn test_generic_struct_snapshots() {
        assert_snap_eq!("struct Test {}", "no_generics");
        assert_snap_eq!("struct Test<> {}", "empty_generics");
        assert_snap_eq!("struct Test<T> {}", "single_generic");
        assert_snap_eq!("struct Test<T1, T2> {}", "multiple_generics");
        assert_snap_eq!("struct Test<T: Numeric> {}", "constrained_generic");
        assert_snap_eq!("struct Test<T1: Numeric, T2: Numeric> {}", "constrained_generics");
    }

    #[test]
    fn test_generic_method_snapshots() {
        assert_snap_eq!("impl Test { fn test() -> void {} }", "no_generics");
        assert_snap_eq!("impl Test { fn test<>() -> void {} }", "empty_generics");
        assert_snap_eq!("impl Test { fn test<T>() -> void {} }", "single_generic");
        assert_snap_eq!("impl Test { fn test<T1, T2>() -> void {} }", "multiple_generics");
        assert_snap_eq!("impl Test { fn test<T: Numeric>() -> void {} }", "constrained_generic");
        assert_snap_eq!(
            "impl Test { fn test<T1: Numeric, T2: Numeric>() -> void {} }",
            "constrained_generics"
        );
    }

    #[test]
    fn test_enum_snapshots() {
        assert_snap_eq!("enum Foo {}", "empty");
        assert_snap_eq!("enum Foo { Bar }", "single_variant");
        assert_snap_eq!("enum Foo { Bar, Baz }", "multiple_variants");

        assert_snap_eq!(
            r#"
                enum Foo {
                    Bar()
                }"#,
            "variant_param_empty"
        );

        assert_snap_eq!(
            r#"
                enum Foo {
                    Bar(int)
                }"#,
            "variant_param_single"
        );

        assert_snap_eq!(
            r#"
                enum Foo {
                    Bar(int, int)
                }"#,
            "variant_param_multiple"
        );

        assert_snap_eq!(
            r#"
                enum Foo {
                    Bar(int, int),
                    Baz(int, int)
                }"#,
            "multiple_variants_multiple_params"
        );
    }

    #[test]
    fn test_type_alias_snapshots() {
        assert_snap_eq!("type A = B", "scalar");
        assert_snap_eq!("type A = [B]", "array");
        assert_snap_eq!("type A = B<C>", "generic");
    }

    #[test]
    fn test_trait_snapshots() {
        assert_snap_eq!("trait Add { }", "empty");
        assert_snap_eq!("trait Add { pub fn add(other: int) -> int }", "method");
        assert_snap_eq!("trait Add { pub fn add(other: int) -> int { } }", "method_impl");
        assert_snap_eq!("trait Add<T> { }", "generic");
        assert_snap_eq!("trait Add<T1, T2> { }", "generics");
        assert_snap_eq!("trait Add { fn add(other: int) -> int }", "private_method");
        assert_snap_eq!("trait Add<T: Numeric> {}", "constrained_generic");
        assert_snap_eq!("trait Add<T1: Numeric, T2: Numeric> {}", "constrained_generics");
        assert_snap_eq!("trait Add { pub fn add(other: int) { } }", "method_no_ret");
    }

    #[test]
    fn test_use_trait_snapshots() {
        assert_snap_eq!("use Add in Int32 {}", "empty");

        assert_snap_eq!(
            r#"
                use Add in Int32 {
                    fn add(other: Int32) -> Int32 {}
                }"#,
            "priv_method"
        );

        assert_snap_eq!(
            r#"
                use Add in Int32 {
                    pub fn add(other: Int32) -> Int32 {}
                }"#,
            "pub_method"
        );

        assert_snap_eq!(
            r#"
                use Add in Int32 {
                    fn add(other: Int32) {}
                }"#,
            "method_no_ret"
        );

        assert_snap_eq!(
            r#"
                use Cast in Int32 {
                    pub fn to_string() -> String {}

                    pub fn to_int() -> Int32 {}
                }"#,
            "methods"
        );

        assert_snap_eq!(
            r#"
                use Add<Int32> in Int32 {
                    pub fn add(other: Int32) -> Int32 {}
                }"#,
            "generic"
        );

        assert_snap_eq!(
            r#"
                use Add<Int32, Int64> in Int32 {
                    pub fn add(other: Int32) -> Int64 {}
                }"#,
            "generics"
        );

        assert_snap_eq!(
            r#"
                use Enumerable<T> in Vector<T> {
                    pub fn next() -> T {}
                }"#,
            "generic_type"
        );
    }

    #[test]
    fn test_doc_comments_snapshots() {
        assert_snap_eq!(
            r#"/// This is a doc comment
            fn foo() -> void { }"#,
            "function"
        );

        assert_snap_eq!(
            r#"/// This is a doc comment
            struct Foo { }"#,
            "struct"
        );

        assert_snap_eq!(
            r#"struct Foo {
                /// This is a doc comment
                pub bar: Int32 = 0;
            }"#,
            "property"
        );

        assert_snap_eq!(
            r#"impl Foo {
                /// This is a doc comment
                pub fn bar() -> void { }
            }"#,
            "method"
        );

        assert_snap_eq!(
            r#"/// This is a doc comment
            trait Foo { }"#,
            "trait"
        );

        assert_snap_eq!(
            r#"trait Foo {
                /// This is a doc comment
                pub fn bar() -> void { }
            }"#,
            "trait_method"
        );

        assert_snap_eq!(
            r#"/// This is a doc comment
            enum Foo {
                Bar
            }"#,
            "enum"
        );

        assert_snap_eq!(
            r#"enum Foo {
                /// This is a doc comment
                Bar
            }"#,
            "enum_case"
        );

        assert_snap_eq!(
            r#"/// This is a doc comment
            type Foo = Bar"#,
            "type_alias"
        );
    }

    #[test]
    fn test_type_aliasing() {
        assert_snap_eq!("type A = B", "scalar_to_scalar");
        assert_snap_eq!("type A = [B]", "scalar_to_array");
        assert_snap_eq!("type A = B<C>", "scalar_to_generic");
    }

    #[test]
    fn test_using_self_as_parameter() {
        assert_snap_eq!(
            r#"impl Foo {
            pub fn bar(self, a: Int32) -> void { }
        }"#,
            "valid"
        );

        assert_err_snap_eq!(
            r#"impl Foo {
            pub fn bar(a: Int32, self) -> void { }
        }"#,
            "invalid"
        );
    }

    #[test]
    fn test_using_self_in_function() {
        assert_snap_eq!("fn foo() -> void { }", "valid");

        assert_err_snap_eq!("fn foo(self) -> void { }", "invalid");
    }
}
