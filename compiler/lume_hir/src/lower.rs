use std::collections::HashMap;

use lume_diag::Result;
use lume_state::{ModuleFileId, hash_id};

use crate::errors::*;
use crate::{self as hir};
use lume_ast::{self as ast, Node};

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
            source: $self.state.source_of($self.file).unwrap().clone(),
            range: $location.into(),
            $( $field: $value ),*
        }
        .into()
    };
}

pub struct LowerModule<'a> {
    /// Defines the ID of the file is being lowered.
    file: ModuleFileId,

    /// Defines the parent state.
    state: &'a lume_state::State,

    /// Defines the type map to register types to.
    map: &'a mut hir::map::Map,

    /// Defines all the local symbols within the current scope.
    locals: hir::symbols::SymbolTable,

    /// Mapping between all imported items and their corresponding item IDs.
    imports: HashMap<String, hir::SymbolName>,

    /// Defines the currently containing namespace expressions exist within, if any.
    namespace: hir::IdentifierPath,

    /// Defines the currently containing class expressions exist within, if any.
    class_stack: Vec<hir::ItemId>,

    /// Defines the current counter for [`LocalId`] instances, so they can stay unique.
    local_id_counter: u64,
}

impl<'a> LowerModule<'a> {
    /// Lowers the single given source module into HIR.
    pub fn lower(
        state: &'a lume_state::State,
        map: &'a mut hir::map::Map,
        file: ModuleFileId,
        expressions: Vec<ast::TopLevelExpression>,
    ) -> Result<()> {
        let mut lower = LowerModule {
            file,
            state,
            map,
            locals: hir::symbols::SymbolTable::new(),
            imports: HashMap::new(),
            namespace: hir::IdentifierPath::empty(),
            class_stack: Vec::new(),
            local_id_counter: 0,
        };

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

        hir::ItemId(self.map.module, id)
    }

    /// Generates the next [`hir::NodeId`] instance in the chain.
    ///
    /// Local IDs are simply incremented over the last used ID, starting from 0.
    fn next_expr_id(&mut self) -> hir::ExpressionId {
        let id = self.local_id_counter;
        self.local_id_counter += 1;

        hir::ExpressionId(self.map.module, hir::LocalId(id))
    }

    /// Generates the next [`hir::NodeId`] instance in the chain.
    ///
    /// Local IDs are simply incremented over the last used ID, starting from 0.
    fn next_stmt_id(&mut self) -> hir::StatementId {
        let id = self.local_id_counter;
        self.local_id_counter += 1;

        hir::StatementId(self.map.module, hir::LocalId(id))
    }

    /// Gets the [`hir::SymbolName`] for the item with the given name.
    fn resolve_symbol_name(&self, name: &String) -> hir::SymbolName {
        for (import, symbol) in &self.imports {
            if import == name {
                return symbol.clone();
            }
        }

        // Since all names hash to the same value, we can compute what the item ID
        // would be, if the symbol is registered within the module.
        self.symbol_name(ast::Identifier {
            name: name.clone(),
            location: ast::Location(0..0),
        })
    }

    fn symbol_name(&self, name: ast::Identifier) -> hir::SymbolName {
        hir::SymbolName {
            name: self.identifier(name),
            namespace: self.namespace.clone(),
        }
    }

    fn identifier(&self, expr: ast::Identifier) -> hir::Identifier {
        hir::Identifier { name: expr.name }
    }

    fn identifier_path(&self, expr: ast::IdentifierPath) -> hir::IdentifierPath {
        let path = expr.path.into_iter().map(|p| self.identifier(p)).collect::<Vec<_>>();

        hir::IdentifierPath { path }
    }

    fn location(&self, expr: ast::Location) -> hir::Location {
        hir::Location {
            file: self.file,
            start: expr.start(),
            length: expr.len(),
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
            ast::TopLevelExpression::Use(f) => self.def_impl(*f)?,
        };

        self.map.items.insert(hir_ast.id(), hir_ast.clone());

        Ok(())
    }

    fn top_import(&mut self, expr: ast::Import) -> Result<()> {
        for imported_name in &expr.names {
            let imported_symbol_name = hir::SymbolName {
                name: self.identifier(imported_name.clone()),
                namespace: self.identifier_path(expr.path.clone()),
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
                    location: expr.location,
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
            location,
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
            location,
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
            location,
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
            location,
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
            location,
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
            ast::Expression::Assignment(e) => self.expr_assignment(*e)?,
            ast::Expression::New(e) => self.expr_new(*e)?,
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

    fn expr_new(&mut self, expr: ast::New) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let location = self.location(expr.location);
        let name = self.type_ref(*expr.name)?;
        let arguments = self.expressions(expr.arguments)?;

        Ok(hir::Expression {
            id,
            location,
            kind: hir::ExpressionKind::New(Box::new(hir::New {
                id,
                name: Box::new(name),
                arguments,
            })),
        })
    }

    fn expr_call(&mut self, expr: ast::Call) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let name = self.identifier(expr.name);
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let arguments = self.expressions(expr.arguments)?;
        let location = self.location(expr.location);

        let kind = if let Some(callee) = expr.callee {
            let callee = self.expression(callee)?;

            hir::ExpressionKind::MethodCall(Box::new(hir::MethodCall {
                id,
                callee,
                name,
                type_parameters,
                arguments,
            }))
        } else {
            let name = self.resolve_symbol_name(&name.name);

            hir::ExpressionKind::FunctionCall(Box::new(hir::FunctionCall {
                id,
                name,
                type_parameters,
                arguments,
            }))
        };

        Ok(hir::Expression { id, location, kind })
    }

    fn expr_literal(&mut self, expr: ast::Literal) -> Result<hir::Expression> {
        let literal = self.literal(expr)?;

        Ok(hir::Expression {
            id: literal.id,
            location: literal.location,
            kind: hir::ExpressionKind::Literal(Box::new(literal)),
        })
    }

    fn expr_member(&mut self, expr: ast::Member) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let location = self.location(expr.location);
        let callee = self.expression(expr.callee)?;

        Ok(hir::Expression {
            id,
            location,
            kind: hir::ExpressionKind::Member(Box::new(hir::Member {
                id,
                callee,
                name: expr.name,
                location,
            })),
        })
    }

    fn expr_range(&mut self, expr: ast::Range) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let location = self.location(expr.location);
        let lower = self.expression(expr.lower)?;
        let upper = self.expression(expr.upper)?;

        let range_type = if expr.inclusive {
            ast::Type::Scalar(Box::new(ast::ScalarType {
                name: "RangeInclusive".to_string(),
                location: lume_ast::Location(0..0),
            }))
        } else {
            ast::Type::Scalar(Box::new(ast::ScalarType {
                name: "Range".to_string(),
                location: lume_ast::Location(0..0),
            }))
        };

        Ok(hir::Expression {
            id,
            location,
            kind: hir::ExpressionKind::New(Box::new(hir::New {
                id,
                name: Box::new(self.type_ref(range_type)?),
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
            location,
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
            ast::TypeDefinition::Class(t) => self.def_class(*t),
            ast::TypeDefinition::Trait(t) => self.def_trait(*t),
            ast::TypeDefinition::Enum(t) => self.def_enum(*t),
            ast::TypeDefinition::Alias(t) => self.def_alias(*t),
        }
    }

    fn def_class(&mut self, expr: ast::ClassDefinition) -> Result<hir::Symbol> {
        let name = self.symbol_name(expr.name);
        let builtin = expr.builtin;
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let location = self.location(expr.location);
        let id = self.item_id(&name);

        self.class_stack.push(id);

        let members = expr
            .members
            .into_iter()
            .map(|m| self.def_class_member(m))
            .collect::<Result<Vec<hir::ClassMember>>>()?;

        self.class_stack.pop();

        Ok(hir::Symbol::Type(Box::new(hir::TypeDefinition::Class(Box::new(
            hir::ClassDefinition {
                id,
                type_id: None,
                name,
                builtin,
                type_parameters,
                members,
                location,
            },
        )))))
    }

    fn def_class_member(&mut self, expr: ast::ClassMember) -> Result<hir::ClassMember> {
        match expr {
            ast::ClassMember::Property(p) => self.def_class_property(*p),
            ast::ClassMember::MethodDefinition(m) => self.def_class_method(*m),
        }
    }

    fn def_class_property(&mut self, expr: ast::Property) -> Result<hir::ClassMember> {
        let visibility = self.visibility(expr.visibility)?;
        let name = self.identifier(expr.name);
        let property_type = self.type_ref(expr.property_type)?;
        let location = self.location(expr.location);

        let default_value = if let Some(def) = expr.default_value {
            Some(self.expression(def)?)
        } else {
            None
        };

        Ok(hir::ClassMember::Property(Box::new(hir::Property {
            prop_id: None,
            name,
            visibility,
            property_type,
            default_value,
            location,
        })))
    }

    fn def_class_method(&mut self, expr: ast::MethodDefinition) -> Result<hir::ClassMember> {
        let visibility = self.visibility(expr.visibility)?;
        let name = self.identifier(expr.name);
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let parameters = self.parameters(expr.parameters)?;
        let return_type = self.opt_type_ref(expr.return_type.and_then(|f| Some(*f)))?;
        let location = self.location(expr.location);

        if expr.external {
            return Ok(hir::ClassMember::ExternalMethod(Box::new(
                hir::ExternalMethodDefinition {
                    method_id: None,
                    name,
                    visibility,
                    type_parameters,
                    parameters,
                    return_type,
                    location,
                },
            )));
        }

        let block = self.isolated_block(expr.block)?;

        Ok(hir::ClassMember::Method(Box::new(hir::MethodDefinition {
            method_id: None,
            name,
            visibility,
            type_parameters,
            parameters,
            return_type,
            block,
            location,
        })))
    }

    fn def_trait(&mut self, expr: ast::TraitDefinition) -> Result<hir::Symbol> {
        let name = self.symbol_name(expr.name);
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let location = self.location(expr.location);
        let id = self.item_id(&name);

        self.class_stack.push(id);

        let methods = expr
            .methods
            .into_iter()
            .map(|m| self.def_trait_methods(m))
            .collect::<Result<Vec<hir::TraitMethodDefinition>>>()?;

        self.class_stack.pop();

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
        let parameters = self.parameters(expr.parameters)?;
        let return_type = self.opt_type_ref(expr.return_type.and_then(|f| Some(*f)))?;
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
        let parameters = self.parameters(expr.parameters)?;
        let return_type = self.opt_type_ref(expr.return_type.and_then(|f| Some(*f)))?;
        let location = self.location(expr.location);
        let id = self.item_id(&name);

        if expr.external {
            return Ok(hir::Symbol::ExternalFunction(Box::new(
                hir::ExternalFunctionDefinition {
                    id,
                    func_id: None,
                    visibility,
                    name,
                    type_parameters,
                    parameters,
                    return_type,
                    location,
                },
            )));
        }

        let block = self.isolated_block(expr.block)?;

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

    fn parameters(&mut self, params: Vec<ast::Parameter>) -> Result<Vec<hir::Parameter>> {
        params
            .into_iter()
            .map(|p| self.parameter(p))
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

    fn def_impl(&mut self, expr: ast::UseTrait) -> Result<hir::Symbol> {
        let name = self.type_ref(*expr.name)?;
        let target = self.type_ref(*expr.target)?;
        let methods = self.def_impl_methods(expr.methods)?;
        let location = self.location(expr.location);

        let id = self.item_id(&name);

        Ok(hir::Symbol::Impl(Box::new(hir::TraitImplementation {
            id,
            name: Box::new(name),
            target: Box::new(target),
            methods,
            location,
        })))
    }

    fn def_impl_methods(
        &mut self,
        methods: Vec<ast::TraitMethodImplementation>,
    ) -> Result<Vec<hir::TraitMethodImplementation>> {
        methods
            .into_iter()
            .map(|m| self.def_impl_method(m))
            .collect::<Result<Vec<_>>>()
    }

    fn def_impl_method(&mut self, expr: ast::TraitMethodImplementation) -> Result<hir::TraitMethodImplementation> {
        let visibility = self.visibility(expr.visibility)?;
        let name = self.symbol_name(expr.name);
        let parameters = self.parameters(expr.parameters)?;
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let return_type = self.opt_type_ref(expr.return_type.and_then(|f| Some(*f)))?;
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

    fn type_ref(&self, expr: ast::Type) -> Result<hir::Type> {
        match expr {
            ast::Type::Scalar(t) => self.type_scalar(*t),
            ast::Type::Array(t) => self.type_array(*t),
            ast::Type::Generic(t) => self.type_generic(*t),
        }
    }

    fn opt_type_ref(&self, expr: Option<ast::Type>) -> Result<Option<hir::Type>> {
        match expr {
            Some(e) => Ok(Some(self.type_ref(e)?)),
            None => Ok(None),
        }
    }

    fn type_scalar(&self, expr: ast::ScalarType) -> Result<hir::Type> {
        let location = self.location(expr.location);
        let name = self.resolve_symbol_name(&expr.name);

        Ok(hir::Type::Scalar(Box::new(hir::ScalarType {
            name,
            type_params: Vec::new(),
            location,
        })))
    }

    fn type_array(&self, expr: ast::ArrayType) -> Result<hir::Type> {
        let element_type = Box::new(self.type_ref(*expr.element_type)?);
        let location = self.location(expr.location);

        Ok(hir::Type::Array(Box::new(hir::ArrayType { element_type, location })))
    }

    fn type_generic(&self, expr: ast::GenericType) -> Result<hir::Type> {
        let location = self.location(expr.location);
        let name = self.resolve_symbol_name(&expr.name.name);

        let type_params = expr
            .type_params
            .into_iter()
            .map(|c| self.type_ref(*c))
            .collect::<Result<Vec<hir::Type>>>()?;

        Ok(hir::Type::Scalar(Box::new(hir::ScalarType {
            name,
            type_params: type_params.into_iter().map(Box::new).collect(),
            location,
        })))
    }
}

#[cfg(test)]
mod tests {
    use lume_parser::parser::Parser;
    use lume_state::ModuleId;

    use super::*;

    fn parse(input: &str) -> Vec<ast::TopLevelExpression> {
        let parser = Parser::parse_str(input);

        parser.unwrap()
    }

    fn lower(input: &str) -> Result<hir::map::Map> {
        let expressions = parse(input);

        let module_id = ModuleId::empty();
        let state = lume_state::State::default();
        let mut map = hir::map::Map::empty(module_id);
        let file = ModuleFileId::empty();

        LowerModule::lower(&state, &mut map, file, expressions)?;

        Ok(map)
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

            insta::assert_debug_snapshot!(lower($input));
        };
    }

    #[test]
    fn test_type_aliasing() {
        assert_snap_eq!("type A = B", "scalar_to_scalar");
        assert_snap_eq!("type A = [B]", "scalar_to_array");
        assert_snap_eq!("type A = B<C>", "scalar_to_generic");
    }
}
