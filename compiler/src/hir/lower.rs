use std::collections::HashMap;

use diag::{Result, source::NamedSource};

use crate::{ModuleFileId, ModuleId, State, hir, id::hash_id};
use ast::{Node, ast};
use hir::errors::*;

pub(crate) struct HirLowering<'a> {
    state: &'a State,

    module: ModuleId,

    map: hir::Map,
}

impl<'a> HirLowering<'a> {
    pub(crate) fn new(state: &'a State, module: ModuleId) -> Self {
        HirLowering {
            state,
            module,
            map: hir::Map::empty(),
        }
    }

    /// Lowers the single given module into HIR.
    pub(crate) fn lower(state: &'a State, module: ModuleId, ast: hir::ParsedExpressions) -> Result<hir::map::Map> {
        Self::new(state, module).run(ast)
    }

    /// Lowers the selected module expressions into HIR.
    pub(crate) fn run(mut self, ast: hir::ParsedExpressions) -> Result<hir::Map> {
        for (file_id, expressions) in ast {
            LowerModuleFile::lower(&self.state, &mut self.map, self.module, file_id, expressions)?;
        }

        Ok(self.map)
    }
}

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
            source: $self.source().unwrap(),
            range: $location.into(),
            $( $field: $value ),*
        }
        .into()
    };
}

#[allow(dead_code)]
pub(crate) struct LowerModuleFile<'ctx, 'map> {
    state: &'ctx State,

    /// Defines which parent module is being lowered.
    module: ModuleId,

    /// Defines the file within `module` being lowered.
    file: ModuleFileId,

    /// Defines the type map to register types to.
    map: &'map mut hir::map::Map,

    /// Defines all the local symbols within the current scope.
    locals: hir::symbols::SymbolTable,

    /// Mapping between all imported items and their corresponding item IDs.
    imports: HashMap<String, hir::ItemId>,

    /// Defines the currently containing namespace expressions exist within, if any.
    namespace: hir::IdentifierPath,

    /// Defines the current counter for [`LocalId`] instances, so they can stay unique.
    local_id_counter: u64,
}

impl<'ctx, 'map> LowerModuleFile<'ctx, 'map> {
    pub(crate) fn new(state: &'ctx State, map: &'map mut hir::map::Map, module: ModuleId, file: ModuleFileId) -> Self {
        LowerModuleFile {
            state,
            module,
            file,
            map,
            locals: hir::symbols::SymbolTable::new(),
            imports: HashMap::new(),
            namespace: hir::IdentifierPath::empty(),
            local_id_counter: 0,
        }
    }

    /// Lowers the single given module file into HIR.
    pub(crate) fn lower(
        state: &'ctx State,
        map: &'map mut hir::map::Map,
        module: ModuleId,
        file: ModuleFileId,
        expressions: Vec<ast::TopLevelExpression>,
    ) -> Result<()> {
        Self::new(state, map, module, file).run(expressions)
    }

    /// Lowers the selected module expressions into HIR.
    pub(crate) fn run(&mut self, expressions: Vec<ast::TopLevelExpression>) -> Result<()> {
        for expr in expressions {
            match expr {
                ast::TopLevelExpression::Namespace(i) => self.top_namespace(*i)?,
                expr => self.top_level_expression(expr)?,
            }
        }

        Ok(())
    }

    /// Gets the named source structure of the current module being lowered.
    fn source(&self) -> Option<NamedSource> {
        let module = match self.state.module(self.module) {
            Some(module) => module,
            None => return None,
        };

        let module_file = match module.file(self.file) {
            Some(file) => file,
            None => return None,
        };

        Some(module_file.source.clone())
    }

    /// Converts the given value into an [`hir::ItemId`].
    fn item_id<T: std::hash::Hash + Sized>(&self, value: T) -> hir::ItemId {
        let id = hash_id(&value);

        hir::ItemId(self.module, id)
    }

    /// Generates the next [`hir::LocalId`] instance in the chain.
    ///
    /// Local IDs are simply incremented over the last used ID, starting from 0.
    fn next_local_id(&mut self) -> hir::LocalId {
        let id = self.local_id_counter;
        self.local_id_counter += 1;

        hir::LocalId(id)
    }

    /// Gets the [`hir::ItemId`] for the item with the given name.
    ///
    /// This method takes no notice of which type of item the ID refers to - only whether
    /// it exists within the current module and has the same name as given.
    fn resolve_item(&self, name: &String) -> Option<hir::ItemId> {
        // Since all names hash to the same value, we can compute what the item ID
        // would be, if the symbol is registered within the module.
        let symbol_name = self.symbol_name(ast::Identifier {
            name: name.clone(),
            location: ast::Location(0..0),
        });

        let guessed_item_id = hir::ItemId(self.module, hash_id(&symbol_name));
        if self.map.items.contains_key(&guessed_item_id) {
            return Some(guessed_item_id);
        }

        // If not declared in the current module, look for an imported symbol
        // of the same name.
        match self.imports.get(name) {
            Some(def) => return Some(*def),
            None => {}
        };

        None
    }

    fn symbol_name(&self, name: ast::Identifier) -> hir::SymbolName {
        let symbol = hir::SymbolName {
            name: self.identifier(name),
            namespace: self.namespace.clone(),
        };

        symbol
    }

    fn identifier(&self, expr: ast::Identifier) -> hir::Identifier {
        let symbol = hir::Identifier {
            name: expr.name,
            location: self.location(expr.location),
        };

        symbol
    }

    fn identifier_path(&self, expr: ast::IdentifierPath) -> hir::IdentifierPath {
        let path = expr.path.into_iter().map(|p| self.identifier(p)).collect::<Vec<_>>();

        hir::IdentifierPath {
            path,
            location: self.location(expr.location),
        }
    }

    fn location(&self, expr: ast::Location) -> hir::Location {
        let symbol = hir::Location {
            file: self.file,
            start: expr.start(),
            length: expr.len(),
        };

        symbol
    }

    fn top_namespace(&mut self, expr: ast::Namespace) -> Result<()> {
        self.namespace = self.identifier_path(expr.path);

        Ok(())
    }

    fn top_level_expression(&mut self, expr: ast::TopLevelExpression) -> Result<()> {
        if let ast::TopLevelExpression::Import(i) = expr {
            return self.top_import(*i);
        }

        let hir_ast = match expr {
            ast::TopLevelExpression::TypeDefinition(t) => self.def_type(*t)?,
            ast::TopLevelExpression::FunctionDefinition(f) => self.def_function(*f)?,
            _ => todo!(),
        };

        self.map.items.insert(hir_ast.id(), hir_ast.clone());

        Ok(())
    }

    fn top_import(&mut self, expr: ast::Import) -> Result<()> {
        for import in expr.flatten() {
            let id = hir::ItemId(self.module, hash_id(&import));
            let path = import.path.last().unwrap();

            self.imports.insert(path.name.clone(), id);
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
        match expr {
            ast::Statement::VariableDeclaration(s) => self.stmt_variable(*s),
            ast::Statement::Break(s) => self.stmt_break(*s),
            ast::Statement::Continue(s) => self.stmt_continue(*s),
            ast::Statement::Return(s) => self.stmt_return(*s),
            ast::Statement::Expression(s) => {
                let expr = self.expression(*s)?;

                Ok(hir::Statement {
                    id: expr.id,
                    location: expr.location.clone(),
                    kind: hir::StatementKind::Expression(Box::new(expr)),
                })
            }
        }
    }

    fn stmt_variable(&mut self, statement: ast::VariableDeclaration) -> Result<hir::Statement> {
        let id = self.next_local_id();
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
        let id = self.next_local_id();
        let location = self.location(statement.location);

        Ok(hir::Statement {
            id,
            location,
            kind: hir::StatementKind::Break(Box::new(hir::Break { id })),
        })
    }

    fn stmt_continue(&mut self, statement: ast::Continue) -> Result<hir::Statement> {
        let id = self.next_local_id();
        let location = self.location(statement.location);

        Ok(hir::Statement {
            id,
            location,
            kind: hir::StatementKind::Continue(Box::new(hir::Continue { id })),
        })
    }

    fn stmt_return(&mut self, statement: ast::Return) -> Result<hir::Statement> {
        let id = self.next_local_id();
        let value = self.expression(statement.value)?;
        let location = self.location(statement.location);

        Ok(hir::Statement {
            id,
            location,
            kind: hir::StatementKind::Return(Box::new(hir::Return { id, value })),
        })
    }

    fn expressions(&mut self, expressions: Vec<ast::Expression>) -> Result<Vec<hir::Expression>> {
        expressions.into_iter().map(|expr| self.expression(expr)).collect()
    }

    fn expression(&mut self, statement: ast::Expression) -> Result<hir::Expression> {
        let expr = match statement {
            ast::Expression::Assignment(e) => self.expr_assignment(*e)?,
            ast::Expression::Call(e) => self.expr_call(*e)?,
            ast::Expression::Literal(e) => self.expr_literal(*e)?,
            ast::Expression::Member(e) => self.expr_member(*e)?,
            ast::Expression::Range(e) => self.expr_range(*e)?,
            ast::Expression::Variable(e) => self.expr_variable(*e)?,
            ast::Expression::If(e) => self.expr_if(*e)?,
            ast::Expression::Unless(e) => self.expr_unless(*e)?,
            ast::Expression::InfiniteLoop(e) => self.expr_infinite_loop(*e)?,
            ast::Expression::IteratorLoop(e) => self.expr_iterator_loop(*e)?,
            ast::Expression::PredicateLoop(e) => self.expr_predicate_loop(*e)?,
        };

        self.map.expressions.insert(expr.id, expr.clone());

        Ok(expr)
    }

    fn expr_assignment(&mut self, expr: ast::Assignment) -> Result<hir::Expression> {
        let id = self.next_local_id();
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
        let id = self.next_local_id();
        let name = self.identifier(expr.name);
        let arguments = self.expressions(expr.arguments)?;
        let location = self.location(expr.location);

        let kind = if let Some(callee) = expr.callee {
            let callee = self.expression(callee)?;

            hir::ExpressionKind::MethodCall(Box::new(hir::MethodCall {
                id,
                callee,
                name,
                arguments,
            }))
        } else {
            let reference = match self.resolve_item(&name.name) {
                Some(id) => id,
                None => return Err(err!(self, location, MissingFunction, name, name.name)),
            };

            hir::ExpressionKind::FunctionCall(Box::new(hir::FunctionCall {
                id,
                reference,
                name,
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
        let id = self.next_local_id();
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
        let id = self.next_local_id();
        let location = self.location(expr.location);
        let lower = self.expression(expr.lower)?;
        let upper = self.expression(expr.upper)?;

        Ok(hir::Expression {
            id,
            location: location.clone(),
            kind: hir::ExpressionKind::Range(Box::new(hir::Range {
                id,
                lower,
                upper,
                inclusive: expr.inclusive,
                location,
            })),
        })
    }

    fn expr_variable(&mut self, expr: ast::Variable) -> Result<hir::Expression> {
        let id = self.next_local_id();

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
            })),
        })
    }

    fn expr_if(&self, _expr: ast::IfCondition) -> Result<hir::Expression> {
        todo!("Not implemented")
    }

    fn expr_unless(&self, _expr: ast::UnlessCondition) -> Result<hir::Expression> {
        todo!("Not implemented")
    }

    fn expr_infinite_loop(&self, _expr: ast::InfiniteLoop) -> Result<hir::Expression> {
        todo!("Not implemented")
    }

    fn expr_iterator_loop(&self, _expr: ast::IteratorLoop) -> Result<hir::Expression> {
        todo!("Not implemented")
    }

    fn expr_predicate_loop(&self, _expr: ast::PredicateLoop) -> Result<hir::Expression> {
        todo!("Not implemented")
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
        let id = self.next_local_id();
        let value = expr.value;
        let location = self.location(expr.location);

        Ok(hir::Literal {
            id,
            location,
            kind: hir::LiteralKind::Int(Box::new(hir::IntLiteral { id, value })),
        })
    }

    fn lit_float(&mut self, expr: ast::FloatLiteral) -> Result<hir::Literal> {
        let id = self.next_local_id();
        let value = expr.value;
        let location = self.location(expr.location);

        Ok(hir::Literal {
            id,
            location,
            kind: hir::LiteralKind::Float(Box::new(hir::FloatLiteral { id, value })),
        })
    }

    fn lit_string(&mut self, expr: ast::StringLiteral) -> Result<hir::Literal> {
        let id = self.next_local_id();
        let value = expr.value.clone();
        let location = self.location(expr.location);

        Ok(hir::Literal {
            id,
            location,
            kind: hir::LiteralKind::String(Box::new(hir::StringLiteral { id, value })),
        })
    }

    fn lit_boolean(&mut self, expr: ast::BooleanLiteral) -> Result<hir::Literal> {
        let id = self.next_local_id();
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
            ast::TypeDefinition::Enum(t) => self.def_enum(*t),
            ast::TypeDefinition::Alias(t) => self.def_alias(*t),
        }
    }

    fn def_class(&self, expr: ast::ClassDefinition) -> Result<hir::Symbol> {
        let name = self.symbol_name(expr.name);
        let builtin = expr.builtin;
        let location = self.location(expr.location);
        let id = self.item_id(&name);

        let members = expr
            .members
            .into_iter()
            .map(|m| self.def_class_member(m))
            .collect::<Result<Vec<hir::ClassMember>>>()?;

        Ok(hir::Symbol::Type(Box::new(hir::TypeDefinition::Class(Box::new(
            hir::ClassDefinition {
                id,
                name,
                builtin,
                members,
                location,
            },
        )))))
    }

    fn def_class_member(&self, _expr: ast::ClassMember) -> Result<hir::ClassMember> {
        panic!("Class member not implemented")
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
            parameters: parameters.into_iter().map(|c| Box::new(c)).collect(),
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
                name,
                definition: Box::new(definition),
                location,
            },
        )))))
    }

    fn def_function(&mut self, expr: ast::FunctionDefinition) -> Result<hir::Symbol> {
        let visibility = self.visibility(expr.visibility)?;
        let name = self.symbol_name(expr.name);
        let parameters = self.parameters(expr.parameters)?;
        let return_type = self.type_ref(*expr.return_type)?;
        let location = self.location(expr.location);
        let id = self.item_id(&name);

        if expr.external {
            return Ok(hir::Symbol::ExternalFunction(Box::new(
                hir::ExternalFunctionDefinition {
                    id,
                    visibility,
                    name,
                    parameters,
                    return_type: Box::new(return_type),
                    location,
                },
            )));
        }

        let block = self.isolated_block(expr.block)?;

        Ok(hir::Symbol::Function(Box::new(hir::FunctionDefinition {
            id,
            visibility,
            name,
            parameters,
            return_type: Box::new(return_type),
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
            id: self.next_local_id(),
            name,
            param_type,
            location,
        })
    }

    fn type_ref(&self, expr: ast::Type) -> Result<hir::Type> {
        match expr {
            ast::Type::Scalar(t) => self.type_scalar(*t),
            ast::Type::Array(t) => self.type_array(*t),
            ast::Type::Generic(t) => self.type_generic(*t),
        }
    }

    fn type_scalar(&self, expr: ast::ScalarType) -> Result<hir::Type> {
        let location = self.location(expr.location);
        let type_def = match self.resolve_item(&expr.name) {
            Some(def) => def,
            None => return Err(err!(self, location, MissingType, name, expr.name)),
        };

        Ok(hir::Type::Scalar(Box::new(hir::ScalarType {
            reference: type_def,
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
        let reference = match self.resolve_item(&expr.name.name) {
            Some(def) => def,
            None => return Err(err!(self, location, MissingType, name, expr.name.name)),
        };

        let type_params = expr
            .type_params
            .into_iter()
            .map(|c| self.type_ref(*c))
            .collect::<Result<Vec<hir::Type>>>()?;

        Ok(hir::Type::Generic(Box::new(hir::GenericType {
            reference,
            type_params: type_params.into_iter().map(|c| Box::new(c)).collect(),
            location,
        })))
    }
}
