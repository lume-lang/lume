use diag::Result;

use crate::{LookupTable, ModuleFileId, ModuleId, ModuleState, State, hir, id::hash_id};
use ast::ast;

use super::ParsedExpressions;

pub(crate) struct HirLowering<'a> {
    state: &'a mut State,

    module: ModuleId,
}

impl<'a> HirLowering<'a> {
    pub(crate) fn new(state: &'a mut State, module: ModuleId) -> Self {
        HirLowering { state, module }
    }

    /// Lowers the single given module into HIR.
    pub(crate) fn lower(state: &'a mut State, module: ModuleId, ast: ParsedExpressions) -> Result<()> {
        Self::new(state, module).run(ast)
    }

    /// Lowers the selected module expressions into HIR.
    pub(crate) fn run(&mut self, ast: ParsedExpressions) -> Result<()> {
        let mut map = hir::Map::new();

        for (file_id, expressions) in ast {
            let symbols = LowerModuleFile::lower(&mut self.state, self.module, file_id, expressions)?;

            map.0.insert(file_id, symbols);
        }

        let module = self.state.module_mut(self.module).unwrap();
        module.state = ModuleState::Parsed { map };

        Ok(())
    }
}

#[allow(dead_code)]
pub(crate) struct LowerModuleFile<'a> {
    state: &'a mut State,

    /// Defines which parent module is being lowered.
    module: ModuleId,

    /// Defines the file within `module` being lowered.
    file: ModuleFileId,

    /// Defines all the defined symbols within the module file.
    lookup: LookupTable,

    /// Defines the currently containing namespace expressions exist within, if any.
    namespace: Option<hir::IdentifierPath>,

    /// Defines the current counter for [`LocalId`] instances, so they can stay unique.
    local_id_counter: u64,
}

impl<'a> LowerModuleFile<'a> {
    pub(crate) fn new(state: &'a mut State, module: ModuleId, file: ModuleFileId) -> Self {
        LowerModuleFile {
            state,
            module,
            file,
            lookup: LookupTable::new(),
            namespace: None,
            local_id_counter: 0,
        }
    }

    /// Lowers the single given module file into HIR.
    pub(crate) fn lower(
        state: &'a mut State,
        module: ModuleId,
        file: ModuleFileId,
        expressions: Vec<ast::TopLevelExpression>,
    ) -> Result<Vec<hir::Symbol>> {
        Self::new(state, module, file).run(expressions)
    }

    /// Lowers the selected module expressions into HIR.
    pub(crate) fn run(&mut self, expressions: Vec<ast::TopLevelExpression>) -> Result<Vec<hir::Symbol>> {
        // Lower all the types first, so we can reference them in the rest of the lowering process.
        self.lower_types(&expressions)?;

        // Afterwards, lower the rest of the expressions.
        self.lower_rest(&expressions)?;

        Ok(vec![])
    }

    fn lower_types(&mut self, expressions: &Vec<ast::TopLevelExpression>) -> Result<()> {
        for expr in expressions {
            match expr {
                ast::TopLevelExpression::FunctionDefinition(i) => {
                    let func_def = self.def_function(i)?;

                    self.lookup.symbols.push(func_def);
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn lower_rest(&mut self, expressions: &Vec<ast::TopLevelExpression>) -> Result<()> {
        for expr in expressions {
            match expr {
                ast::TopLevelExpression::Import(i) => {
                    let imported_symbols = self.top_import(i)?;

                    self.lookup.symbols.extend(imported_symbols);
                }
                ast::TopLevelExpression::Namespace(i) => {
                    self.namespace = Some(self.top_namespace(i)?);
                }
                expr => {
                    let expression = self.top_level_expression(expr)?;

                    self.lookup.symbols.push(expression);
                }
            }
        }

        Ok(())
    }

    fn next_local_id(&mut self) -> hir::LocalId {
        let id = self.local_id_counter;
        self.local_id_counter += 1;

        hir::LocalId(id)
    }

    fn type_id(&self, name: &ast::Identifier) -> hir::TypeId {
        hir::TypeId(hash_id(name))
    }

    fn symbol_name(&mut self, name: &ast::Identifier) -> Result<hir::SymbolName> {
        let symbol = hir::SymbolName {
            name: self.identifier(name)?,
            namespace: self.namespace.clone(),
        };

        Ok(symbol)
    }

    fn identifier(&mut self, expr: &ast::Identifier) -> Result<hir::Identifier> {
        let symbol = hir::Identifier {
            name: expr.name.clone(),
            location: self.location(&expr.location)?,
        };

        Ok(symbol)
    }

    fn identifier_path(&mut self, expr: &ast::IdentifierPath) -> Result<hir::IdentifierPath> {
        let path = expr
            .path
            .iter()
            .map(|p| self.identifier(p))
            .collect::<Result<Vec<_>>>()?;

        let identifier_path = hir::IdentifierPath {
            path,
            location: self.location(&expr.location)?,
        };

        Ok(identifier_path)
    }

    fn location(&self, expr: &ast::Location) -> Result<hir::Location> {
        let symbol = hir::Location {
            file: self.file,
            range: expr.0.clone(),
        };

        Ok(symbol)
    }

    fn top_level_expression(&mut self, expr: &ast::TopLevelExpression) -> Result<hir::Symbol> {
        match expr {
            ast::TopLevelExpression::TypeDefinition(t) => self.def_type(t),
            ast::TopLevelExpression::FunctionDefinition(f) => self.def_function(f),
            _ => todo!(),
        }
    }

    fn top_namespace(&mut self, expr: &ast::Namespace) -> Result<hir::IdentifierPath> {
        Ok(self.identifier_path(&expr.path)?)
    }

    fn top_import(&mut self, expr: &ast::Import) -> Result<Vec<hir::Symbol>> {
        let namespace = self.identifier_path(&expr.path)?;
        let location = self.location(&expr.location)?;
        let mut symbols = Vec::new();

        for name in &expr.names {
            let ext_symbol = hir::ExternalSymbol {
                name: hir::SymbolName {
                    namespace: Some(namespace.clone()),
                    name: self.identifier(&name)?,
                },
                location: location.clone(),
            };

            symbols.push(hir::Symbol::Extern(Box::new(ext_symbol)));
        }

        Ok(symbols)
    }

    fn block(&mut self, expr: &ast::Block) -> Result<hir::Block> {
        let statements = self.statements(&expr.statements)?;
        let location = self.location(&expr.location)?;

        let block = hir::Block { statements, location };

        Ok(block)
    }

    fn statements(&mut self, statements: &Vec<ast::Statement>) -> Result<Vec<hir::Statement>> {
        statements.iter().map(|s| self.statement(s)).collect::<Result<Vec<_>>>()
    }

    fn statement(&mut self, expr: &ast::Statement) -> Result<hir::Statement> {
        match expr {
            ast::Statement::VariableDeclaration(s) => self.stmt_variable(s),
            ast::Statement::Break(s) => self.stmt_break(s),
            ast::Statement::Continue(s) => self.stmt_continue(s),
            ast::Statement::Return(s) => self.stmt_return(s),
            ast::Statement::Expression(s) => {
                let expr = self.expression(s)?;

                Ok(hir::Statement::Expression(Box::new(expr)))
            }
        }
    }

    fn stmt_variable(&mut self, statement: &ast::VariableDeclaration) -> Result<hir::Statement> {
        let id = self.next_local_id();
        let name = self.identifier(&statement.name)?;
        let variable_type = self.type_ref(statement.variable_type.as_ref().unwrap())?;
        let value = self.expression(&statement.value)?;
        let location = self.location(&statement.location)?;

        if statement.is_const {
            return Ok(hir::Statement::Const(Box::new(hir::ConstDeclaration {
                id,
                name,
                variable_type,
                value,
                location,
            })));
        }

        Ok(hir::Statement::Variable(Box::new(hir::VariableDeclaration {
            id,
            name,
            variable_type,
            value,
            location,
        })))
    }

    fn stmt_break(&mut self, statement: &ast::Break) -> Result<hir::Statement> {
        let location = self.location(&statement.location)?;

        Ok(hir::Statement::Break(Box::new(hir::Break { location })))
    }

    fn stmt_continue(&mut self, statement: &ast::Continue) -> Result<hir::Statement> {
        let location = self.location(&statement.location)?;

        Ok(hir::Statement::Continue(Box::new(hir::Continue { location })))
    }

    fn stmt_return(&mut self, statement: &ast::Return) -> Result<hir::Statement> {
        let value = self.expression(&statement.value)?;
        let location = self.location(&statement.location)?;

        Ok(hir::Statement::Return(Box::new(hir::Return { value, location })))
    }

    fn expression(&mut self, statement: &ast::Expression) -> Result<hir::Expression> {
        match statement {
            ast::Expression::Assignment(e) => self.expr_assignment(e),
            ast::Expression::Call(e) => self.expr_call(e),
            ast::Expression::Literal(e) => self.expr_literal(e),
            ast::Expression::Member(e) => self.expr_member(e),
            ast::Expression::Range(e) => self.expr_range(e),
            ast::Expression::Variable(e) => self.expr_variable(e),
            ast::Expression::If(e) => self.expr_if(e),
            ast::Expression::Unless(e) => self.expr_unless(e),
            ast::Expression::InfiniteLoop(e) => self.expr_infinite_loop(e),
            ast::Expression::IteratorLoop(e) => self.expr_iterator_loop(e),
            ast::Expression::PredicateLoop(e) => self.expr_predicate_loop(e),
        }
    }

    fn expr_assignment(&mut self, _expr: &ast::Assignment) -> Result<hir::Expression> {
        todo!("Not implemented")
    }

    fn expr_call(&mut self, _expr: &ast::Call) -> Result<hir::Expression> {
        todo!("Not implemented")
    }

    fn expr_literal(&mut self, _expr: &ast::Literal) -> Result<hir::Expression> {
        todo!("Not implemented")
    }

    fn expr_member(&mut self, _expr: &ast::Member) -> Result<hir::Expression> {
        todo!("Not implemented")
    }

    fn expr_range(&mut self, _expr: &ast::Range) -> Result<hir::Expression> {
        todo!("Not implemented")
    }

    fn expr_variable(&mut self, _expr: &ast::Variable) -> Result<hir::Expression> {
        todo!("Not implemented")
    }

    fn expr_if(&mut self, _expr: &ast::IfCondition) -> Result<hir::Expression> {
        todo!("Not implemented")
    }

    fn expr_unless(&mut self, _expr: &ast::UnlessCondition) -> Result<hir::Expression> {
        todo!("Not implemented")
    }

    fn expr_infinite_loop(&mut self, _expr: &ast::InfiniteLoop) -> Result<hir::Expression> {
        todo!("Not implemented")
    }

    fn expr_iterator_loop(&mut self, _expr: &ast::IteratorLoop) -> Result<hir::Expression> {
        todo!("Not implemented")
    }

    fn expr_predicate_loop(&mut self, _expr: &ast::PredicateLoop) -> Result<hir::Expression> {
        todo!("Not implemented")
    }

    fn def_type(&mut self, expr: &ast::TypeDefinition) -> Result<hir::Symbol> {
        match expr {
            ast::TypeDefinition::Class(t) => self.def_class(t),
            ast::TypeDefinition::Enum(t) => self.def_enum(t),
            ast::TypeDefinition::Alias(t) => self.def_alias(t),
        }
    }

    fn def_class(&mut self, _expr: &ast::ClassDefinition) -> Result<hir::Symbol> {
        panic!("Class definition not implemented")
    }

    fn def_enum(&mut self, expr: &ast::EnumDefinition) -> Result<hir::Symbol> {
        let type_id = self.type_id(&expr.name);
        let symbol_name = self.symbol_name(&expr.name)?;
        let location = self.location(&expr.location)?;

        let cases = expr
            .cases
            .iter()
            .map(|c| self.def_enum_case(c))
            .collect::<Result<Vec<_>>>()?;

        let symbol = hir::EnumDefinition {
            type_id,
            name: symbol_name,
            cases,
            location,
        };

        Ok(hir::Symbol::Type(Box::new(hir::TypeDefinition::Enum(Box::new(symbol)))))
    }

    fn def_enum_case(&mut self, expr: &ast::EnumDefinitionCase) -> Result<hir::EnumDefinitionCase> {
        let name = self.symbol_name(&expr.name)?;
        let location = self.location(&expr.location)?;

        let parameters = expr
            .parameters
            .iter()
            .map(|c| self.type_ref(c))
            .collect::<Result<Vec<hir::Type>>>()?;

        let symbol = hir::EnumDefinitionCase {
            name,
            parameters: parameters.into_iter().map(|c| Box::new(c)).collect(),
            location,
        };

        Ok(symbol)
    }

    fn def_alias(&mut self, expr: &ast::AliasDefinition) -> Result<hir::Symbol> {
        let type_id = self.type_id(&expr.name);
        let symbol_name = self.symbol_name(&expr.name)?;
        let definition = self.type_ref(&expr.definition)?;
        let location = self.location(&expr.location)?;

        let symbol = hir::AliasDefinition {
            type_id,
            name: symbol_name,
            definition: Box::new(definition),
            location,
        };

        Ok(hir::Symbol::Type(Box::new(hir::TypeDefinition::Alias(Box::new(
            symbol,
        )))))
    }

    fn def_function(&mut self, expr: &ast::FunctionDefinition) -> Result<hir::Symbol> {
        let name = self.symbol_name(&expr.name)?;
        let visibility = self.visibility(&expr.visibility)?;
        let parameters = self.parameters(&expr.parameters)?;
        let return_type = Box::new(self.type_ref(&*expr.return_type)?);
        let location = self.location(&expr.location)?;

        if expr.external {
            return Ok(hir::Symbol::ExternalFunction(Box::new(
                hir::ExternalFunctionDefinition {
                    name,
                    visibility,
                    parameters,
                    return_type,
                    location,
                },
            )));
        }

        let block = self.block(&expr.block)?;

        Ok(hir::Symbol::Function(Box::new(hir::FunctionDefinition {
            name,
            visibility,
            parameters,
            return_type,
            block,
            location,
        })))
    }

    fn visibility(&mut self, expr: &ast::Visibility) -> Result<hir::Visibility> {
        match expr {
            ast::Visibility::Public { .. } => Ok(hir::Visibility::Public),
            ast::Visibility::Private { .. } => Ok(hir::Visibility::Private),
        }
    }

    fn parameters(&mut self, params: &Vec<ast::Parameter>) -> Result<Vec<hir::Parameter>> {
        params.iter().map(|p| self.parameter(p)).collect::<Result<Vec<_>>>()
    }

    fn parameter(&mut self, param: &ast::Parameter) -> Result<hir::Parameter> {
        let name = self.identifier(&param.name)?;
        let param_type = self.type_ref(&param.param_type)?;
        let location = self.location(&param.location)?;

        Ok(hir::Parameter {
            id: self.next_local_id(),
            name,
            param_type,
            location,
        })
    }

    fn type_ref(&mut self, expr: &ast::Type) -> Result<hir::Type> {
        match expr {
            ast::Type::Scalar(t) => self.type_scalar(t),
            ast::Type::Array(t) => self.type_array(t),
            ast::Type::Generic(t) => self.type_generic(t),
        }
    }

    fn type_scalar(&mut self, _expr: &ast::ScalarType) -> Result<hir::Type> {
        panic!("Not implemented")
    }

    fn type_array(&mut self, expr: &ast::ArrayType) -> Result<hir::Type> {
        let element_type = Box::new(self.type_ref(&*expr.element_type)?);
        let location = self.location(&expr.location)?;

        Ok(hir::Type::Array(Box::new(hir::ArrayType { element_type, location })))
    }

    fn type_generic(&mut self, expr: &ast::GenericType) -> Result<hir::Type> {
        let name = self.identifier(&expr.name)?;
        let location = self.location(&expr.location)?;

        let type_params = expr
            .type_params
            .iter()
            .map(|c| self.type_ref(c))
            .collect::<Result<Vec<hir::Type>>>()?;

        Ok(hir::Type::Generic(Box::new(hir::GenericType {
            name,
            type_params: type_params.into_iter().map(|c| Box::new(c)).collect(),
            location,
        })))
    }
}
