use crate::LowerModule;
use error_snippet::Result;

use lume_ast::{self as ast};
use lume_hir::{self as hir};

impl LowerModule<'_> {
    /// Lowers the given AST block into a HIR block, within an nested scope.
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn block(&mut self, expr: ast::Block) -> hir::Block {
        self.locals.push_frame();

        let statements = self.statements(expr.statements);
        let location = self.location(expr.location);

        self.locals.pop_frame();

        hir::Block { statements, location }
    }

    /// Lowers the given AST block into a HIR block, within an isolated scope.
    ///
    /// This functions much like [`block`], but pushes a boundary into the symbol table,
    /// separating local variables within the block from the parent scope.
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn isolated_block(&mut self, expr: ast::Block, params: &[lume_hir::Parameter]) -> hir::Block {
        self.locals.push_boundary();

        for param in params {
            let decl = lume_hir::VariableDeclaration {
                id: self.next_stmt_id(),
                name: param.name.clone(),
                declared_type: Some(param.param_type.clone()),
                value: lume_hir::Expression::void(),
            };

            self.locals.define_var(decl);
        }

        let statements = self.statements(expr.statements);
        let location = self.location(expr.location);

        self.locals.pop_boundary();

        hir::Block { statements, location }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn statements(&mut self, statements: Vec<ast::Statement>) -> Vec<hir::Statement> {
        statements
            .into_iter()
            .filter_map(|expr| match self.statement(expr) {
                Ok(e) => Some(e),
                Err(err) => {
                    self.dcx.emit(err);
                    None
                }
            })
            .collect::<Vec<_>>()
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn statement(&mut self, expr: ast::Statement) -> Result<hir::Statement> {
        let stmt = match expr {
            ast::Statement::VariableDeclaration(s) => self.stmt_variable(*s)?,
            ast::Statement::Break(s) => self.stmt_break(*s),
            ast::Statement::Continue(s) => self.stmt_continue(*s),
            ast::Statement::Return(s) => self.stmt_return(*s)?,
            ast::Statement::If(e) => self.stmt_if(*e)?,
            ast::Statement::Unless(e) => self.stmt_unless(*e)?,
            ast::Statement::InfiniteLoop(e) => self.stmt_infinite_loop(*e),
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

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
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

        self.locals.define_var(decl.clone());

        let statement = hir::Statement {
            id,
            location,
            kind: hir::StatementKind::Variable(Box::new(decl)),
        };

        Ok(statement)
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_break(&mut self, statement: ast::Break) -> hir::Statement {
        let id = self.next_stmt_id();
        let location = self.location(statement.location);

        hir::Statement {
            id,
            location,
            kind: hir::StatementKind::Break(Box::new(hir::Break { id })),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_continue(&mut self, statement: ast::Continue) -> hir::Statement {
        let id = self.next_stmt_id();
        let location = self.location(statement.location);

        hir::Statement {
            id,
            location,
            kind: hir::StatementKind::Continue(Box::new(hir::Continue { id })),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
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

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
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

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
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

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn stmt_condition(&mut self, expr: ast::Condition) -> Result<hir::Condition> {
        let location = self.location(expr.location);

        let condition = if let Some(cond) = expr.condition {
            Some(self.expression(cond)?)
        } else {
            None
        };

        let block = self.block(expr.block);

        Ok(hir::Condition {
            condition,
            block,
            location,
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_infinite_loop(&mut self, expr: ast::InfiniteLoop) -> hir::Statement {
        let id = self.next_stmt_id();
        let location = self.location(expr.location);
        let block = self.block(expr.block);

        hir::Statement {
            id,
            location: location.clone(),
            kind: hir::StatementKind::InfiniteLoop(Box::new(hir::InfiniteLoop { id, block, location })),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn stmt_iterator_loop(&mut self, expr: ast::IteratorLoop) -> Result<hir::Statement> {
        let id = self.next_stmt_id();
        let location = self.location(expr.location);
        let collection = self.expression(expr.collection)?;
        let block = self.block(expr.block);

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

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn stmt_predicate_loop(&mut self, expr: ast::PredicateLoop) -> Result<hir::Statement> {
        let id = self.next_stmt_id();
        let location = self.location(expr.location);
        let condition = self.expression(expr.condition)?;
        let block = self.block(expr.block);

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
}
