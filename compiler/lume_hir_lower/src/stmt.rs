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
            self.locals.define(
                param.name.to_string(),
                lume_hir::VariableSource::Parameter(param.clone()),
            );
        }

        let statements = self.statements(expr.statements);
        let location = self.location(expr.location);

        self.locals.pop_boundary();

        hir::Block { statements, location }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn statements(&mut self, statements: Vec<ast::Statement>) -> Vec<lume_span::StatementId> {
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
    fn statement(&mut self, expr: ast::Statement) -> Result<lume_span::StatementId> {
        let stmt = match expr {
            ast::Statement::VariableDeclaration(s) => self.stmt_variable(*s)?,
            ast::Statement::Break(s) => self.stmt_break(*s),
            ast::Statement::Continue(s) => self.stmt_continue(*s),
            ast::Statement::Final(s) => self.stmt_final(*s)?,
            ast::Statement::Return(s) => self.stmt_return(*s)?,
            ast::Statement::InfiniteLoop(e) => self.stmt_infinite_loop(*e),
            ast::Statement::IteratorLoop(e) => self.stmt_iterator_loop(*e)?,
            ast::Statement::PredicateLoop(e) => self.stmt_predicate_loop(*e)?,
            ast::Statement::Expression(s) => {
                let id = self.next_stmt_id();
                let expr = self.expression(*s)?;

                let location = self.map.expression(expr).unwrap().location;

                hir::Statement {
                    id,
                    location,
                    kind: hir::StatementKind::Expression(expr),
                }
            }
        };

        let id = stmt.id;

        self.map.statements.insert(id, stmt);

        Ok(id)
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
            name: name.clone(),
            declared_type,
            value,
            location,
        };

        self.locals
            .define(name.to_string(), lume_hir::VariableSource::Variable(decl.clone()));

        let statement = hir::Statement {
            id,
            location,
            kind: hir::StatementKind::Variable(decl),
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
            kind: hir::StatementKind::Break(hir::Break { id, location }),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_continue(&mut self, statement: ast::Continue) -> hir::Statement {
        let id = self.next_stmt_id();
        let location = self.location(statement.location);

        hir::Statement {
            id,
            location,
            kind: hir::StatementKind::Continue(hir::Continue { id, location }),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_final(&mut self, statement: ast::Final) -> Result<hir::Statement> {
        let id = self.next_stmt_id();
        let value = self.expression(statement.value)?;
        let location = self.map.expression(value).unwrap().location;

        Ok(hir::Statement {
            id,
            location,
            kind: hir::StatementKind::Final(hir::Final { id, value, location }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn stmt_return(&mut self, statement: ast::Return) -> Result<hir::Statement> {
        let id = self.next_stmt_id();
        let value = self.opt_expression(statement.value)?;
        let location = self.location(statement.location);

        Ok(hir::Statement {
            id,
            location,
            kind: hir::StatementKind::Return(hir::Return { id, value, location }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_infinite_loop(&mut self, expr: ast::InfiniteLoop) -> hir::Statement {
        let id = self.next_stmt_id();
        let location = self.location(expr.location);
        let block = self.block(expr.block);

        hir::Statement {
            id,
            location,
            kind: hir::StatementKind::InfiniteLoop(hir::InfiniteLoop { id, block, location }),
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
            location,
            kind: hir::StatementKind::IteratorLoop(hir::IteratorLoop {
                id,
                collection,
                block,
                location,
            }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn stmt_predicate_loop(&mut self, expr: ast::PredicateLoop) -> Result<hir::Statement> {
        let id = self.next_stmt_id();
        let location = self.location(expr.location);

        let block = self.block(lume_ast::Block {
            statements: vec![lume_ast::Statement::Expression(Box::new(lume_ast::Expression::If(
                Box::new(lume_ast::IfCondition {
                    cases: vec![
                        lume_ast::Condition {
                            condition: Some(expr.condition),
                            block: expr.block,
                            location: lume_ast::Location(0..0),
                        },
                        lume_ast::Condition {
                            condition: None,
                            block: lume_ast::Block {
                                statements: vec![lume_ast::Statement::Break(Box::new(lume_ast::Break {
                                    location: lume_ast::Location(0..0),
                                }))],
                                location: lume_ast::Location(0..0),
                            },
                            location: lume_ast::Location(0..0),
                        },
                    ],
                    location: lume_ast::Location(0..0),
                }),
            )))],
            location: lume_ast::Location(0..0),
        });

        Ok(hir::Statement {
            id,
            location,
            kind: hir::StatementKind::InfiniteLoop(hir::InfiniteLoop { id, block, location }),
        })
    }
}
