use error_snippet::Result;

use crate::LowerModule;

impl LowerModule {
    /// Lowers the given AST block into a HIR block, within an nested scope.
    #[libftrace::traced(level = Debug)]
    pub(super) fn block(&mut self, expr: lume_ast::Block) -> lume_hir::Block {
        self.locals.push_frame();

        let id = self.next_node_id();
        let statements = self.statements(expr.statements);
        let location = self.location(expr.location);

        self.locals.pop_frame();

        lume_hir::Block {
            id,
            statements,
            location,
        }
    }

    /// Lowers the given AST block into a HIR block, within an isolated scope.
    ///
    /// This functions much like [`block`], but pushes a boundary into the
    /// symbol table, separating local variables within the block from the
    /// parent scope.
    #[libftrace::traced(level = Debug)]
    pub(super) fn isolated_block(&mut self, expr: lume_ast::Block, params: &[lume_hir::Parameter]) -> lume_hir::Block {
        self.locals.push_boundary();

        for param in params {
            self.locals.define(
                param.name.to_string(),
                lume_hir::VariableSource::Parameter(param.clone()),
            );
        }

        let id = self.next_node_id();
        let statements = self.statements(expr.statements);
        let location = self.location(expr.location);

        self.locals.pop_boundary();

        lume_hir::Block {
            id,
            statements,
            location,
        }
    }

    #[libftrace::traced(level = Debug)]
    pub(super) fn statements(&mut self, statements: Vec<lume_ast::Statement>) -> Vec<lume_span::NodeId> {
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

    #[libftrace::traced(level = Debug)]
    fn statement(&mut self, expr: lume_ast::Statement) -> Result<lume_span::NodeId> {
        let stmt = match expr {
            lume_ast::Statement::VariableDeclaration(s) => self.stmt_variable(*s)?,
            lume_ast::Statement::Break(s) => self.stmt_break(*s),
            lume_ast::Statement::Continue(s) => self.stmt_continue(*s),
            lume_ast::Statement::Final(s) => self.stmt_final(*s)?,
            lume_ast::Statement::Return(s) => self.stmt_return(*s)?,
            lume_ast::Statement::InfiniteLoop(e) => self.stmt_infinite_loop(*e),
            lume_ast::Statement::IteratorLoop(e) => self.stmt_iterator_loop(*e)?,
            lume_ast::Statement::PredicateLoop(e) => self.stmt_predicate_loop(*e),
            lume_ast::Statement::Expression(s) => {
                let id = self.next_node_id();
                let expr = self.expression(*s)?;

                let location = self.map.expression(expr).unwrap().location;

                lume_hir::Statement {
                    id,
                    location,
                    kind: lume_hir::StatementKind::Expression(expr),
                }
            }
        };

        let id = stmt.id;

        self.map.nodes.insert(id, lume_hir::Node::Statement(stmt));

        Ok(id)
    }

    #[libftrace::traced(level = Debug)]
    fn stmt_variable(&mut self, statement: lume_ast::VariableDeclaration) -> Result<lume_hir::Statement> {
        let id = self.next_node_id();
        let name = self.identifier(statement.name);
        let value = self.expression(statement.value)?;
        let location = self.location(statement.location);

        let declared_type = match statement.variable_type {
            Some(t) => Some(self.type_ref(t)?),
            None => None,
        };

        let decl = lume_hir::VariableDeclaration {
            id,
            name: name.clone(),
            declared_type,
            value,
            location,
        };

        self.locals
            .define(name.to_string(), lume_hir::VariableSource::Variable(decl.clone()));

        let statement = lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Variable(decl),
        };

        Ok(statement)
    }

    #[libftrace::traced(level = Debug)]
    fn stmt_break(&mut self, statement: lume_ast::Break) -> lume_hir::Statement {
        let id = self.next_node_id();
        let location = self.location(statement.location);

        lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Break(lume_hir::Break { id, location }),
        }
    }

    #[libftrace::traced(level = Debug)]
    fn stmt_continue(&mut self, statement: lume_ast::Continue) -> lume_hir::Statement {
        let id = self.next_node_id();
        let location = self.location(statement.location);

        lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Continue(lume_hir::Continue { id, location }),
        }
    }

    #[libftrace::traced(level = Debug)]
    fn stmt_final(&mut self, statement: lume_ast::Final) -> Result<lume_hir::Statement> {
        let id = self.next_node_id();
        let value = self.expression(statement.value)?;
        let location = self.map.expression(value).unwrap().location;

        Ok(lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Final(lume_hir::Final { id, value, location }),
        })
    }

    #[libftrace::traced(level = Debug)]
    fn stmt_return(&mut self, statement: lume_ast::Return) -> Result<lume_hir::Statement> {
        let id = self.next_node_id();
        let value = self.opt_expression(statement.value)?;
        let location = self.location(statement.location);

        Ok(lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Return(lume_hir::Return { id, value, location }),
        })
    }

    #[libftrace::traced(level = Debug)]
    fn stmt_infinite_loop(&mut self, expr: lume_ast::InfiniteLoop) -> lume_hir::Statement {
        let id = self.next_node_id();
        let location = self.location(expr.location);
        let block = self.block(expr.block);

        lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::InfiniteLoop(lume_hir::InfiniteLoop { id, block, location }),
        }
    }

    #[libftrace::traced(level = Debug)]
    fn stmt_iterator_loop(&mut self, expr: lume_ast::IteratorLoop) -> Result<lume_hir::Statement> {
        let id = self.next_node_id();
        let location = self.location(expr.location);
        let collection = self.expression(expr.collection)?;
        let block = self.block(expr.block);

        Ok(lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::IteratorLoop(lume_hir::IteratorLoop {
                id,
                collection,
                block,
                location,
            }),
        })
    }

    #[libftrace::traced(level = Debug)]
    fn stmt_predicate_loop(&mut self, expr: lume_ast::PredicateLoop) -> lume_hir::Statement {
        let id = self.next_node_id();
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

        lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::InfiniteLoop(lume_hir::InfiniteLoop { id, block, location }),
        }
    }
}
