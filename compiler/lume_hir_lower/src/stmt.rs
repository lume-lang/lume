use crate::*;

impl LoweringContext<'_> {
    /// Lowers the given AST block into a HIR block, within an nested scope.
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn block(&mut self, expr: lume_ast::Block) -> lume_hir::Block {
        lume_hir::with_frame!(self.current_locals, || {
            let id = self.next_node_id();
            let statements = self.statements(expr.stmt());
            let location = self.location(expr.location());

            lume_hir::Block {
                id,
                statements,
                location,
            }
        })
    }

    /// Lowers the given AST block into a HIR block, within an nested scope.
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn block_opt(&mut self, block: Option<lume_ast::Block>) -> lume_hir::Block {
        match block {
            Some(block) => self.block(block),
            None => lume_hir::Block::missing(),
        }
    }

    /// Lowers the given AST block into a HIR block, within an isolated scope.
    ///
    /// This functions much like [`block`], but pushes a boundary into the
    /// symbol table, separating local variables within the block from the
    /// parent scope.
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn isolated_block(&mut self, expr: lume_ast::Block, params: &[lume_hir::Parameter]) -> lume_hir::Block {
        lume_hir::with_boundary!(self.current_locals, || {
            for param in params {
                self.current_locals
                    .define(param.name.to_string(), lume_hir::VariableSource::Parameter(param.id));
            }

            let id = self.next_node_id();
            let statements = self.statements(expr.stmt());
            let location = self.location(expr.location());

            lume_hir::Block {
                id,
                statements,
                location,
            }
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn statements<I>(&mut self, statements: I) -> Vec<lume_span::NodeId>
    where
        I: IntoIterator<Item = lume_ast::Stmt>,
    {
        statements
            .into_iter()
            .map(|expr| self.statement(expr))
            .collect::<Vec<_>>()
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn statement(&mut self, expr: lume_ast::Stmt) -> NodeId {
        match expr {
            lume_ast::Stmt::LetStmt(s) => self.stmt_variable(s),
            lume_ast::Stmt::BreakStmt(s) => self.stmt_break(s),
            lume_ast::Stmt::ContinueStmt(s) => self.stmt_continue(s),
            lume_ast::Stmt::FinalStmt(s) => self.stmt_final(s),
            lume_ast::Stmt::ReturnStmt(s) => self.stmt_return(s),
            lume_ast::Stmt::LoopStmt(e) => self.stmt_infinite_loop(e),
            lume_ast::Stmt::ForStmt(e) => self.stmt_iterator_loop(e),
            lume_ast::Stmt::WhileStmt(e) => self.stmt_predicate_loop(e),
            lume_ast::Stmt::ExprStmt(s) => self.stmt_expression(s),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_variable(&mut self, statement: lume_ast::LetStmt) -> NodeId {
        let name = self.ident_opt(statement.name());
        let declared_type = statement.ty().map(|t| self.hir_type(t));
        let value = self.opt_expression(statement.expr(), Place::RValue);
        let location = self.location(statement.location());

        self.alloc_variable_decl(name, value, declared_type, location)
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_break(&mut self, statement: lume_ast::BreakStmt) -> NodeId {
        let id = self.next_node_id();
        let location = self.location(statement.location());

        self.alloc_stmt(lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Break(lume_hir::Break { id, location }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_continue(&mut self, statement: lume_ast::ContinueStmt) -> NodeId {
        let id = self.next_node_id();
        let location = self.location(statement.location());

        self.alloc_stmt(lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Continue(lume_hir::Continue { id, location }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_final(&mut self, statement: lume_ast::FinalStmt) -> NodeId {
        let id = self.next_node_id();
        let value = self.opt_expression(statement.expr(), Place::RValue);
        let location = self.location(statement.location());

        self.alloc_stmt(lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Final(lume_hir::Final { id, value, location }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_return(&mut self, statement: lume_ast::ReturnStmt) -> NodeId {
        let id = self.next_node_id();
        let value = statement.expr().map(|expr| self.expression(expr, Place::default()));
        let location = self.location(statement.location());

        self.alloc_stmt(lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Return(lume_hir::Return { id, value, location }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_infinite_loop(&mut self, expr: lume_ast::LoopStmt) -> NodeId {
        let location = self.location(expr.location());
        let block = self.block_opt(expr.block());

        self.alloc_infinite_loop(block, location)
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_iterator_loop(&mut self, stmt: lume_ast::ForStmt) -> NodeId {
        self.desugar_for_loop(stmt)
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_predicate_loop(&mut self, expr: lume_ast::WhileStmt) -> NodeId {
        let condition = self.opt_expression(expr.condition(), Place::RValue);
        let block = self.block_opt(expr.block());
        let location = self.location(expr.location());

        self.desugar_while_loop(condition, block, location)
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_expression(&mut self, expr: lume_ast::ExprStmt) -> NodeId {
        let expr = self.opt_expression(expr.expr(), Place::RValue);

        self.alloc_expr_stmt(expr)
    }
}
