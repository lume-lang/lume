use error_snippet::Result;
use lume_span::Internable;

use crate::LowerFunction;

impl LowerFunction<'_> {
    pub(crate) fn statement(&mut self, stmt: lume_span::StatementId) -> Result<lume_tir::Statement> {
        let stmt = self.lower.tcx.hir_expect_stmt(stmt);

        match &stmt.kind {
            lume_hir::StatementKind::Variable(stmt) => self.variable_statement(stmt),
            lume_hir::StatementKind::Break(stmt) => Ok(self.break_statement(stmt)),
            lume_hir::StatementKind::Continue(stmt) => Ok(self.continue_statement(stmt)),
            lume_hir::StatementKind::Final(stmt) => self.final_statement(stmt),
            lume_hir::StatementKind::Return(stmt) => self.return_statement(stmt),
            lume_hir::StatementKind::InfiniteLoop(stmt) => self.infinite_statement(stmt),
            lume_hir::StatementKind::IteratorLoop(stmt) => self.iterator_statement(stmt),
            lume_hir::StatementKind::Expression(expr) => Ok(lume_tir::Statement::Expression(self.expression(*expr)?)),
        }
    }

    fn variable_statement(&mut self, stmt: &lume_hir::VariableDeclaration) -> Result<lume_tir::Statement> {
        let var = self.mark_variable(lume_tir::VariableSource::Variable);
        let value = self.expression(stmt.value)?;

        self.variable_mapping.insert(lume_span::DefId::Statement(stmt.id), var);

        Ok(lume_tir::Statement::Variable(lume_tir::VariableDeclaration {
            id: stmt.id,
            var,
            name: stmt.name.to_string().intern(),
            value,
            location: stmt.location,
        }))
    }

    fn break_statement(&mut self, stmt: &lume_hir::Break) -> lume_tir::Statement {
        let target = self.lower.tcx.hir_loop_target_stmt(stmt.id).unwrap();

        lume_tir::Statement::Break(lume_tir::Break {
            id: stmt.id,
            target: target.id,
            location: stmt.location,
        })
    }

    fn continue_statement(&mut self, stmt: &lume_hir::Continue) -> lume_tir::Statement {
        let target = self.lower.tcx.hir_loop_target_stmt(stmt.id).unwrap();

        lume_tir::Statement::Continue(lume_tir::Continue {
            id: stmt.id,
            target: target.id,
            location: stmt.location,
        })
    }

    fn final_statement(&mut self, stmt: &lume_hir::Final) -> Result<lume_tir::Statement> {
        let value = self.expression(stmt.value)?;

        Ok(lume_tir::Statement::Final(lume_tir::Final {
            id: stmt.id,
            value,
            location: stmt.location,
        }))
    }

    fn return_statement(&mut self, stmt: &lume_hir::Return) -> Result<lume_tir::Statement> {
        let value = if let Some(val) = stmt.value {
            Some(self.expression(val)?)
        } else {
            None
        };

        Ok(lume_tir::Statement::Return(lume_tir::Return {
            id: stmt.id,
            value,
            location: stmt.location,
        }))
    }

    fn infinite_statement(&mut self, stmt: &lume_hir::InfiniteLoop) -> Result<lume_tir::Statement> {
        let block = self.lower_block(&stmt.block)?;

        Ok(lume_tir::Statement::InfiniteLoop(lume_tir::InfiniteLoop {
            id: stmt.id,
            block,
            location: stmt.location,
        }))
    }

    fn iterator_statement(&mut self, stmt: &lume_hir::IteratorLoop) -> Result<lume_tir::Statement> {
        let collection = self.expression(stmt.collection)?;
        let block = self.lower_block(&stmt.block)?;

        Ok(lume_tir::Statement::IteratorLoop(lume_tir::IteratorLoop {
            id: stmt.id,
            collection,
            block,
            location: stmt.location,
        }))
    }
}
