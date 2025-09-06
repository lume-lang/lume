use crate::FunctionTransformer;

impl FunctionTransformer<'_, '_> {
    pub(super) fn block(&mut self, block: &lume_tir::Block) -> Option<lume_mir::Operand> {
        let mut val = None;

        for stmt in &block.statements {
            val = self.statement(stmt);
        }

        val
    }

    pub(super) fn statement(&mut self, stmt: &lume_tir::Statement) -> Option<lume_mir::Operand> {
        match stmt {
            lume_tir::Statement::Variable(decl) => self.declare_variable(decl),
            lume_tir::Statement::Break(stmt) => self.break_loop(stmt),
            lume_tir::Statement::Continue(stmt) => self.continue_loop(stmt),
            lume_tir::Statement::Final(fin) => Some(self.final_value(fin)),
            lume_tir::Statement::Return(ret) => self.return_value(ret),
            lume_tir::Statement::InfiniteLoop(stmt) => self.infinite_loop(stmt),
            lume_tir::Statement::IteratorLoop(_) => todo!("mir: iterator loop"),
            lume_tir::Statement::Expression(expr) => Some(self.expression(expr)),
        }
    }

    fn declare_variable(&mut self, stmt: &lume_tir::VariableDeclaration) -> Option<lume_mir::Operand> {
        let value = self.expression(&stmt.value);
        let is_ref_ty = self.tcx().tdb().is_reference_type(stmt.value.ty.instance_of).unwrap();

        let register = if is_ref_ty {
            match &value.kind {
                lume_mir::OperandKind::Reference { id } | lume_mir::OperandKind::Load { id } => *id,
                lume_mir::OperandKind::LoadField { target, .. } => *target,
                _ => self.declare_value(value),
            }
        } else {
            self.declare_value(value)
        };

        self.variables.insert(stmt.var, register);

        None
    }

    fn break_loop(&mut self, stmt: &lume_tir::Break) -> Option<lume_mir::Operand> {
        let (_, end) = self.func.expect_loop_target();

        self.func.current_block_mut().branch(end, stmt.location);

        None
    }

    fn continue_loop(&mut self, stmt: &lume_tir::Continue) -> Option<lume_mir::Operand> {
        let (body, _) = self.func.expect_loop_target();

        self.func.current_block_mut().branch(body, stmt.location);

        None
    }

    fn final_value(&mut self, stmt: &lume_tir::Final) -> lume_mir::Operand {
        self.expression(&stmt.value)
    }

    fn return_value(&mut self, stmt: &lume_tir::Return) -> Option<lume_mir::Operand> {
        let value = stmt.value.clone().map(|val| self.expression(&val));

        self.func.current_block_mut().return_any(value, stmt.location);

        None
    }

    fn infinite_loop(&mut self, stmt: &lume_tir::InfiniteLoop) -> Option<lume_mir::Operand> {
        let body_block = self.func.new_block();

        let merge_block = if stmt.is_returning() {
            None
        } else {
            Some(self.func.new_block())
        };

        self.func.current_block_mut().branch(body_block, stmt.location);

        if let Some(merge_block) = merge_block {
            self.func.enter_loop_scope(body_block, merge_block);
        }

        self.func.set_current_block(body_block);
        self.block(&stmt.block);

        if merge_block.is_some() {
            self.func.exit_scope();
        }

        // Loop back to the start of the loop body
        self.func.current_block_mut().branch(body_block, stmt.location);

        if let Some(merge_block) = merge_block {
            self.func.current_block_mut().branch(merge_block, stmt.location);
            self.func.set_current_block(merge_block);
        }

        None
    }
}
