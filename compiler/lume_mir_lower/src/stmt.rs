use crate::FunctionTransformer;

impl FunctionTransformer<'_> {
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
            lume_tir::Statement::Break(_) => self.break_loop(),
            lume_tir::Statement::Continue(_) => self.continue_loop(),
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
            match value {
                lume_mir::Operand::Reference { id } | lume_mir::Operand::Load { id } => id,
                lume_mir::Operand::LoadField { target, .. } => target,
                _ => self.declare_value(value),
            }
        } else {
            self.declare_value(value)
        };

        self.variables.insert(stmt.var, register);

        None
    }

    fn break_loop(&mut self) -> Option<lume_mir::Operand> {
        let (_, end) = self.func.expect_loop_target();

        self.func.current_block_mut().branch(end);
        self.add_edge(self.func.current_block().id, end);

        None
    }

    fn continue_loop(&mut self) -> Option<lume_mir::Operand> {
        let (body, _) = self.func.expect_loop_target();

        self.func.current_block_mut().branch(body);
        self.add_edge(self.func.current_block().id, body);

        None
    }

    fn final_value(&mut self, stmt: &lume_tir::Final) -> lume_mir::Operand {
        self.expression(&stmt.value)
    }

    fn return_value(&mut self, stmt: &lume_tir::Return) -> Option<lume_mir::Operand> {
        let value = stmt.value.clone().map(|val| self.expression(&val));

        self.func.current_block_mut().return_any(value);

        None
    }

    fn infinite_loop(&mut self, stmt: &lume_tir::InfiniteLoop) -> Option<lume_mir::Operand> {
        let body_block = self.func.new_block();

        let merge_block = if stmt.is_returning() {
            None
        } else {
            Some(self.func.new_block())
        };

        self.add_edge(self.func.current_block().id, body_block);
        self.func.current_block_mut().branch(body_block);

        if let Some(merge_block) = merge_block {
            self.add_edge(body_block, body_block);
            self.func.enter_loop_scope(body_block, merge_block);
        }

        self.func.set_current_block(body_block);
        self.block(&stmt.block);

        if merge_block.is_some() {
            self.func.exit_scope();
        }

        // Loop back to the start of the loop body
        self.add_edge(self.func.current_block().id, body_block);
        self.func.current_block_mut().branch(body_block);

        if let Some(merge_block) = merge_block {
            self.add_edge(self.func.current_block().id, merge_block);

            self.func.current_block_mut().branch(merge_block);
            self.func.set_current_block(merge_block);
        }

        None
    }
}
