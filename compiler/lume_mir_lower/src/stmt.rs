use crate::FunctionTransformer;

impl FunctionTransformer<'_> {
    pub(super) fn block(&mut self, block: &lume_hir::Block) {
        for stmt in &block.statements {
            self.statement(stmt);
        }
    }

    pub(super) fn statement(&mut self, stmt: &lume_hir::Statement) {
        match &stmt.kind {
            lume_hir::StatementKind::Variable(decl) => self.declare_variable(decl),
            lume_hir::StatementKind::Break(_) => self.break_loop(),
            lume_hir::StatementKind::Continue(_) => self.continue_loop(),
            lume_hir::StatementKind::Return(ret) => self.return_value(ret),
            lume_hir::StatementKind::If(cond) => self.if_condition(cond),
            lume_hir::StatementKind::InfiniteLoop(stmt) => self.infinite_loop(stmt),
            lume_hir::StatementKind::IteratorLoop(_) => todo!("mir: iterator loop"),
            lume_hir::StatementKind::PredicateLoop(_) => todo!("mir: predicate loop"),
            lume_hir::StatementKind::Expression(expr) => {
                self.expression(expr);
            }
        }
    }

    fn declare_variable(&mut self, stmt: &lume_hir::VariableDeclaration) {
        let value = self.expression(&stmt.value);

        let hir_type = self.tcx().type_of_expr(&stmt.value).unwrap();
        let is_ref_ty = self.tcx().tdb().is_reference_type(hir_type.instance_of).unwrap();

        let register = if is_ref_ty {
            match value {
                lume_mir::Operand::Reference { id } => id,
                lume_mir::Operand::String { .. } => todo!("define global string constants"),
                _ => self.declare_value(value),
            }
        } else {
            self.declare_value(value)
        };

        self.variables.insert(stmt.id, register);
    }

    fn break_loop(&mut self) {
        let (_, end) = self.func.expect_loop_target();

        self.func.current_block_mut().branch(end);
    }

    fn continue_loop(&mut self) {
        let (body, _) = self.func.expect_loop_target();

        self.func.current_block_mut().branch(body);
    }

    fn return_value(&mut self, stmt: &lume_hir::Return) {
        let value = stmt.value.clone().map(|val| self.expression(&val));

        self.func.current_block_mut().return_any(value);
    }

    fn if_condition(&mut self, stmt: &lume_hir::If) {
        let merge_block = if stmt.is_returning() {
            None
        } else {
            Some(self.func.new_block())
        };

        for case in &stmt.cases {
            let Some(condition) = case.condition.as_ref() else {
                continue;
            };

            let cond_then = self.func.new_block();
            let cond_else = self.func.new_block();

            self.build_conditional_graph(condition, cond_then, cond_else);

            self.func.set_current_block(cond_then);
            self.block(&case.block);

            if let Some(merge_block) = merge_block
                && !self.func.current_block().has_terminator()
            {
                self.func.current_block_mut().branch(merge_block);
            }

            self.func.set_current_block(cond_else);
        }

        if let Some(else_block) = &stmt.else_branch() {
            self.block(&else_block.block);
        }

        if let Some(merge_block) = merge_block
            && !self.func.current_block().has_terminator()
        {
            self.func.current_block_mut().branch(merge_block);
        }

        if let Some(merge_block) = merge_block {
            self.func.set_current_block(merge_block);
        }
    }

    fn build_conditional_graph(
        &mut self,
        expr: &lume_hir::Expression,
        then_block: lume_mir::BasicBlockId,
        else_block: lume_mir::BasicBlockId,
    ) {
        if let lume_hir::ExpressionKind::Logical(comp_expr) = &expr.kind {
            match comp_expr.op.kind {
                // Build graph for logical AND expressions
                //
                // For example, given an expression such as `x < 10 && x > 5`, the graph
                // would be visualized as:
                //
                //      BB0
                //     x < 10?
                //    /      \
                //  false    BB1
                //          x > 5?
                //         /     \
                //       false   true
                //
                // where `BB0` is the entry block and `BB1` is an intermediary block.
                lume_hir::LogicalOperatorKind::And => {
                    let inter_block = self.func.new_block();

                    let lhs_val = self.expression(&comp_expr.lhs);
                    let lhs_expr = self.func.declare_value(lume_mir::Type::Boolean, lhs_val);

                    self.func
                        .current_block_mut()
                        .conditional_branch(lhs_expr, inter_block, else_block);

                    self.func.set_current_block(inter_block);

                    let rhs_val = self.expression(&comp_expr.rhs);
                    let rhs_expr = self.func.declare_value(lume_mir::Type::Boolean, rhs_val);

                    self.func
                        .current_block_mut()
                        .conditional_branch(rhs_expr, then_block, else_block);
                }

                // Build graph for logical OR expressions
                //
                // For example, given an expression such as `x < 10 || y < 10`, the graph
                // would be visualized as:
                //
                //      BB0
                //     x < 10?
                //    /      \
                //  true     BB1
                //          y < 10?
                //         /      \
                //       true     false
                //
                // where `BB0` is the entry block and `BB1` is an intermediary block.
                lume_hir::LogicalOperatorKind::Or => {
                    let inter_block = self.func.new_block();

                    let lhs_val = self.expression(&comp_expr.lhs);
                    let lhs_expr = self.func.declare_value(lume_mir::Type::Boolean, lhs_val);

                    self.func
                        .current_block_mut()
                        .conditional_branch(lhs_expr, then_block, inter_block);

                    self.func.set_current_block(inter_block);

                    let rhs_val = self.expression(&comp_expr.rhs);
                    let rhs_expr = self.func.declare_value(lume_mir::Type::Boolean, rhs_val);

                    self.func
                        .current_block_mut()
                        .conditional_branch(rhs_expr, then_block, else_block);
                }
            }
        } else {
            let cond_val = self.expression(expr);
            let cond_expr = self.func.declare_value(lume_mir::Type::Boolean, cond_val);

            self.func
                .current_block_mut()
                .conditional_branch(cond_expr, then_block, else_block);
        }
    }

    fn infinite_loop(&mut self, stmt: &lume_hir::InfiniteLoop) {
        let body_block = self.func.new_block();

        let merge_block = if stmt.is_returning() {
            None
        } else {
            Some(self.func.new_block())
        };

        self.func.current_block_mut().branch(body_block);

        if let Some(merge_block) = merge_block {
            self.func.enter_loop_scope(body_block, merge_block);
        }

        self.func.set_current_block(body_block);
        self.block(&stmt.block);

        if merge_block.is_some() {
            self.func.exit_scope();
        }

        // Loop back to the start of the loop body
        self.func.current_block_mut().branch(body_block);

        if let Some(merge_block) = merge_block {
            self.func.current_block_mut().branch(merge_block);
            self.func.set_current_block(merge_block);
        }
    }
}
