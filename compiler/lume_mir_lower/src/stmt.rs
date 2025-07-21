use crate::FunctionTransformer;

impl FunctionTransformer<'_> {
    pub(super) fn block(&mut self, block: &lume_tir::Block) {
        for stmt in &block.statements {
            self.statement(stmt);
        }
    }

    pub(super) fn statement(&mut self, stmt: &lume_tir::Statement) {
        match stmt {
            lume_tir::Statement::Variable(decl) => self.declare_variable(decl),
            lume_tir::Statement::Break(_) => self.break_loop(),
            lume_tir::Statement::Continue(_) => self.continue_loop(),
            lume_tir::Statement::Return(ret) => self.return_value(ret),
            lume_tir::Statement::If(cond) => self.if_condition(cond),
            lume_tir::Statement::InfiniteLoop(stmt) => self.infinite_loop(stmt),
            lume_tir::Statement::IteratorLoop(_) => todo!("mir: iterator loop"),
            lume_tir::Statement::PredicateLoop(_) => todo!("mir: predicate loop"),
            lume_tir::Statement::Expression(expr) => {
                self.expression(expr);
            }
        }
    }

    fn declare_variable(&mut self, stmt: &lume_tir::VariableDeclaration) {
        let value = self.expression(&stmt.value);
        let is_ref_ty = self.tcx().tdb().is_reference_type(stmt.value.ty.instance_of).unwrap();

        let register = if is_ref_ty {
            match value {
                lume_mir::Operand::Reference { id } => id,
                lume_mir::Operand::String { .. } => todo!("define global string constants"),
                _ => self.declare_value(value),
            }
        } else {
            self.declare_value(value)
        };

        self.variables.insert(stmt.var, register);
    }

    fn break_loop(&mut self) {
        let (_, end) = self.func.expect_loop_target();

        self.func.current_block_mut().branch(end);
        self.add_edge(self.func.current_block().id, end);
    }

    fn continue_loop(&mut self) {
        let (body, _) = self.func.expect_loop_target();

        self.func.current_block_mut().branch(body);
        self.add_edge(self.func.current_block().id, body);
    }

    fn return_value(&mut self, stmt: &lume_tir::Return) {
        let value = stmt.value.clone().map(|val| self.expression(&val));

        self.func.current_block_mut().return_any(value);
    }

    fn if_condition(&mut self, stmt: &lume_tir::If) {
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

            self.add_edge(self.func.current_block().id, cond_then);
            self.add_edge(self.func.current_block().id, cond_else);

            self.build_conditional_graph(condition, cond_then, cond_else);

            self.func.set_current_block(cond_then);
            self.block(&case.block);

            if let Some(merge_block) = merge_block {
                if !self.func.current_block().has_terminator() {
                    self.func.current_block_mut().branch(merge_block);
                }

                self.add_edge(cond_then, merge_block);
                self.add_edge(cond_else, merge_block);
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
        expr: &lume_tir::Expression,
        then_block: lume_mir::BasicBlockId,
        else_block: lume_mir::BasicBlockId,
    ) {
        if let lume_tir::ExpressionKind::Logical(comp_expr) = &expr.kind {
            match comp_expr.op {
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
                lume_tir::LogicalOperator::And => {
                    let inter_block = self.func.new_block();

                    let lhs_val = self.expression(&comp_expr.lhs);
                    let lhs_expr = self.func.declare_value(lume_mir::Type::boolean(), lhs_val);

                    self.func
                        .current_block_mut()
                        .conditional_branch(lhs_expr, inter_block, else_block);

                    self.func.set_current_block(inter_block);

                    let rhs_val = self.expression(&comp_expr.rhs);
                    let rhs_expr = self.func.declare_value(lume_mir::Type::boolean(), rhs_val);

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
                lume_tir::LogicalOperator::Or => {
                    let inter_block = self.func.new_block();

                    let lhs_val = self.expression(&comp_expr.lhs);
                    let lhs_expr = self.func.declare_value(lume_mir::Type::boolean(), lhs_val);

                    self.func
                        .current_block_mut()
                        .conditional_branch(lhs_expr, then_block, inter_block);

                    self.func.set_current_block(inter_block);

                    let rhs_val = self.expression(&comp_expr.rhs);
                    let rhs_expr = self.func.declare_value(lume_mir::Type::boolean(), rhs_val);

                    self.func
                        .current_block_mut()
                        .conditional_branch(rhs_expr, then_block, else_block);
                }
            }
        } else {
            let cond_val = self.expression(expr);
            let cond_expr = self.func.declare_value(lume_mir::Type::boolean(), cond_val);

            self.func
                .current_block_mut()
                .conditional_branch(cond_expr, then_block, else_block);
        }
    }

    fn infinite_loop(&mut self, stmt: &lume_tir::InfiniteLoop) {
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
    }
}
