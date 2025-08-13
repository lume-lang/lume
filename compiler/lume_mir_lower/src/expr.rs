use lume_mir::FunctionId;

use crate::FunctionTransformer;

impl FunctionTransformer<'_> {
    pub(super) fn expression(&mut self, expr: &lume_tir::Expression) -> lume_mir::Operand {
        match &expr.kind {
            lume_tir::ExpressionKind::Assignment(expr) => self.assignment(expr),
            lume_tir::ExpressionKind::Binary(expr) => self.binary(expr),
            lume_tir::ExpressionKind::Cast(expr) => self.cast(expr),
            lume_tir::ExpressionKind::Construct(expr) => self.construct(expr),
            lume_tir::ExpressionKind::Call(expr) => self.call_expression(expr),
            lume_tir::ExpressionKind::If(cond) => self.if_condition(cond),
            lume_tir::ExpressionKind::IntrinsicCall(call) => self.intrinsic_call(call),
            lume_tir::ExpressionKind::Literal(lit) => self.literal(&lit.kind),
            lume_tir::ExpressionKind::Logical(expr) => self.logical(expr),
            lume_tir::ExpressionKind::Member(expr) => self.member(expr),
            lume_tir::ExpressionKind::Scope(expr) => self.scope(expr),
            lume_tir::ExpressionKind::Variable(var) => self.variable_reference(var),
            lume_tir::ExpressionKind::Variant(var) => self.variant(var),
        }
    }

    fn assignment(&mut self, expr: &lume_tir::Assignment) -> lume_mir::Operand {
        let value = self.expression(&expr.value);

        match self.expression(&expr.target) {
            lume_mir::Operand::Reference { id } => {
                self.func.current_block_mut().assign(id, value);

                lume_mir::Operand::Reference { id }
            }
            lume_mir::Operand::Load { id } => {
                self.func.current_block_mut().store(id, value);

                lume_mir::Operand::Load { id }
            }
            load @ lume_mir::Operand::LoadField { target, offset, .. } => {
                self.func.current_block_mut().store_field(target, offset, value);

                load
            }
            lume_mir::Operand::SlotAddress { .. } => panic!("bug!: attempted to assign slot-address"),
            lume_mir::Operand::Boolean { .. }
            | lume_mir::Operand::Integer { .. }
            | lume_mir::Operand::Float { .. }
            | lume_mir::Operand::String { .. } => panic!("bug!: attempted to assign literal"),
        }
    }

    fn binary(&mut self, expr: &lume_tir::Binary) -> lume_mir::Operand {
        let lhs = self.expression(&expr.lhs);
        let rhs = self.expression(&expr.rhs);
        let args = vec![lhs, rhs];

        let expr_ty = &expr.lhs.ty;

        let name = if expr_ty.is_integer() {
            let bits = expr_ty.bitwidth();
            let signed = expr_ty.signed();

            match expr.op {
                lume_tir::BinaryOperator::And => lume_mir::Intrinsic::IntAnd { bits, signed },
                lume_tir::BinaryOperator::Or => lume_mir::Intrinsic::IntOr { bits, signed },
                lume_tir::BinaryOperator::Xor => lume_mir::Intrinsic::IntXor { bits, signed },
            }
        } else {
            unreachable!()
        };

        let decl = lume_mir::Declaration::Intrinsic { name, args };
        let reg = self.declare(decl);

        lume_mir::Operand::Load { id: reg }
    }

    fn cast(&mut self, expr: &lume_tir::Cast) -> lume_mir::Operand {
        let expr_ty = &expr.source.ty;
        debug_assert!(expr_ty.is_integer());

        let source = self.expression(&expr.source);
        let operand = self.declare(lume_mir::Declaration::Operand(source));

        let decl = lume_mir::Declaration::Cast {
            operand,
            bits: expr.target.bitwidth(),
        };

        lume_mir::Operand::Reference { id: self.declare(decl) }
    }

    fn construct(&mut self, expr: &lume_tir::Construct) -> lume_mir::Operand {
        let name = self.tcx().new_named_type(&expr.ty, true).unwrap().to_string();
        let props = self.tcx().tdb().find_fields(expr.ty.instance_of).collect::<Vec<_>>();

        let prop_types = props
            .iter()
            .map(|prop| self.lower_type(&prop.field_type))
            .collect::<Vec<_>>();

        let prop_sizes = prop_types.iter().map(lume_mir::Type::bytesize).collect::<Vec<_>>();

        let struct_type = lume_mir::Type::structure(expr.ty.clone(), name, prop_types);
        let struct_ptr = lume_mir::Type::pointer(struct_type.clone());

        let reg = self.func.add_register(struct_ptr);
        self.func.current_block_mut().allocate(reg, struct_type);

        let mut offset = 0;

        for (field, size) in expr.fields.iter().zip(prop_sizes) {
            let value = self.expression(&field.value);

            self.func.current_block_mut().store_field(reg, offset, value);

            offset += size;
        }

        lume_mir::Operand::Reference { id: reg }
    }

    fn call_expression(&mut self, expr: &lume_tir::Call) -> lume_mir::Operand {
        let func_id = FunctionId(expr.function.as_usize());

        let args = expr
            .arguments
            .iter()
            .map(|arg| self.expression(arg))
            .collect::<Vec<_>>();

        self.call(func_id, args)
    }

    fn intrinsic_call(&mut self, expr: &lume_tir::IntrinsicCall) -> lume_mir::Operand {
        let name = self.intrinsic_of(&expr.kind);
        let args = expr.arguments.iter().map(|arg| self.expression(arg)).collect();

        let decl = lume_mir::Declaration::Intrinsic { name, args };
        let reg = self.declare(decl);

        lume_mir::Operand::Reference { id: reg }
    }

    fn if_condition(&mut self, expr: &lume_tir::If) -> lume_mir::Operand {
        let merge_block = if expr.is_returning() {
            None
        } else {
            Some(self.func.new_block())
        };

        let return_reg = if let Some(merge_block) = merge_block
            && let Some(return_type) = expr.return_type.as_ref()
        {
            let expr_ty = self.lower_type(return_type);
            let param_reg = self.func.add_block_parameter(merge_block, expr_ty);

            Some(param_reg)
        } else {
            None
        };

        for case in &expr.cases {
            // Ignore the `else` condition, if present.
            let Some(condition) = case.condition.as_ref() else {
                continue;
            };

            let cond_then = self.func.new_block();
            let cond_else = self.func.new_block();

            self.add_edge(self.func.current_block().id, cond_then);
            self.add_edge(self.func.current_block().id, cond_else);

            self.build_conditional_graph(condition, cond_then, cond_else);

            self.func.set_current_block(cond_then);
            let value = self.block(&case.block);

            if let Some(merge_block) = merge_block {
                let args = if let Some(cond_result) = value
                    && let Some(return_reg) = return_reg
                {
                    let loaded_result = self.load_operand(&cond_result);

                    self.func
                        .current_block_mut()
                        .push_phi_register(loaded_result, return_reg);

                    &[loaded_result][..]
                } else {
                    &[]
                };

                self.func.current_block_mut().branch_with(merge_block, args);

                self.add_edge(cond_then, merge_block);
                self.add_edge(cond_else, merge_block);
            }

            self.func.set_current_block(cond_else);
        }

        if let Some(else_block) = &expr.else_branch() {
            let value = self.block(&else_block.block);

            if let Some(merge_block) = merge_block {
                let args = if let Some(cond_result) = value
                    && let Some(return_reg) = return_reg
                {
                    let loaded_result = self.load_operand(&cond_result);

                    self.func
                        .current_block_mut()
                        .push_phi_register(loaded_result, return_reg);

                    &[loaded_result][..]
                } else {
                    &[]
                };

                self.func.current_block_mut().branch_with(merge_block, args);
            }
        }

        if let Some(merge_block) = merge_block {
            self.func.set_current_block(merge_block);
        }

        if let Some(return_reg) = return_reg {
            lume_mir::Operand::Reference { id: return_reg }
        } else {
            self.null_operand()
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

                    self.add_edge(self.func.current_block().id, inter_block);
                    self.add_edge(inter_block, else_block);

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

                    self.add_edge(self.func.current_block().id, inter_block);
                    self.add_edge(inter_block, then_block);

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

    fn intrinsic_of(&self, expr: &lume_tir::IntrinsicKind) -> lume_mir::Intrinsic {
        match expr {
            lume_tir::IntrinsicKind::FloatEq { bits } => lume_mir::Intrinsic::FloatEq { bits: *bits },
            lume_tir::IntrinsicKind::FloatNe { bits } => lume_mir::Intrinsic::FloatNe { bits: *bits },
            lume_tir::IntrinsicKind::FloatGe { bits } => lume_mir::Intrinsic::FloatGe { bits: *bits },
            lume_tir::IntrinsicKind::FloatGt { bits } => lume_mir::Intrinsic::FloatGt { bits: *bits },
            lume_tir::IntrinsicKind::FloatLe { bits } => lume_mir::Intrinsic::FloatLe { bits: *bits },
            lume_tir::IntrinsicKind::FloatLt { bits } => lume_mir::Intrinsic::FloatLt { bits: *bits },
            lume_tir::IntrinsicKind::FloatAdd { bits } => lume_mir::Intrinsic::FloatAdd { bits: *bits },
            lume_tir::IntrinsicKind::FloatSub { bits } => lume_mir::Intrinsic::FloatSub { bits: *bits },
            lume_tir::IntrinsicKind::FloatMul { bits } => lume_mir::Intrinsic::FloatMul { bits: *bits },
            lume_tir::IntrinsicKind::FloatDiv { bits } => lume_mir::Intrinsic::FloatDiv { bits: *bits },
            lume_tir::IntrinsicKind::IntEq { bits, signed } => lume_mir::Intrinsic::IntEq {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntNe { bits, signed } => lume_mir::Intrinsic::IntNe {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntGe { bits, signed } => lume_mir::Intrinsic::IntGe {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntGt { bits, signed } => lume_mir::Intrinsic::IntGt {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntLe { bits, signed } => lume_mir::Intrinsic::IntLe {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntLt { bits, signed } => lume_mir::Intrinsic::IntLt {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntAdd { bits, signed } => lume_mir::Intrinsic::IntAdd {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntSub { bits, signed } => lume_mir::Intrinsic::IntSub {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntMul { bits, signed } => lume_mir::Intrinsic::IntMul {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntDiv { bits, signed } => lume_mir::Intrinsic::IntDiv {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntAnd { bits, signed } => lume_mir::Intrinsic::IntAnd {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntOr { bits, signed } => lume_mir::Intrinsic::IntOr {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::IntXor { bits, signed } => lume_mir::Intrinsic::IntXor {
                bits: *bits,
                signed: *signed,
            },
            lume_tir::IntrinsicKind::BooleanEq => lume_mir::Intrinsic::BooleanEq,
            lume_tir::IntrinsicKind::BooleanNe => lume_mir::Intrinsic::BooleanNe,
            lume_tir::IntrinsicKind::BooleanAnd => lume_mir::Intrinsic::BooleanAnd,
            lume_tir::IntrinsicKind::BooleanOr => lume_mir::Intrinsic::BooleanOr,
            lume_tir::IntrinsicKind::Metadata { id } => {
                let metadata_store = &self.transformer.mir.metadata.metadata;
                let metadata_entry = metadata_store.get(id).unwrap();

                lume_mir::Intrinsic::Metadata {
                    metadata: metadata_entry.clone(),
                }
            }
        }
    }

    fn logical(&mut self, expr: &lume_tir::Logical) -> lume_mir::Operand {
        let lhs = self.expression(&expr.lhs);
        let rhs = self.expression(&expr.rhs);
        let args = vec![lhs, rhs];

        let name = match expr.op {
            lume_tir::LogicalOperator::And => lume_mir::Intrinsic::BooleanAnd,
            lume_tir::LogicalOperator::Or => lume_mir::Intrinsic::BooleanOr,
        };

        let decl = lume_mir::Declaration::Intrinsic { name, args };

        lume_mir::Operand::Reference { id: self.declare(decl) }
    }

    fn member(&mut self, expr: &lume_tir::Member) -> lume_mir::Operand {
        let target_val = self.expression(&expr.callee);
        let target_reg = self.load_operand(&target_val);

        let index = expr.field.index;
        let offset = self.field_offset(&expr.field);

        lume_mir::Operand::LoadField {
            target: target_reg,
            offset,
            index,
        }
    }

    fn scope(&mut self, expr: &lume_tir::Scope) -> lume_mir::Operand {
        let mut val: Option<lume_mir::Operand> = None;

        for stmt in &expr.body {
            val = self.statement(stmt);
        }

        val.unwrap_or_else(|| self.null_operand())
    }

    fn variable_reference(&mut self, expr: &lume_tir::VariableReference) -> lume_mir::Operand {
        let id = match &expr.source {
            lume_tir::VariableSource::Parameter => lume_mir::RegisterId::new(expr.reference.0),
            lume_tir::VariableSource::Variable => *self.variables.get(&expr.reference).unwrap(),
        };

        lume_mir::Operand::Reference { id }
    }

    fn variant(&mut self, expr: &lume_tir::Variant) -> lume_mir::Operand {
        let discriminant = lume_mir::Operand::Integer {
            bits: 8,
            signed: false,
            value: i64::from(expr.index),
        };

        let enum_ty = self.tcx().type_of(expr.id).unwrap();
        let enum_def = self.tcx().enum_def_type(enum_ty.instance_of).unwrap();
        let def = lume_span::DefId::Item(enum_def.id);

        let mut union_cases = Vec::new();

        for variant in self.tcx().enum_cases_expr(expr.id).unwrap() {
            let params = &variant.parameters;
            let case_refs = self.tcx().mk_type_refs_from(params, def).unwrap();

            union_cases.reserve_exact(case_refs.len());
            for case_ref in case_refs {
                union_cases.push(self.lower_type(&case_ref));
            }
        }

        let union_type = lume_mir::Type::union(expr.ty.clone(), union_cases);
        let reg = self.func.add_register(union_type.clone());
        self.func.current_block_mut().allocate(reg, union_type);

        let mut offset = 0;

        // Store the discriminant of the variant first
        self.func.current_block_mut().store_field(reg, offset, discriminant);
        offset += 1;

        for argument in &expr.arguments {
            let value = self.expression(argument);
            let operand_size = usize::from(value.bitsize() / 8);

            self.func.current_block_mut().store_field(reg, offset, value);

            offset += operand_size;
        }

        lume_mir::Operand::Reference { id: reg }
    }

    #[expect(clippy::unused_self)]
    fn literal(&self, expr: &lume_tir::LiteralKind) -> lume_mir::Operand {
        match expr {
            lume_tir::LiteralKind::Boolean(val) => lume_mir::Operand::Boolean { value: *val },
            lume_tir::LiteralKind::Int(val) => {
                let (bits, signed, value) = match val {
                    lume_tir::IntLiteral::I8(val) => (8, true, *val),
                    lume_tir::IntLiteral::I16(val) => (16, true, *val),
                    lume_tir::IntLiteral::I32(val) => (32, true, *val),
                    lume_tir::IntLiteral::I64(val) => (64, true, *val),
                    lume_tir::IntLiteral::U8(val) => (8, false, *val),
                    lume_tir::IntLiteral::U16(val) => (16, false, *val),
                    lume_tir::IntLiteral::U32(val) => (32, false, *val),
                    lume_tir::IntLiteral::U64(val) => (64, false, *val),
                };

                lume_mir::Operand::Integer { value, bits, signed }
            }
            lume_tir::LiteralKind::Float(val) => {
                let (bits, value) = match val {
                    lume_tir::FloatLiteral::F32(val) => (32, *val),
                    lume_tir::FloatLiteral::F64(val) => (64, *val),
                };

                lume_mir::Operand::Float { value, bits }
            }
            lume_tir::LiteralKind::String(val) => lume_mir::Operand::String { value: *val },
        }
    }

    fn load_operand(&mut self, op: &lume_mir::Operand) -> lume_mir::RegisterId {
        if let lume_mir::Operand::Reference { id } = &op {
            *id
        } else {
            self.declare_value(op.clone())
        }
    }

    fn field_offset(&self, field: &lume_types::Field) -> usize {
        let mut offset = 0;

        for (idx, prop) in self.tcx().tdb().find_fields(field.owner).enumerate() {
            if idx == field.index {
                break;
            }

            let prop_ty = self.lower_type(&prop.field_type);
            offset += prop_ty.bytesize();
        }

        offset
    }
}
