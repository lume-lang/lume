use lume_mir::FunctionId;

use crate::FunctionTransformer;

impl FunctionTransformer<'_> {
    pub(super) fn expression(&mut self, expr: &lume_tir::Expression) -> lume_mir::Operand {
        match &expr.kind {
            lume_tir::ExpressionKind::Assignment(expr) => self.assignment(expr),
            lume_tir::ExpressionKind::Binary(expr) => self.binary(expr),
            lume_tir::ExpressionKind::Cast(_) => todo!("cast MIR lowering"),
            lume_tir::ExpressionKind::Construct(expr) => self.construct(expr),
            lume_tir::ExpressionKind::Call(expr) => self.call_expression(expr),
            lume_tir::ExpressionKind::IntrinsicCall(call) => self.intrinsic_call(call),
            lume_tir::ExpressionKind::Literal(lit) => self.literal(&lit.kind),
            lume_tir::ExpressionKind::Logical(expr) => self.logical(expr),
            lume_tir::ExpressionKind::Member(expr) => self.member(expr),
            lume_tir::ExpressionKind::Variable(var) => self.variable_reference(var),
            lume_tir::ExpressionKind::Variant(_) => todo!("enum variant MIR lowering"),
        }
    }

    fn assignment(&mut self, expr: &lume_tir::Assignment) -> lume_mir::Operand {
        let lume_mir::Operand::Reference { id } = self.expression(&expr.target) else {
            todo!()
        };

        let value = self.expression(&expr.value);
        self.func.current_block_mut().store(id, value);

        lume_mir::Operand::Load { id }
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

    fn construct(&mut self, expr: &lume_tir::Construct) -> lume_mir::Operand {
        let props = self
            .tcx()
            .tdb()
            .find_properties(expr.ty.instance_of)
            .collect::<Vec<_>>();

        let prop_types = props
            .iter()
            .map(|prop| self.lower_type(&prop.property_type))
            .collect::<Vec<_>>();

        let struct_type = lume_mir::Type::structure(expr.ty.clone(), prop_types);

        let reg = self.func.add_register(struct_type.clone());
        self.func.current_block_mut().allocate_heap(reg, struct_type);

        for (idx, field) in expr.fields.iter().enumerate() {
            let value = self.expression(&field.value);

            self.func.current_block_mut().store_field(reg, idx, value);
        }

        lume_mir::Operand::Reference { id: reg }
    }

    fn call_expression(&mut self, expr: &lume_tir::Call) -> lume_mir::Operand {
        let func_id = FunctionId(expr.function.0);

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

        lume_mir::Operand::Load { id: reg }
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

        let expr_ty = &expr.lhs.ty;

        let name = if expr_ty.is_bool() {
            match expr.op {
                lume_tir::LogicalOperator::And => lume_mir::Intrinsic::BooleanAnd,
                lume_tir::LogicalOperator::Or => lume_mir::Intrinsic::BooleanOr,
            }
        } else {
            unreachable!()
        };

        let decl = lume_mir::Declaration::Intrinsic { name, args };
        let reg = self.declare(decl);

        lume_mir::Operand::Load { id: reg }
    }

    fn member(&mut self, expr: &lume_tir::Member) -> lume_mir::Operand {
        let target_val = self.expression(&expr.callee);
        let target_reg = self.load_operand(&target_val);

        let property_idx = expr.property.index;

        lume_mir::Operand::LoadField {
            target: target_reg,
            field: property_idx,
        }
    }

    fn variable_reference(&mut self, expr: &lume_tir::VariableReference) -> lume_mir::Operand {
        let id = match &expr.source {
            lume_tir::VariableSource::Parameter => lume_mir::RegisterId::param(expr.reference.0),
            lume_tir::VariableSource::Variable => *self.variables.get(&expr.reference).unwrap(),
        };

        lume_mir::Operand::Load { id }
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
}
