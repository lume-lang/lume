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
            lume_tir::ExpressionKind::IntrinsicCall(call) => self.intrinsic_call(call),
            lume_tir::ExpressionKind::Literal(lit) => self.literal(&lit.kind),
            lume_tir::ExpressionKind::Logical(expr) => self.logical(expr),
            lume_tir::ExpressionKind::Member(expr) => self.member(expr),
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
        let props = self
            .tcx()
            .tdb()
            .find_properties(expr.ty.instance_of)
            .collect::<Vec<_>>();

        let prop_types = props
            .iter()
            .map(|prop| self.lower_type(&prop.property_type))
            .collect::<Vec<_>>();

        let prop_sizes = prop_types.iter().map(lume_mir::Type::bytesize).collect::<Vec<_>>();

        let struct_type = lume_mir::Type::structure(expr.ty.clone(), prop_types);
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

        let index = expr.property.index;
        let offset = self.property_offset(&expr.property);

        lume_mir::Operand::LoadField {
            target: target_reg,
            offset,
            index,
        }
    }

    fn variable_reference(&mut self, expr: &lume_tir::VariableReference) -> lume_mir::Operand {
        let id = match &expr.source {
            lume_tir::VariableSource::Parameter => lume_mir::RegisterId::new(expr.reference.0),
            lume_tir::VariableSource::Variable => *self.variables.get(&expr.reference).unwrap(),
        };

        lume_mir::Operand::Reference { id }
    }

    fn variant(&mut self, expr: &lume_tir::Variant) -> lume_mir::Operand {
        let variant = self.tcx().enum_case_expr(expr.id).unwrap();

        let discriminant = lume_mir::Operand::Integer {
            bits: 8,
            signed: false,
            value: i64::from(expr.index),
        };

        // If the variant only consists of a discriminant, we skip
        // the allocation and only pass an 8-bit integer around.
        if variant.parameters.is_empty() {
            return discriminant;
        }

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

    fn property_offset(&self, property: &lume_types::Property) -> usize {
        let mut offset = 0;

        for (idx, prop) in self.tcx().tdb().find_properties(property.owner).enumerate() {
            if idx == property.index {
                break;
            }

            let prop_ty = self.lower_type(&prop.property_type);
            offset += prop_ty.bytesize();
        }

        offset
    }
}
