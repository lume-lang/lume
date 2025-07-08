use crate::FunctionTransformer;

impl FunctionTransformer<'_> {
    pub(super) fn expression(&mut self, expr: &lume_hir::Expression) -> lume_mir::Operand {
        match &expr.kind {
            lume_hir::ExpressionKind::Assignment(expr) => self.assignment(expr),
            lume_hir::ExpressionKind::Binary(_) => todo!("binary MIR lowering"),
            lume_hir::ExpressionKind::Cast(_) => todo!("cast MIR lowering"),
            lume_hir::ExpressionKind::Construct(expr) => self.construct(expr),
            lume_hir::ExpressionKind::StaticCall(expr) => self.static_call(expr),
            lume_hir::ExpressionKind::InstanceCall(expr) => self.instance_call(expr),
            lume_hir::ExpressionKind::IntrinsicCall(call) => self.intrinsic_call(call),
            lume_hir::ExpressionKind::Literal(lit) => self.literal(&lit.kind),
            lume_hir::ExpressionKind::Logical(_) => todo!("logical MIR lowering"),
            lume_hir::ExpressionKind::Member(_) => todo!("member MIR lowering"),
            lume_hir::ExpressionKind::Variable(var) => self.variable_reference(var),
            lume_hir::ExpressionKind::Variant(_) => todo!("enum variant MIR lowering"),
            lume_hir::ExpressionKind::Void => unreachable!(),
        }
    }

    fn assignment(&mut self, expr: &lume_hir::Assignment) -> lume_mir::Operand {
        let lume_mir::Operand::Reference { id } = self.expression(&expr.target) else {
            todo!()
        };

        let value = self.expression(&expr.value);
        self.func.current_block_mut().store(id, value);

        lume_mir::Operand::Load { id }
    }

    fn construct(&mut self, expr: &lume_hir::Construct) -> lume_mir::Operand {
        let constructed_type = self.tcx().find_type_ref(&expr.path).unwrap().unwrap();
        let properties = self.tcx().tdb().find_properties(constructed_type.instance_of);

        let struct_type = lume_mir::Type::Struct {
            properties: properties
                .map(|prop| self.lower_type(&prop.property_type))
                .collect::<Vec<_>>(),
        };

        let reg = self.func.add_register(struct_type.clone());
        self.func.current_block_mut().allocate_heap(reg, struct_type);

        lume_mir::Operand::Reference { id: reg }
    }

    fn static_call(&mut self, expr: &lume_hir::StaticCall) -> lume_mir::Operand {
        let callable = self.tcx().probe_callable_static(expr).unwrap();
        let func_id = self.mir.new_function_id(callable.to_call_reference());
        let args = expr.arguments.iter().map(|arg| self.expression(arg)).collect();

        self.call(func_id, args)
    }

    fn instance_call(&mut self, expr: &lume_hir::InstanceCall) -> lume_mir::Operand {
        let callable = self.tcx().probe_callable_instance(expr).unwrap();
        let func_id = self.mir.new_function_id(callable.to_call_reference());

        let mut args = vec![self.expression(&expr.callee)];
        for expr_arg in &expr.arguments {
            args.push(self.expression(expr_arg));
        }

        self.call(func_id, args)
    }

    fn intrinsic_call(&mut self, expr: &lume_hir::IntrinsicCall) -> lume_mir::Operand {
        let name = self.intrinsic_of(expr);
        let args = expr.arguments.iter().map(|arg| self.expression(arg)).collect();

        let decl = lume_mir::Declaration::Intrinsic { name, args };
        let reg = self.declare(decl);

        lume_mir::Operand::Load { id: reg }
    }

    fn intrinsic_of(&mut self, expr: &lume_hir::IntrinsicCall) -> lume_mir::Intrinsic {
        let callee_ty = self.tcx().type_of_expr(expr.callee()).unwrap();

        if callee_ty.is_integer() {
            let (bits, signed) = match callee_ty {
                ty if ty.is_i8() => (8, true),
                ty if ty.is_i16() => (16, true),
                ty if ty.is_i32() => (32, true),
                ty if ty.is_i64() => (64, true),
                ty if ty.is_u8() => (8, false),
                ty if ty.is_u16() => (16, false),
                ty if ty.is_u32() => (32, false),
                ty if ty.is_u64() => (64, false),
                _ => unreachable!(),
            };

            match expr.name.name().as_str() {
                "==" => lume_mir::Intrinsic::IntEq { bits, signed },
                "!=" => lume_mir::Intrinsic::IntNe { bits, signed },
                ">" => lume_mir::Intrinsic::IntGt { bits, signed },
                ">=" => lume_mir::Intrinsic::IntGe { bits, signed },
                "<" => lume_mir::Intrinsic::IntLt { bits, signed },
                "<=" => lume_mir::Intrinsic::IntLe { bits, signed },
                "+" => lume_mir::Intrinsic::IntAdd { bits, signed },
                "-" => lume_mir::Intrinsic::IntSub { bits, signed },
                "*" => lume_mir::Intrinsic::IntMul { bits, signed },
                "/" => lume_mir::Intrinsic::IntDiv { bits, signed },
                _ => unreachable!(),
            }
        } else if callee_ty.is_float() {
            let bits = match callee_ty {
                ty if ty.is_f32() => 32,
                ty if ty.is_f64() => 64,
                _ => unreachable!(),
            };

            match expr.name.name().as_str() {
                "==" => lume_mir::Intrinsic::FloatEq { bits },
                "!=" => lume_mir::Intrinsic::FloatNe { bits },
                ">" => lume_mir::Intrinsic::FloatGt { bits },
                ">=" => lume_mir::Intrinsic::FloatGe { bits },
                "<" => lume_mir::Intrinsic::FloatLt { bits },
                "<=" => lume_mir::Intrinsic::FloatLe { bits },
                "+" => lume_mir::Intrinsic::FloatAdd { bits },
                "-" => lume_mir::Intrinsic::FloatSub { bits },
                "*" => lume_mir::Intrinsic::FloatMul { bits },
                "/" => lume_mir::Intrinsic::FloatDiv { bits },
                _ => unreachable!(),
            }
        } else if callee_ty.is_bool() {
            match expr.name.name().as_str() {
                "==" => lume_mir::Intrinsic::BooleanEq,
                "!=" => lume_mir::Intrinsic::BooleanNe,
                _ => unreachable!(),
            }
        } else {
            unreachable!()
        }
    }

    fn variable_reference(&mut self, expr: &lume_hir::Variable) -> lume_mir::Operand {
        let id = match &expr.reference {
            lume_hir::VariableSource::Parameter(id) => lume_mir::RegisterId::param(id.index),
            lume_hir::VariableSource::Variable(id) => *self.variables.get(&id.id).unwrap(),
        };

        lume_mir::Operand::Load { id }
    }

    #[expect(clippy::unused_self)]
    fn literal(&self, expr: &lume_hir::LiteralKind) -> lume_mir::Operand {
        match expr {
            lume_hir::LiteralKind::Boolean(val) => lume_mir::Operand::Boolean { value: val.value },
            lume_hir::LiteralKind::Int(val) => {
                let (bits, signed) = match val.kind {
                    lume_hir::IntKind::I8 => (8, true),
                    lume_hir::IntKind::I16 => (16, true),
                    lume_hir::IntKind::I32 => (32, true),
                    lume_hir::IntKind::I64 => (64, true),
                    lume_hir::IntKind::U8 => (8, false),
                    lume_hir::IntKind::U16 => (16, false),
                    lume_hir::IntKind::U32 => (32, false),
                    lume_hir::IntKind::U64 => (64, false),
                };

                lume_mir::Operand::Integer {
                    value: val.value,
                    bits,
                    signed,
                }
            }
            lume_hir::LiteralKind::Float(val) => {
                let bits = match val.kind {
                    lume_hir::FloatKind::F32 => 32,
                    lume_hir::FloatKind::F64 => 64,
                };

                lume_mir::Operand::Float { value: val.value, bits }
            }
            lume_hir::LiteralKind::String(val) => lume_mir::Operand::String {
                value: val.value.clone(),
            },
        }
    }
}
