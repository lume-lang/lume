use cranelift::prelude::*;

use crate::cranelift::LowerFunction;

impl LowerFunction<'_> {
    pub(crate) fn cg_declaration(&mut self, decl: &lume_mir::Declaration) -> Value {
        self.set_srcloc(decl.location);

        match &decl.kind {
            lume_mir::DeclarationKind::Operand(op) => self.cg_operand(op),
            lume_mir::DeclarationKind::Cast { operand, bits } => {
                let operand_ty = self.func.registers.register_ty(*operand);
                let is_int = matches!(operand_ty.kind, lume_mir::TypeKind::Integer { .. });

                if is_int {
                    self.icast(*operand, *bits)
                } else {
                    self.fcast(*operand, *bits)
                }
            }
            lume_mir::DeclarationKind::Call { func_id, args } => {
                let ret = self.call(*func_id, args);

                if ret.is_empty() {
                    self.builder.ins().iconst(self.backend.cl_ptr_type(), 0)
                } else {
                    ret[0]
                }
            }
            lume_mir::DeclarationKind::IndirectCall { ptr, signature, args } => {
                let ret = self.indirect_call(*ptr, signature.to_owned(), args);

                if ret.is_empty() {
                    self.builder.ins().iconst(self.backend.cl_ptr_type(), 0)
                } else {
                    ret[0]
                }
            }
            lume_mir::DeclarationKind::Intrinsic { name, args } => match name {
                // Floating-pointer comparison
                lume_mir::Intrinsic::FloatEq { .. } => self.fcmp(FloatCC::Equal, &args[0], &args[1]),
                lume_mir::Intrinsic::FloatNe { .. } => self.fcmp(FloatCC::NotEqual, &args[0], &args[1]),
                lume_mir::Intrinsic::FloatGe { .. } => self.fcmp(FloatCC::GreaterThanOrEqual, &args[0], &args[1]),
                lume_mir::Intrinsic::FloatGt { .. } => self.fcmp(FloatCC::GreaterThan, &args[0], &args[1]),
                lume_mir::Intrinsic::FloatLe { .. } => self.fcmp(FloatCC::LessThanOrEqual, &args[0], &args[1]),
                lume_mir::Intrinsic::FloatLt { .. } => self.fcmp(FloatCC::LessThan, &args[0], &args[1]),

                // Floating-pointer arithmetic
                lume_mir::Intrinsic::FloatAdd { .. } => self.fadd(&args[0], &args[1]),
                lume_mir::Intrinsic::FloatSub { .. } => self.fsub(&args[0], &args[1]),
                lume_mir::Intrinsic::FloatMul { .. } => self.fmul(&args[0], &args[1]),
                lume_mir::Intrinsic::FloatDiv { .. } => self.fdiv(&args[0], &args[1]),

                // Integer comparison
                lume_mir::Intrinsic::IntEq { .. } | lume_mir::Intrinsic::BooleanEq => {
                    self.icmp(IntCC::Equal, &args[0], &args[1])
                }
                lume_mir::Intrinsic::IntNe { .. } | lume_mir::Intrinsic::BooleanNe => {
                    self.icmp(IntCC::NotEqual, &args[0], &args[1])
                }
                lume_mir::Intrinsic::IntGe { signed: true, .. } => {
                    self.icmp(IntCC::SignedGreaterThanOrEqual, &args[0], &args[1])
                }
                lume_mir::Intrinsic::IntGt { signed: true, .. } => {
                    self.icmp(IntCC::SignedGreaterThan, &args[0], &args[1])
                }
                lume_mir::Intrinsic::IntLe { signed: true, .. } => {
                    self.icmp(IntCC::SignedLessThanOrEqual, &args[0], &args[1])
                }
                lume_mir::Intrinsic::IntLt { signed: true, .. } => self.icmp(IntCC::SignedLessThan, &args[0], &args[1]),
                lume_mir::Intrinsic::IntGe { signed: false, .. } => {
                    self.icmp(IntCC::UnsignedGreaterThanOrEqual, &args[0], &args[1])
                }
                lume_mir::Intrinsic::IntGt { signed: false, .. } => {
                    self.icmp(IntCC::UnsignedGreaterThan, &args[0], &args[1])
                }
                lume_mir::Intrinsic::IntLe { signed: false, .. } => {
                    self.icmp(IntCC::UnsignedLessThanOrEqual, &args[0], &args[1])
                }
                lume_mir::Intrinsic::IntLt { signed: false, .. } => {
                    self.icmp(IntCC::UnsignedLessThan, &args[0], &args[1])
                }

                // Integer arithmetic
                lume_mir::Intrinsic::IntAdd { .. } => self.iadd(&args[0], &args[1]),
                lume_mir::Intrinsic::IntSub { .. } => self.isub(&args[0], &args[1]),
                lume_mir::Intrinsic::IntMul { .. } => self.imul(&args[0], &args[1]),
                lume_mir::Intrinsic::IntDiv { .. } => self.idiv(&args[0], &args[1]),
                lume_mir::Intrinsic::IntAnd { .. } | lume_mir::Intrinsic::BooleanAnd => self.and(&args[0], &args[1]),
                lume_mir::Intrinsic::IntOr { .. } | lume_mir::Intrinsic::BooleanOr => self.or(&args[0], &args[1]),
                lume_mir::Intrinsic::IntXor { .. } => self.xor(&args[0], &args[1]),

                lume_mir::Intrinsic::Metadata { metadata } => {
                    let Some(value) = self.reference_static_data(&metadata.full_name) else {
                        panic!("bug!: no type metadata allocated for `{}`", metadata.full_name);
                    };

                    value
                }
            },
        }
    }

    #[tracing::instrument(level = "TRACE", skip(self), fields(func = %self.func.name), ret(Display))]
    pub(crate) fn cg_operand(&mut self, op: &lume_mir::Operand) -> Value {
        self.set_srcloc(op.location);

        match &op.kind {
            lume_mir::OperandKind::Boolean { value } => {
                self.builder.ins().iconst(Self::cl_bool_type(), i64::from(*value))
            }
            lume_mir::OperandKind::Integer { bits, value, .. } => match *bits {
                8 => self.builder.ins().iconst(types::I8, *value),
                16 => self.builder.ins().iconst(types::I16, *value),
                32 => self.builder.ins().iconst(types::I32, *value),
                64 => self.builder.ins().iconst(types::I64, *value),
                _ => unreachable!(),
            },
            lume_mir::OperandKind::Float { bits, value } => match *bits {
                #[expect(clippy::cast_possible_truncation)]
                32 => self.builder.ins().f32const(*value as f32),
                64 => self.builder.ins().f64const(*value),
                _ => unreachable!(),
            },
            lume_mir::OperandKind::String { value } => self.reference_static_string(value.as_str()),
            lume_mir::OperandKind::Bitcast { source, target } => {
                let source = self.use_var(*source);
                let target_type = self.backend.cl_type_of(target);

                self.builder.ins().bitcast(target_type, MemFlags::new(), source)
            }
            lume_mir::OperandKind::Load { id } => self.load_var(*id),
            lume_mir::OperandKind::LoadField {
                target,
                index,
                offset,
                field_type,
            } => self.load_field_as(*target, *index, *offset, self.backend.cl_type_of(field_type)),
            lume_mir::OperandKind::SlotAddress { id } => {
                let slot = self.retrieve_slot(*id);

                self.builder.ins().stack_addr(self.backend.cl_ptr_type(), slot, 0)
            }
            lume_mir::OperandKind::Reference { id } => self.use_var(*id),
        }
    }
}
