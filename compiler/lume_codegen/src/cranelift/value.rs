use cranelift::prelude::*;

use crate::cranelift::LowerFunction;

impl LowerFunction<'_> {
    pub(crate) fn cg_declaration(&mut self, decl: &lume_mir::Declaration) -> Value {
        match decl {
            lume_mir::Declaration::Operand(op) => self.cg_operand(op),
            lume_mir::Declaration::Cast { .. } => todo!(),
            lume_mir::Declaration::Call { func_id, args } => {
                let ret = self.call(*func_id, args);

                if ret.is_empty() {
                    self.builder.ins().iconst(self.backend.cl_ptr_type(), 0)
                } else {
                    ret[0]
                }
            }
            lume_mir::Declaration::Intrinsic { name, args } => match *name {
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

                lume_mir::Intrinsic::Metadata { .. } => todo!(),
            },
            lume_mir::Declaration::Reference { id } => self.use_var(*id),
            lume_mir::Declaration::Load { id } => self.load_var(*id),
        }
    }

    pub(crate) fn cg_operand(&mut self, op: &lume_mir::Operand) -> Value {
        match op {
            lume_mir::Operand::Boolean { value } => self.builder.ins().iconst(Self::cl_bool_type(), i64::from(*value)),
            lume_mir::Operand::Integer { bits, value, .. } => match *bits {
                8 => self.builder.ins().iconst(types::I8, *value),
                16 => self.builder.ins().iconst(types::I16, *value),
                32 => self.builder.ins().iconst(types::I32, *value),
                64 => self.builder.ins().iconst(types::I64, *value),
                _ => unreachable!(),
            },
            lume_mir::Operand::Float { bits, value } => match *bits {
                #[expect(clippy::cast_possible_truncation)]
                32 => self.builder.ins().f32const(*value as f32),
                64 => self.builder.ins().f64const(*value),
                _ => unreachable!(),
            },
            lume_mir::Operand::String { .. } => todo!(),
            lume_mir::Operand::Load { id } => self.load_var(*id),
            lume_mir::Operand::LoadField { target, index, offset } => self.load_field(*target, *index, *offset),
            lume_mir::Operand::Reference { id } => self.use_var(*id),
        }
    }
}
