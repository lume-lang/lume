use inkwell::values::{BasicValue, BasicValueEnum, FloatValue, IntValue};

use crate::FunctionLower;

impl<'ctx> FunctionLower<'_, 'ctx> {
    pub(super) fn operand(&self, val: &lume_mir::Operand) -> BasicValueEnum<'ctx> {
        match val {
            lume_mir::Operand::Boolean { value } => self.builder.bool_literal(*value).as_basic_value_enum(),
            lume_mir::Operand::Integer { bits, value, .. } => match *bits {
                8 => self.builder.i8_literal(*value).as_basic_value_enum(),
                16 => self.builder.i16_literal(*value).as_basic_value_enum(),
                32 => self.builder.i32_literal(*value).as_basic_value_enum(),
                64 => self.builder.i64_literal(*value).as_basic_value_enum(),
                _ => unimplemented!(),
            },
            lume_mir::Operand::Float { bits, value } => match *bits {
                32 => self.builder.f32_literal(*value).as_basic_value_enum(),
                64 => self.builder.f64_literal(*value).as_basic_value_enum(),
                _ => unimplemented!(),
            },
            lume_mir::Operand::String { value } => self.builder.string_literal(value.as_str()).as_basic_value_enum(),
            lume_mir::Operand::Load { id } => self.load(*id),
            lume_mir::Operand::LoadField { target, field } => self.load_field(*target, *field),
            lume_mir::Operand::Reference { id } => self.retrieve_var_ptr(*id).0.as_basic_value_enum(),
        }
    }

    pub(super) fn decl_value(&self, decl: &lume_mir::Declaration) -> BasicValueEnum<'ctx> {
        match decl {
            lume_mir::Declaration::Operand(val) => self.operand(val),
            lume_mir::Declaration::Intrinsic { name, args } => self.intrinsic_value(name, args),
            lume_mir::Declaration::Call { func_id, args } => {
                let func = self.module.find_function(*func_id);
                let args = args.iter().map(|arg| self.operand(arg)).collect::<Vec<_>>();

                self.builder.call_with_return(func, &args)
            }
            _ => todo!(),
        }
    }

    #[expect(clippy::too_many_lines)]
    fn intrinsic_value(&self, name: &lume_mir::Intrinsic, args: &[lume_mir::Operand]) -> BasicValueEnum<'ctx> {
        match name {
            lume_mir::Intrinsic::IntAdd { .. } => {
                let lhs = self.load_int_from(&args[0]);
                let rhs = self.load_int_from(&args[1]);

                self.builder.int_add(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntSub { .. } => {
                let lhs = self.load_int_from(&args[0]);
                let rhs = self.load_int_from(&args[1]);

                self.builder.int_sub(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntMul { .. } => {
                let lhs = self.load_int_from(&args[0]);
                let rhs = self.load_int_from(&args[1]);

                self.builder.int_mul(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntDiv { signed, .. } => {
                let lhs = self.load_int_from(&args[0]);
                let rhs = self.load_int_from(&args[1]);

                if *signed {
                    self.builder.int_signed_div(lhs, rhs).as_basic_value_enum()
                } else {
                    self.builder.int_unsigned_div(lhs, rhs).as_basic_value_enum()
                }
            }
            lume_mir::Intrinsic::IntEq { .. } | lume_mir::Intrinsic::BooleanEq => {
                let lhs = self.load_int_from(&args[0]);
                let rhs = self.load_int_from(&args[1]);

                self.builder.int_eq(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntNe { .. } | lume_mir::Intrinsic::BooleanNe => {
                let lhs = self.load_int_from(&args[0]);
                let rhs = self.load_int_from(&args[1]);

                self.builder.int_ne(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntGt { signed, .. } => {
                let lhs = self.load_int_from(&args[0]);
                let rhs = self.load_int_from(&args[1]);

                self.builder.int_gt(*signed, lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntGe { signed, .. } => {
                let lhs = self.load_int_from(&args[0]);
                let rhs = self.load_int_from(&args[1]);

                self.builder.int_ge(*signed, lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntLt { signed, .. } => {
                let lhs = self.load_int_from(&args[0]);
                let rhs = self.load_int_from(&args[1]);

                self.builder.int_lt(*signed, lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntLe { signed, .. } => {
                let lhs = self.load_int_from(&args[0]);
                let rhs = self.load_int_from(&args[1]);

                self.builder.int_le(*signed, lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntAnd { .. } | lume_mir::Intrinsic::BooleanAnd => {
                let lhs = self.load_int_from(&args[0]);
                let rhs = self.load_int_from(&args[1]);

                self.builder.int_and(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntOr { .. } | lume_mir::Intrinsic::BooleanOr => {
                let lhs = self.load_int_from(&args[0]);
                let rhs = self.load_int_from(&args[1]);

                self.builder.int_or(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntXor { .. } => {
                let lhs = self.load_int_from(&args[0]);
                let rhs = self.load_int_from(&args[1]);

                self.builder.int_xor(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatAdd { .. } => {
                let lhs = self.load_float_from(&args[0]);
                let rhs = self.load_float_from(&args[1]);

                self.builder.float_add(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatSub { .. } => {
                let lhs = self.load_float_from(&args[0]);
                let rhs = self.load_float_from(&args[1]);

                self.builder.float_sub(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatMul { .. } => {
                let lhs = self.load_float_from(&args[0]);
                let rhs = self.load_float_from(&args[1]);
                self.builder.float_mul(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatDiv { .. } => {
                let lhs = self.load_float_from(&args[0]);
                let rhs = self.load_float_from(&args[1]);

                self.builder.float_div(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatEq { .. } => {
                let lhs = self.load_float_from(&args[0]);
                let rhs = self.load_float_from(&args[1]);

                self.builder.float_eq(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatNe { .. } => {
                let lhs = self.load_float_from(&args[0]);
                let rhs = self.load_float_from(&args[1]);

                self.builder.float_ne(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatGt { .. } => {
                let lhs = self.load_float_from(&args[0]);
                let rhs = self.load_float_from(&args[1]);

                self.builder.float_gt(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatGe { .. } => {
                let lhs = self.load_float_from(&args[0]);
                let rhs = self.load_float_from(&args[1]);

                self.builder.float_ge(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatLt { .. } => {
                let lhs = self.load_float_from(&args[0]);
                let rhs = self.load_float_from(&args[1]);

                self.builder.float_lt(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatLe { .. } => {
                let lhs = self.load_float_from(&args[0]);
                let rhs = self.load_float_from(&args[1]);

                self.builder.float_le(lhs, rhs).as_basic_value_enum()
            }
        }
    }

    pub(crate) fn load_int_from(&self, value: &lume_mir::Operand) -> IntValue<'ctx> {
        match value {
            int @ (lume_mir::Operand::Integer { .. } | lume_mir::Operand::Boolean { .. }) => {
                self.operand(int).into_int_value()
            }
            lume_mir::Operand::Load { id } => self.load(*id).into_int_value(),
            _ => panic!("Unsupported value type for integer loading"),
        }
    }

    pub(crate) fn load_float_from(&self, value: &lume_mir::Operand) -> FloatValue<'ctx> {
        match value {
            float @ lume_mir::Operand::Float { .. } => self.operand(float).into_float_value(),
            lume_mir::Operand::Load { id } => self.load(*id).into_float_value(),
            _ => panic!("Unsupported value type for float loading"),
        }
    }
}
