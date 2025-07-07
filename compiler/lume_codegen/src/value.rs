use inkwell::values::{BasicValue, BasicValueEnum, FloatValue, IntValue};

use crate::FunctionLower;

impl<'ctx> FunctionLower<'_, 'ctx> {
    pub(super) fn value(&self, val: &lume_mir::Value) -> BasicValueEnum<'ctx> {
        match val {
            lume_mir::Value::Boolean { value } => self.builder.bool_literal(*value).as_basic_value_enum(),
            lume_mir::Value::Integer { bits, value, .. } => match *bits {
                8 => self.builder.i8_literal(*value).as_basic_value_enum(),
                16 => self.builder.i16_literal(*value).as_basic_value_enum(),
                32 => self.builder.i32_literal(*value).as_basic_value_enum(),
                64 => self.builder.i64_literal(*value).as_basic_value_enum(),
                _ => unimplemented!(),
            },
            lume_mir::Value::Float { bits, value } => match *bits {
                32 => self.builder.f32_literal(*value).as_basic_value_enum(),
                64 => self.builder.f64_literal(*value).as_basic_value_enum(),
                _ => unimplemented!(),
            },
            lume_mir::Value::String { value } => self.builder.string_literal(value.as_str()).as_basic_value_enum(),
            lume_mir::Value::Reference { id } => {
                let (val, _) = self.load(*id);

                val.as_basic_value_enum()
            }
            lume_mir::Value::Load { id } => {
                let (val, _) = self.load_ptr(*id);

                val.as_basic_value_enum()
            }
            lume_mir::Value::Call { func_id, args } => {
                let func = self.module.find_function(*func_id);
                let args = args.iter().map(|arg| self.value(arg)).collect::<Vec<_>>();

                self.builder.call_with_return(func, &args)
            }
        }
    }

    pub(super) fn decl_value(&self, decl: &lume_mir::Declaration) -> BasicValueEnum<'ctx> {
        match decl {
            lume_mir::Declaration::Value(val) => self.value(val),
            lume_mir::Declaration::Intrinsic { name, args } => self.intrinsic_value(name, args),
            _ => todo!(),
        }
    }

    #[expect(clippy::too_many_lines)]
    fn intrinsic_value(&self, name: &lume_mir::Intrinsic, args: &[lume_mir::Value]) -> BasicValueEnum<'ctx> {
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
            lume_mir::Intrinsic::IntEq { .. } => {
                let lhs = self.load_int_from(&args[0]);
                let rhs = self.load_int_from(&args[1]);

                self.builder.int_eq(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntNe { .. } => {
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
            lume_mir::Intrinsic::BooleanEq => {
                let lhs = self.load_bool_from(&args[0]);
                let rhs = self.load_bool_from(&args[1]);

                self.builder.int_eq(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::BooleanNe => {
                let lhs = self.load_bool_from(&args[0]);
                let rhs = self.load_bool_from(&args[1]);

                self.builder.int_ne(lhs, rhs).as_basic_value_enum()
            }
        }
    }

    pub(crate) fn load_bool_from(&self, value: &lume_mir::Value) -> IntValue<'ctx> {
        match value {
            int @ lume_mir::Value::Boolean { .. } => self.value(int).into_int_value(),
            lume_mir::Value::Reference { id } => self.load(*id).0.into_int_value(),
            lume_mir::Value::Load { id } => {
                let (ptr, _) = self.load_ptr(*id);

                self.builder.load_bool(ptr)
            }
            _ => panic!("Unsupported value type for integer loading"),
        }
    }

    pub(crate) fn load_int_from(&self, value: &lume_mir::Value) -> IntValue<'ctx> {
        match value {
            int @ lume_mir::Value::Integer { .. } => self.value(int).into_int_value(),
            lume_mir::Value::Load { id } | lume_mir::Value::Reference { id } => {
                let (ptr, _) = self.load_ptr(*id);

                self.builder.load_int(value.bitsize(), ptr)
            }
            _ => panic!("Unsupported value type for integer loading"),
        }
    }

    pub(crate) fn load_float_from(&self, value: &lume_mir::Value) -> FloatValue<'ctx> {
        match value {
            float @ lume_mir::Value::Float { .. } => self.value(float).into_float_value(),
            lume_mir::Value::Reference { id } => self.load(*id).0.into_float_value(),
            lume_mir::Value::Load { id } => {
                let (ptr, _) = self.load_ptr(*id);

                self.builder.load_float(value.bitsize(), ptr)
            }
            _ => panic!("Unsupported value type for integer loading"),
        }
    }
}
