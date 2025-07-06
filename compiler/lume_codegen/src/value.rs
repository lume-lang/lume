use inkwell::values::{BasicValue, BasicValueEnum};
use lume_mir::RegisterId;

use crate::FunctionLower;

impl<'ctx> FunctionLower<'ctx> {
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
    fn intrinsic_value(&self, name: &lume_mir::Intrinsic, args: &[RegisterId]) -> BasicValueEnum<'ctx> {
        match name {
            lume_mir::Intrinsic::IntAdd { bits, .. } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_int(*bits, lhs_ptr);
                let rhs = self.builder.load_int(*bits, rhs_ptr);

                self.builder.int_add(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntSub { bits, .. } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_int(*bits, lhs_ptr);
                let rhs = self.builder.load_int(*bits, rhs_ptr);

                self.builder.int_sub(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntMul { bits, .. } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_int(*bits, lhs_ptr);
                let rhs = self.builder.load_int(*bits, rhs_ptr);

                self.builder.int_mul(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntDiv { bits, signed } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_int(*bits, lhs_ptr);
                let rhs = self.builder.load_int(*bits, rhs_ptr);

                if *signed {
                    self.builder.int_signed_div(lhs, rhs).as_basic_value_enum()
                } else {
                    self.builder.int_unsigned_div(lhs, rhs).as_basic_value_enum()
                }
            }
            lume_mir::Intrinsic::IntEq { bits, .. } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_int(*bits, lhs_ptr);
                let rhs = self.builder.load_int(*bits, rhs_ptr);

                self.builder.int_eq(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntNe { bits, .. } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_int(*bits, lhs_ptr);
                let rhs = self.builder.load_int(*bits, rhs_ptr);

                self.builder.int_ne(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntGt { bits, signed } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_int(*bits, lhs_ptr);
                let rhs = self.builder.load_int(*bits, rhs_ptr);

                self.builder.int_gt(*signed, lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntGe { bits, signed } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_int(*bits, lhs_ptr);
                let rhs = self.builder.load_int(*bits, rhs_ptr);

                self.builder.int_ge(*signed, lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntLt { bits, signed } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_int(*bits, lhs_ptr);
                let rhs = self.builder.load_int(*bits, rhs_ptr);

                self.builder.int_lt(*signed, lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::IntLe { bits, signed } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_int(*bits, lhs_ptr);
                let rhs = self.builder.load_int(*bits, rhs_ptr);

                self.builder.int_le(*signed, lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatAdd { bits } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_float(*bits, lhs_ptr);
                let rhs = self.builder.load_float(*bits, rhs_ptr);

                self.builder.float_add(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatSub { bits } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_float(*bits, lhs_ptr);
                let rhs = self.builder.load_float(*bits, rhs_ptr);

                self.builder.float_sub(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatMul { bits } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_float(*bits, lhs_ptr);
                let rhs = self.builder.load_float(*bits, rhs_ptr);

                self.builder.float_mul(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatDiv { bits } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_float(*bits, lhs_ptr);
                let rhs = self.builder.load_float(*bits, rhs_ptr);

                self.builder.float_div(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatEq { bits } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_float(*bits, lhs_ptr);
                let rhs = self.builder.load_float(*bits, rhs_ptr);

                self.builder.float_eq(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatNe { bits } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_float(*bits, lhs_ptr);
                let rhs = self.builder.load_float(*bits, rhs_ptr);

                self.builder.float_ne(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatGt { bits } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_float(*bits, lhs_ptr);
                let rhs = self.builder.load_float(*bits, rhs_ptr);

                self.builder.float_gt(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatGe { bits } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_float(*bits, lhs_ptr);
                let rhs = self.builder.load_float(*bits, rhs_ptr);

                self.builder.float_ge(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatLt { bits } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_float(*bits, lhs_ptr);
                let rhs = self.builder.load_float(*bits, rhs_ptr);

                self.builder.float_lt(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::FloatLe { bits } => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_float(*bits, lhs_ptr);
                let rhs = self.builder.load_float(*bits, rhs_ptr);

                self.builder.float_le(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::BooleanEq => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_bool(lhs_ptr);
                let rhs = self.builder.load_bool(rhs_ptr);

                self.builder.int_eq(lhs, rhs).as_basic_value_enum()
            }
            lume_mir::Intrinsic::BooleanNe => {
                let (lhs_ptr, _) = self.load(args[0]);
                let (rhs_ptr, _) = self.load(args[1]);

                let lhs = self.builder.load_bool(lhs_ptr);
                let rhs = self.builder.load_bool(rhs_ptr);

                self.builder.int_ne(lhs, rhs).as_basic_value_enum()
            }
        }
    }
}
