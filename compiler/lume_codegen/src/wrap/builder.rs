use std::{collections::HashMap, sync::RwLock};

use inkwell::{
    types::BasicType,
    values::{BasicValue, BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue},
};
use lume_mir::BasicBlockId;

use crate::{BasicBlock, Context};

pub(crate) struct Builder<'ctx> {
    inner: inkwell::builder::Builder<'ctx>,
    pub(crate) func_value: FunctionValue<'ctx>,
    pub(crate) ctx: &'ctx Context,

    blocks: RwLock<HashMap<BasicBlockId, BasicBlock<'ctx>>>,
}

impl<'ctx> Builder<'ctx> {
    pub fn new(ctx: &'ctx Context, func_value: FunctionValue<'ctx>) -> Self {
        Self {
            inner: ctx.inner.create_builder(),
            func_value,
            ctx,
            blocks: RwLock::new(HashMap::new()),
        }
    }

    pub fn add_block(&self) -> BasicBlock<'ctx> {
        self.ctx.inner.append_basic_block(self.func_value, "")
    }

    pub fn switch_to_block(&self, block: BasicBlock<'ctx>) {
        self.inner.position_at_end(block);
    }

    pub fn switch_to_block_id(&self, block: BasicBlockId) {
        self.inner.position_at_end(self.block(block));
    }

    pub fn block(&self, id: BasicBlockId) -> BasicBlock<'ctx> {
        *self.blocks.read().unwrap().get(&id).unwrap()
    }

    pub fn register_block(&self, id: BasicBlockId, block: BasicBlock<'ctx>) {
        self.blocks.write().unwrap().insert(id, block);
    }

    #[allow(clippy::cast_lossless)]
    pub fn bool_literal(&self, value: bool) -> inkwell::values::IntValue<'ctx> {
        self.ctx.bool_type().const_int(value as u64, false)
    }

    #[expect(clippy::cast_sign_loss)]
    pub fn i8_literal(&self, value: i64) -> inkwell::values::IntValue<'ctx> {
        self.ctx.int_type(8).const_int(value as u64, true)
    }

    #[expect(clippy::cast_sign_loss)]
    pub fn i16_literal(&self, value: i64) -> inkwell::values::IntValue<'ctx> {
        self.ctx.int_type(16).const_int(value as u64, true)
    }

    #[expect(clippy::cast_sign_loss)]
    pub fn i32_literal(&self, value: i64) -> inkwell::values::IntValue<'ctx> {
        self.ctx.int_type(32).const_int(value as u64, true)
    }

    #[expect(clippy::cast_sign_loss)]
    pub fn i64_literal(&self, value: i64) -> inkwell::values::IntValue<'ctx> {
        self.ctx.int_type(64).const_int(value as u64, true)
    }

    pub fn f32_literal(&self, value: f64) -> inkwell::values::FloatValue<'ctx> {
        self.ctx.f32_type().const_float(value)
    }

    pub fn f64_literal(&self, value: f64) -> inkwell::values::FloatValue<'ctx> {
        self.ctx.f64_type().const_float(value)
    }

    pub fn string_literal(&self, value: &str) -> inkwell::values::PointerValue<'ctx> {
        self.inner
            .build_global_string_ptr(value, "")
            .unwrap()
            .as_pointer_value()
    }

    pub fn alloca<T: BasicType<'ctx>>(&self, ty: T) -> PointerValue<'ctx> {
        self.inner.build_alloca(ty, "").unwrap()
    }

    pub fn load<T: BasicType<'ctx>>(&self, ptr: PointerValue<'ctx>, ty: T) -> BasicValueEnum<'ctx> {
        self.inner.build_load(ty, ptr, "").unwrap()
    }

    pub fn load_i8(&self, ptr: PointerValue<'ctx>) -> IntValue<'ctx> {
        self.load(ptr, self.ctx.int_type(8)).into_int_value()
    }

    pub fn load_i16(&self, ptr: PointerValue<'ctx>) -> IntValue<'ctx> {
        self.load(ptr, self.ctx.int_type(16)).into_int_value()
    }

    pub fn load_i32(&self, ptr: PointerValue<'ctx>) -> IntValue<'ctx> {
        self.load(ptr, self.ctx.int_type(32)).into_int_value()
    }

    pub fn load_i64(&self, ptr: PointerValue<'ctx>) -> IntValue<'ctx> {
        self.load(ptr, self.ctx.int_type(64)).into_int_value()
    }

    pub fn load_int(&self, bits: u8, ptr: PointerValue<'ctx>) -> IntValue<'ctx> {
        match bits {
            8 => self.load_i8(ptr),
            16 => self.load_i16(ptr),
            32 => self.load_i32(ptr),
            64 => self.load_i64(ptr),
            _ => panic!("Unsupported bit width for integer type"),
        }
    }

    pub fn load_f32(&self, ptr: PointerValue<'ctx>) -> FloatValue<'ctx> {
        self.load(ptr, self.ctx.f32_type()).into_float_value()
    }

    pub fn load_f64(&self, ptr: PointerValue<'ctx>) -> FloatValue<'ctx> {
        self.load(ptr, self.ctx.f64_type()).into_float_value()
    }

    pub fn load_float(&self, bits: u8, ptr: PointerValue<'ctx>) -> FloatValue<'ctx> {
        match bits {
            32 => self.load_f32(ptr),
            64 => self.load_f64(ptr),
            _ => panic!("Unsupported bit width for float type"),
        }
    }

    pub fn load_bool(&self, ptr: PointerValue<'ctx>) -> IntValue<'ctx> {
        self.load(ptr, self.ctx.int_type(1)).into_int_value()
    }

    pub fn int_add(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        self.inner.build_int_add(lhs, rhs, "").unwrap()
    }

    pub fn int_sub(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        self.inner.build_int_sub(lhs, rhs, "").unwrap()
    }

    pub fn int_mul(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        self.inner.build_int_mul(lhs, rhs, "").unwrap()
    }

    pub fn int_signed_div(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        self.inner.build_int_signed_div(lhs, rhs, "").unwrap()
    }

    pub fn int_unsigned_div(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        self.inner.build_int_unsigned_div(lhs, rhs, "").unwrap()
    }

    pub fn int_compare(&self, op: inkwell::IntPredicate, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        self.inner.build_int_compare(op, lhs, rhs, "").unwrap()
    }

    pub fn int_eq(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        self.int_compare(inkwell::IntPredicate::EQ, lhs, rhs)
    }

    pub fn int_ne(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        self.int_compare(inkwell::IntPredicate::NE, lhs, rhs)
    }

    pub fn int_gt(&self, signed: bool, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        if signed {
            self.int_signed_gt(lhs, rhs)
        } else {
            self.int_unsigned_gt(lhs, rhs)
        }
    }

    pub fn int_signed_gt(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        self.int_compare(inkwell::IntPredicate::SGT, lhs, rhs)
    }

    pub fn int_unsigned_gt(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        self.int_compare(inkwell::IntPredicate::UGT, lhs, rhs)
    }

    pub fn int_ge(&self, signed: bool, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        if signed {
            self.int_signed_ge(lhs, rhs)
        } else {
            self.int_unsigned_ge(lhs, rhs)
        }
    }

    pub fn int_signed_ge(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        self.int_compare(inkwell::IntPredicate::SGE, lhs, rhs)
    }

    pub fn int_unsigned_ge(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        self.int_compare(inkwell::IntPredicate::UGE, lhs, rhs)
    }

    pub fn int_lt(&self, signed: bool, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        if signed {
            self.int_signed_lt(lhs, rhs)
        } else {
            self.int_unsigned_lt(lhs, rhs)
        }
    }

    pub fn int_signed_lt(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        self.int_compare(inkwell::IntPredicate::SLT, lhs, rhs)
    }

    pub fn int_unsigned_lt(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        self.int_compare(inkwell::IntPredicate::ULT, lhs, rhs)
    }

    pub fn int_le(&self, signed: bool, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        if signed {
            self.int_signed_le(lhs, rhs)
        } else {
            self.int_unsigned_le(lhs, rhs)
        }
    }

    pub fn int_signed_le(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        self.int_compare(inkwell::IntPredicate::SLE, lhs, rhs)
    }

    pub fn int_unsigned_le(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> IntValue<'ctx> {
        self.int_compare(inkwell::IntPredicate::ULE, lhs, rhs)
    }

    pub fn float_add(&self, lhs: FloatValue<'ctx>, rhs: FloatValue<'ctx>) -> FloatValue<'ctx> {
        self.inner.build_float_add(lhs, rhs, "").unwrap()
    }

    pub fn float_sub(&self, lhs: FloatValue<'ctx>, rhs: FloatValue<'ctx>) -> FloatValue<'ctx> {
        self.inner.build_float_sub(lhs, rhs, "").unwrap()
    }

    pub fn float_mul(&self, lhs: FloatValue<'ctx>, rhs: FloatValue<'ctx>) -> FloatValue<'ctx> {
        self.inner.build_float_mul(lhs, rhs, "").unwrap()
    }

    pub fn float_div(&self, lhs: FloatValue<'ctx>, rhs: FloatValue<'ctx>) -> FloatValue<'ctx> {
        self.inner.build_float_div(lhs, rhs, "").unwrap()
    }

    pub fn float_compare(
        &self,
        predicate: inkwell::FloatPredicate,
        lhs: FloatValue<'ctx>,
        rhs: FloatValue<'ctx>,
    ) -> IntValue<'ctx> {
        self.inner.build_float_compare(predicate, lhs, rhs, "").unwrap()
    }

    pub fn float_eq(&self, lhs: FloatValue<'ctx>, rhs: FloatValue<'ctx>) -> IntValue<'ctx> {
        self.float_compare(inkwell::FloatPredicate::OEQ, lhs, rhs)
    }

    pub fn float_ne(&self, lhs: FloatValue<'ctx>, rhs: FloatValue<'ctx>) -> IntValue<'ctx> {
        self.float_compare(inkwell::FloatPredicate::ONE, lhs, rhs)
    }

    pub fn float_gt(&self, lhs: FloatValue<'ctx>, rhs: FloatValue<'ctx>) -> IntValue<'ctx> {
        self.float_compare(inkwell::FloatPredicate::OGT, lhs, rhs)
    }

    pub fn float_ge(&self, lhs: FloatValue<'ctx>, rhs: FloatValue<'ctx>) -> IntValue<'ctx> {
        self.float_compare(inkwell::FloatPredicate::OGE, lhs, rhs)
    }

    pub fn float_lt(&self, lhs: FloatValue<'ctx>, rhs: FloatValue<'ctx>) -> IntValue<'ctx> {
        self.float_compare(inkwell::FloatPredicate::OLT, lhs, rhs)
    }

    pub fn float_le(&self, lhs: FloatValue<'ctx>, rhs: FloatValue<'ctx>) -> IntValue<'ctx> {
        self.float_compare(inkwell::FloatPredicate::OLE, lhs, rhs)
    }

    pub fn store<T: BasicValue<'ctx>>(&self, ptr: PointerValue<'ctx>, val: T) {
        self.inner.build_store(ptr, val).unwrap();
    }

    pub fn branch(&self, block: BasicBlock<'ctx>) {
        self.inner.build_unconditional_branch(block).unwrap();
    }

    pub fn conditional_branch(
        &self,
        condition: IntValue<'ctx>,
        then_block: BasicBlock<'ctx>,
        else_block: BasicBlock<'ctx>,
    ) {
        self.inner
            .build_conditional_branch(condition, then_block, else_block)
            .unwrap();
    }

    pub fn unreachable(&self) {
        self.inner.build_unreachable().unwrap();
    }

    pub fn return_value(&self, value: &dyn BasicValue<'ctx>) {
        self.inner.build_return(Some(value)).unwrap();
    }

    pub fn return_void(&self) {
        self.inner.build_return(None).unwrap();
    }
}
