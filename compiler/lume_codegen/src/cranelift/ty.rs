use cranelift::prelude::*;
use cranelift_module::Module;

use crate::cranelift::{CraneliftBackend, LowerFunction};

impl CraneliftBackend<'_> {
    #[tracing::instrument(level = "TRACE", skip(self, ty), fields(ty = %ty), ret(Display))]
    pub(crate) fn cl_type_of(&self, ty: &lume_mir::Type) -> types::Type {
        match &ty.kind {
            lume_mir::TypeKind::Boolean => Self::cl_bool_type(),
            lume_mir::TypeKind::Integer { bits, .. } => match *bits {
                8 => types::I8,
                16 => types::I16,
                32 => types::I32,
                64 => types::I64,
                _ => unreachable!(),
            },
            lume_mir::TypeKind::Float { bits } => match *bits {
                32 => types::F32,
                64 => types::F64,
                _ => unreachable!(),
            },
            lume_mir::TypeKind::String
            | lume_mir::TypeKind::Struct { .. }
            | lume_mir::TypeKind::Union { .. }
            | lume_mir::TypeKind::Pointer { .. }
            | lume_mir::TypeKind::Metadata { .. } => self.cl_ptr_type(),
            lume_mir::TypeKind::Void => unreachable!(),
        }
    }

    pub(crate) fn cl_bool_type() -> types::Type {
        types::I8
    }

    pub(crate) fn cl_ptr_type(&self) -> types::Type {
        self.module().target_config().pointer_type()
    }
}

impl LowerFunction<'_> {
    pub(crate) fn cl_type_of_declaration(&self, decl: &lume_mir::Declaration) -> types::Type {
        match &decl {
            lume_mir::Declaration::Operand(op) => self.cl_type_of_operand(op),
            lume_mir::Declaration::Cast { bits, .. } => match bits {
                8 => types::I8,
                16 => types::I16,
                32 => types::I32,
                64 => types::I64,
                _ => unreachable!(),
            },
            lume_mir::Declaration::Call { func_id, .. } => {
                let func = self.declared_func(*func_id);

                // If the function has no return type, we assume a pointer
                // type is fine.
                let Some(ret) = func.sig.returns.first() else {
                    return self.backend.cl_ptr_type();
                };

                ret.value_type
            }
            lume_mir::Declaration::Intrinsic { name, .. } => match *name {
                lume_mir::Intrinsic::FloatEq { .. }
                | lume_mir::Intrinsic::FloatNe { .. }
                | lume_mir::Intrinsic::FloatGe { .. }
                | lume_mir::Intrinsic::FloatGt { .. }
                | lume_mir::Intrinsic::FloatLe { .. }
                | lume_mir::Intrinsic::FloatLt { .. }
                | lume_mir::Intrinsic::IntEq { .. }
                | lume_mir::Intrinsic::IntNe { .. }
                | lume_mir::Intrinsic::IntGe { .. }
                | lume_mir::Intrinsic::IntGt { .. }
                | lume_mir::Intrinsic::IntLe { .. }
                | lume_mir::Intrinsic::IntLt { .. }
                | lume_mir::Intrinsic::BooleanEq
                | lume_mir::Intrinsic::BooleanNe
                | lume_mir::Intrinsic::BooleanAnd
                | lume_mir::Intrinsic::BooleanOr => Self::cl_bool_type(),
                lume_mir::Intrinsic::IntAdd { bits, .. }
                | lume_mir::Intrinsic::IntSub { bits, .. }
                | lume_mir::Intrinsic::IntMul { bits, .. }
                | lume_mir::Intrinsic::IntDiv { bits, .. }
                | lume_mir::Intrinsic::IntAnd { bits, .. }
                | lume_mir::Intrinsic::IntOr { bits, .. }
                | lume_mir::Intrinsic::IntXor { bits, .. } => match bits {
                    8 => types::I8,
                    16 => types::I16,
                    32 => types::I32,
                    64 => types::I64,
                    _ => unreachable!(),
                },
                lume_mir::Intrinsic::FloatAdd { bits }
                | lume_mir::Intrinsic::FloatSub { bits }
                | lume_mir::Intrinsic::FloatMul { bits }
                | lume_mir::Intrinsic::FloatDiv { bits } => match bits {
                    32 => types::F32,
                    64 => types::F64,
                    _ => unreachable!(),
                },
                lume_mir::Intrinsic::Metadata { .. } => self.backend.cl_ptr_type(),
            },
        }
    }

    pub(crate) fn cl_type_of_operand(&self, op: &lume_mir::Operand) -> types::Type {
        match &op {
            lume_mir::Operand::Boolean { .. } => Self::cl_bool_type(),
            lume_mir::Operand::Integer { bits, .. } => Type::int(u16::from(*bits)).unwrap(),
            lume_mir::Operand::Float { bits, .. } => match *bits {
                32 => types::F32,
                64 => types::F64,
                _ => unreachable!(),
            },
            lume_mir::Operand::String { .. } | lume_mir::Operand::SlotAddress { .. } => self.backend.cl_ptr_type(),
            lume_mir::Operand::Load { id } => self.retrieve_load_type(*id),
            lume_mir::Operand::LoadField { target, index, .. } => self.retrieve_field_type(*target, *index),
            lume_mir::Operand::Reference { id } => self.retrieve_var_type(*id),
        }
    }

    pub(crate) fn cl_bool_type() -> types::Type {
        CraneliftBackend::cl_bool_type()
    }
}
