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
            | lume_mir::TypeKind::Metadata { .. }
            | lume_mir::TypeKind::Void => self.cl_ptr_type(),
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
    pub(crate) fn cl_bool_type() -> types::Type {
        CraneliftBackend::cl_bool_type()
    }
}
