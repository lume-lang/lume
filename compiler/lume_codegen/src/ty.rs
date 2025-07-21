use inkwell::types::BasicType;

use crate::Context;

impl Context {
    pub fn lower_type(&'_ self, ty: &lume_mir::Type) -> inkwell::types::BasicTypeEnum<'_> {
        match &ty.kind {
            lume_mir::TypeKind::Void | lume_mir::TypeKind::Boolean => self.bool_type().as_basic_type_enum(),
            lume_mir::TypeKind::Integer { bits: n, .. } => self.int_type(*n).as_basic_type_enum(),
            lume_mir::TypeKind::Float { bits: n } => match n {
                32 => self.f32_type().as_basic_type_enum(),
                64 => self.f64_type().as_basic_type_enum(),
                _ => panic!("unsupported float type: {n}-bits"),
            },
            lume_mir::TypeKind::String | lume_mir::TypeKind::Pointer { .. } => self.ptr_type().as_basic_type_enum(),
            lume_mir::TypeKind::Struct { properties } => self.struct_type(properties).as_basic_type_enum(),
            lume_mir::TypeKind::Metadata { .. } => todo!(),
        }
    }

    pub fn lower_fn_type(&'_ self, signature: &lume_mir::Signature, varargs: bool) -> inkwell::types::FunctionType<'_> {
        let params = signature
            .parameters
            .iter()
            .map(|ty| self.lower_type(ty).into())
            .collect::<Vec<inkwell::types::BasicMetadataTypeEnum>>();

        if signature.return_type.kind == lume_mir::TypeKind::Void {
            self.void_type().fn_type(&params, varargs)
        } else {
            self.lower_type(&signature.return_type).fn_type(&params, varargs)
        }
    }

    pub fn void_type(&self) -> inkwell::types::VoidType<'_> {
        self.inner.void_type()
    }

    #[allow(clippy::cast_lossless)]
    pub fn int_type(&self, bits: u8) -> inkwell::types::IntType<'_> {
        self.inner.custom_width_int_type(bits as u32)
    }

    pub fn bool_type(&self) -> inkwell::types::IntType<'_> {
        self.inner.bool_type()
    }

    pub fn f32_type(&self) -> inkwell::types::FloatType<'_> {
        self.inner.f32_type()
    }

    pub fn f64_type(&self) -> inkwell::types::FloatType<'_> {
        self.inner.f64_type()
    }

    pub fn ptr_type(&self) -> inkwell::types::PointerType<'_> {
        self.inner.ptr_type(inkwell::AddressSpace::default())
    }

    pub fn struct_type(&self, properties: &[lume_mir::Type]) -> inkwell::types::StructType<'_> {
        let props = properties.iter().map(|ty| self.lower_type(ty)).collect::<Vec<_>>();

        self.inner.struct_type(&props, false)
    }
}
