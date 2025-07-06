use inkwell::types::BasicType;

use crate::Context;

impl Context {
    pub fn lower_type(&'_ self, ty: &lume_mir::Type) -> inkwell::types::BasicTypeEnum<'_> {
        match &ty {
            lume_mir::Type::Void | lume_mir::Type::Boolean => self.bool_type().as_basic_type_enum(),
            lume_mir::Type::Integer { bits: n, .. } => self.int_type(*n).as_basic_type_enum(),
            lume_mir::Type::Float { bits: n } => match n {
                32 => self.f32_type().as_basic_type_enum(),
                64 => self.f64_type().as_basic_type_enum(),
                _ => panic!("unsupported float type: {n}-bits"),
            },
            lume_mir::Type::String => self.string_type().as_basic_type_enum(),
            lume_mir::Type::Struct { properties } => self.struct_type(properties).as_basic_type_enum(),
            lume_mir::Type::Pointer => self.ptr_type().as_basic_type_enum(),
        }
    }

    pub fn lower_fn_type(
        &'_ self,
        params: &[lume_mir::Type],
        return_ty: &lume_mir::Type,
        varargs: bool,
    ) -> inkwell::types::FunctionType<'_> {
        let params = params
            .iter()
            .map(|ty| self.lower_type(ty).into())
            .collect::<Vec<inkwell::types::BasicMetadataTypeEnum>>();

        if *return_ty == lume_mir::Type::Void {
            self.void_type().fn_type(&params, varargs)
        } else {
            self.lower_type(return_ty).fn_type(&params, varargs)
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

    pub fn char_type(&self) -> inkwell::types::IntType<'_> {
        self.int_type(8)
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

    #[expect(clippy::cast_possible_truncation)]
    pub fn string_type(&self) -> inkwell::types::ArrayType<'_> {
        self.char_type().array_type(size_of::<String>() as u32)
    }

    pub fn struct_type(&self, properties: &[lume_mir::Type]) -> inkwell::types::StructType<'_> {
        let props = properties.iter().map(|ty| self.lower_type(ty)).collect::<Vec<_>>();

        self.inner.struct_type(&props, false)
    }
}
