use crate::FunctionTransformer;

impl FunctionTransformer<'_> {
    pub(super) fn lower_type(&self, type_ref: &lume_types::TypeRef) -> lume_mir::Type {
        let ty = self.transformer.tcx.db().type_(type_ref.instance_of).unwrap();

        match ty {
            ty if ty.kind == lume_types::TypeKindRef::Void => lume_mir::Type::Void,
            ty if ty.name == lume_hir::Path::i8() => lume_mir::Type::Integer { bits: 8, signed: true },
            ty if ty.name == lume_hir::Path::i16() => lume_mir::Type::Integer { bits: 16, signed: true },
            ty if ty.name == lume_hir::Path::i32() => lume_mir::Type::Integer { bits: 32, signed: true },
            ty if ty.name == lume_hir::Path::i64() => lume_mir::Type::Integer { bits: 64, signed: true },
            ty if ty.name == lume_hir::Path::u8() => lume_mir::Type::Integer { bits: 8, signed: false },
            ty if ty.name == lume_hir::Path::u16() => lume_mir::Type::Integer {
                bits: 16,
                signed: false,
            },
            ty if ty.name == lume_hir::Path::u32() => lume_mir::Type::Integer {
                bits: 32,
                signed: false,
            },
            ty if ty.name == lume_hir::Path::u64() => lume_mir::Type::Integer {
                bits: 64,
                signed: false,
            },
            _ => lume_mir::Type::Pointer,
        }
    }
}
