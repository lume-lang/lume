use lume_mir_queries::MirQueryCtx;
use lume_types::TypeRef;

/// Lowers the given type reference into an MIR type.
pub(crate) fn lower_type(mcx: &MirQueryCtx, type_ref: &TypeRef) -> lume_mir::Type {
    if mcx.tcx().is_type_never(type_ref) {
        return lume_mir::Type::never();
    }

    let ty = mcx.tcx().db().type_(type_ref.instance_of).unwrap();

    match &ty.kind {
        lume_types::TypeKind::Void => lume_mir::Type::void(),
        lume_types::TypeKind::Bool => lume_mir::Type::boolean(),
        lume_types::TypeKind::Int(n) => lume_mir::Type::integer(*n, true),
        lume_types::TypeKind::UInt(n) => lume_mir::Type::integer(*n, false),
        lume_types::TypeKind::Float(n) => lume_mir::Type::float(*n),
        lume_types::TypeKind::String => lume_mir::Type::string(),
        lume_types::TypeKind::Struct => {
            if mcx.tcx().is_std_pointer(type_ref) {
                let elemental_type = lower_type(mcx, &type_ref.bound_types[0]);

                return lume_mir::Type::pointer(elemental_type);
            }

            let struct_def = mcx.tcx().hir_expect_struct(type_ref.instance_of);
            let name = format!("{:+}", struct_def.name);

            let ty_props = mcx.tcx().fields_on(type_ref.instance_of).unwrap();
            let props = ty_props
                .iter()
                .map(|prop| {
                    let field_type = mcx
                        .tcx()
                        .mk_type_ref_from(&prop.field_type, type_ref.instance_of)
                        .unwrap();

                    lower_type(mcx, &field_type)
                })
                .collect::<Vec<_>>();

            let struct_ty = lume_mir::Type::structure(name, props);

            lume_mir::Type::pointer(struct_ty)
        }
        lume_types::TypeKind::Enum => {
            let enum_ty = lume_mir::Type::union(Vec::new());

            lume_mir::Type::pointer(enum_ty)
        }
        lume_types::TypeKind::Trait => lume_mir::Type::pointer(lume_mir::Type::void()),
        lume_types::TypeKind::TypeParameter => lume_mir::Type::type_param(),
    }
}
