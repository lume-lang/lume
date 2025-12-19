use lume_span::Location;

use crate::TyInferCtx;

/// Verifies that all declared types within the HIR which *cannot* be inferred -
/// such as return types, variable declaration types, trait implementation
/// targets, etc. - have a fully-qualified type, with all required type
/// arguments present.
#[libftrace::traced(level = Debug)]
pub(crate) fn verify_type_names(tcx: &TyInferCtx) {
    for (id, item) in &tcx.hir.nodes {
        if !tcx.hir_is_local_node(*id) {
            continue;
        }

        match item {
            lume_hir::Node::Type(ty) => match ty {
                lume_hir::TypeDefinition::Struct(struct_def) => {
                    for field in &struct_def.fields {
                        verify_type_name(tcx, &field.field_type.name, field.field_type.location);
                    }
                }
                lume_hir::TypeDefinition::Trait(trait_def) => {
                    for method in &trait_def.methods {
                        for param in &method.parameters {
                            verify_type_name(tcx, &param.param_type.name, param.param_type.location);
                        }

                        verify_type_name(tcx, &method.return_type.name, method.return_type.location);
                    }
                }
                lume_hir::TypeDefinition::Enum(enum_def) => {
                    for case in &enum_def.cases {
                        for param in &case.parameters {
                            verify_type_name(tcx, &param.name, param.location);
                        }
                    }
                }
                lume_hir::TypeDefinition::TypeParameter(_) => {}
            },
            lume_hir::Node::Impl(impl_block) => {
                verify_type_name(tcx, &impl_block.target.name, impl_block.target.location);

                for method in &impl_block.methods {
                    for param in &method.parameters {
                        verify_type_name(tcx, &param.param_type.name, param.param_type.location);
                    }

                    verify_type_name(tcx, &method.return_type.name, method.return_type.location);
                }
            }
            lume_hir::Node::TraitImpl(trait_impl) => {
                verify_type_name(tcx, &trait_impl.name.name, trait_impl.name.location);
                verify_type_name(tcx, &trait_impl.target.name, trait_impl.target.location);

                for method in &trait_impl.methods {
                    for param in &method.parameters {
                        verify_type_name(tcx, &param.param_type.name, param.param_type.location);
                    }

                    verify_type_name(tcx, &method.return_type.name, method.return_type.location);
                }
            }
            lume_hir::Node::Function(func) => {
                for param in &func.parameters {
                    verify_type_name(tcx, &param.param_type.name, param.location);
                }

                verify_type_name(tcx, &func.return_type.name, func.return_type.location);
            }
            _ => {}
        }
    }
}

pub(crate) fn verify_type_name(tcx: &TyInferCtx, path: &lume_hir::Path, location: Location) {
    let mut type_path = path.clone();

    // We need the *type* name, so we strip back the path until we have the actual
    // type of the path.
    while !type_path.is_type() {
        if let Some(parent) = type_path.parent() {
            type_path = parent;
        } else {
            return;
        }
    }

    let Some(matching_type) = tcx.tdb().find_type(&type_path) else {
        return;
    };

    let expected_arg_count = tcx.type_params_of(matching_type.id).unwrap_or(&[]).len();
    let declared_arg_count = type_path.bound_types().len();

    if expected_arg_count != declared_arg_count {
        tcx.dcx().emit(
            crate::unify::diagnostics::TypeArgumentCountMismatch {
                location,
                type_name: path.clone(),
                expected: expected_arg_count,
                actual: declared_arg_count,
            }
            .into(),
        );
    }
}
