use lume_errors::Result;
use lume_infer::TyInferCtx;

use crate::engine::{Context, Engine, TypeVar};
use crate::introduce::InferedNode;

impl Engine<'_, TyInferCtx> {
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub(crate) fn apply_substitutions(&mut self) -> Result<()> {
        fn replace_in(
            bound_types: &mut [lume_hir::Type],
            type_var_id: TypeVar<TyInferCtx>,
            replacement: lume_hir::Type,
        ) {
            let idx = bound_types
                .iter()
                .position(|ty| ty.id == type_var_id.0)
                .expect("expected type variable in node");

            bound_types[idx] = replacement;
        }

        let env = self.env.try_read().unwrap();

        for (node_id, type_var_id) in self.affected_nodes() {
            let type_parameter_binding = self.ctx.hir_tyvar_binding_of(type_var_id.0).unwrap();
            let substitute = env
                .substitute_of(type_var_id)
                .expect("expected type variables to be resolved");

            let replacement_ty = self.ctx.hir_lift_type(&substitute)?;

            tracing::trace!(
                type_variable = %type_var_id,
                substitute = %self.ctx.name_of_type(&substitute).unwrap_or(String::from("<unknown>")),
                "replaced_type_var"
            );

            let mut node = if let Some(node) = self.ctx.hir_node(node_id)
                && let Ok(inferred) = InferedNode::try_from(node)
            {
                inferred
            } else {
                continue;
            };

            match &mut node {
                InferedNode::Cast(expr) => {
                    if let lume_hir::PathSegment::Type { bound_types, .. } = &mut expr.target.name.name {
                        replace_in(bound_types, type_var_id, replacement_ty);
                    }
                }
                InferedNode::Construct(expr) => {
                    if let lume_hir::PathSegment::Type { bound_types, .. } = &mut expr.path.name {
                        replace_in(bound_types, type_var_id, replacement_ty);
                    }
                }
                InferedNode::VariableDeclaration(_decl) => continue,
                InferedNode::IntrinsicCall(expr) => {
                    replace_in(&mut expr.bound_types, type_var_id, replacement_ty);
                }
                InferedNode::InstanceCall(call) => {
                    let callable = self.ctx.probe_callable_instance(call)?;
                    let is_bound_to_method = self
                        .ctx
                        .is_bound_to_method(type_parameter_binding.as_node_id(), callable)?;

                    // Push the type replacement onto the type path segment or callable path
                    // segment, depending on which one the type parameter is
                    // bound to.
                    if is_bound_to_method {
                        if let lume_hir::PathSegment::Callable { bound_types, .. } = &mut call.name {
                            replace_in(bound_types, type_var_id, replacement_ty);
                        }
                    } else {
                        replace_in(&mut call.bound_types, type_var_id, replacement_ty);
                    }
                }
                InferedNode::StaticCall(call) => {
                    let callable = self.ctx.probe_callable_static(call)?;
                    let is_bound_to_method = self
                        .ctx
                        .is_bound_to_method(type_parameter_binding.as_node_id(), callable)?;

                    // Push the type replacement onto the type path segment or callable path
                    // segment, depending on which one the type parameter is
                    // bound to.
                    if is_bound_to_method
                        && let lume_hir::PathSegment::Callable { bound_types, .. } = &mut call.name.name
                    {
                        replace_in(bound_types, type_var_id, replacement_ty);
                    } else if let Some(lume_hir::PathSegment::Type { bound_types, .. }) = &mut call.name.root.last_mut()
                    {
                        replace_in(bound_types, type_var_id, replacement_ty);
                    }
                }
                InferedNode::Variant(expr) => {
                    if let Some(lume_hir::PathSegment::Type { bound_types, .. }) = &mut expr.name.root.last_mut() {
                        replace_in(bound_types, type_var_id, replacement_ty);
                    }
                }
                InferedNode::Pattern(pattern) => {
                    if let lume_hir::PatternKind::Variant(variant) = &mut pattern.kind
                        && let Some(lume_hir::PathSegment::Type { bound_types, .. }) = &mut variant.name.root.last_mut()
                    {
                        replace_in(bound_types, type_var_id, replacement_ty);
                    }
                }
            }

            self.ctx
                .hir_mut()
                .nodes
                .insert(node_id, Into::<lume_hir::Node>::into(node));

            // Remove the temporary type variable node from the HIR and type database.
            //
            // This is to prevent problems in later stages of the compiler, which aren't
            // meant to handle them.
            self.ctx.hir_mut().nodes.swap_remove(&type_var_id.0);
            self.ctx.tdb_mut().types.swap_remove(&type_var_id.0);
        }

        Ok(())
    }
}
