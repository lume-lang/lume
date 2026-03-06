use lume_errors::Result;
use lume_span::NodeId;
use lume_types::TypeRef;

use crate::{TypeVariableId, UnificationPass, is_type_contained_within};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Constraint {
    Equal { lhs: TypeRef, rhs: TypeRef },
    Subtype { of: TypeRef, param: NodeId },
}

impl UnificationPass<'_> {
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn create_constraints_of(&self, expr: NodeId, path: &lume_hir::Path, type_variable: TypeVariableId) -> Result<()> {
        self.ensure_entry_for(type_variable);

        let type_parameter_id = self
            .tcx
            .hir()
            .expect_type_variable(type_variable.0.as_node_id())?
            .binding;

        match &path.name {
            lume_hir::PathSegment::Type { .. } => {
                let Some(type_def) = self.tcx.tdb().find_type(path) else {
                    return Ok(());
                };

                for bound_type in type_def.name.all_bound_types() {
                    if bound_type.id != type_parameter_id {
                        continue;
                    }

                    let type_param_id = bound_type.id.as_node_id();
                    let type_param = self.tcx.hir_expect_type_parameter(type_param_id);

                    for type_param_constraint in &type_param.constraints {
                        let constraint_type = self.tcx.mk_type_ref_from(type_param_constraint, type_param.id)?;

                        self.sub(type_variable, constraint_type, type_param.id);
                    }
                }
            }
            lume_hir::PathSegment::Callable { .. } => {
                let Some(callable) = self.tcx.callable_with_name(path) else {
                    return Ok(());
                };

                let Some(call_expr) = self.tcx.hir_call_expr(expr) else {
                    return Ok(());
                };

                let args = call_expr.arguments();
                let params = self.tcx.signature_of(callable)?.params;

                for (param, arg) in params.into_iter().zip(args) {
                    if !is_type_contained_within(type_parameter_id, &param.ty) {
                        continue;
                    }

                    self.eq(type_variable, self.tcx.type_of(arg)?, param.ty.clone());
                }

                for bound_type in callable.name().all_bound_types() {
                    if bound_type.id != type_parameter_id {
                        continue;
                    }

                    let type_param_id = bound_type.id.as_node_id();
                    let type_param = self.tcx.hir_expect_type_parameter(type_param_id);

                    for type_param_constraint in &type_param.constraints {
                        let constraint_type = self.tcx.mk_type_ref_from(type_param_constraint, type_param.id)?;

                        self.sub(type_variable, constraint_type, type_param.id);
                    }
                }
            }
            lume_hir::PathSegment::Variant { .. } => {
                let parent_path = path
                    .clone()
                    .parent()
                    .expect("expected Variant path segment to have Type parent");

                let enum_def = self.tcx.enum_def_with_name(&parent_path)?;
                let enum_case_def = self.tcx.enum_case_with_name(path)?;

                let arguments = match &self.tcx.hir_expect_node(expr) {
                    lume_hir::Node::Expression(expr) => {
                        if let lume_hir::ExpressionKind::Variant(variant) = &expr.kind {
                            let argument_ids = &variant.arguments;

                            argument_ids
                                .iter()
                                .map(|id| self.tcx.type_of(*id))
                                .collect::<Result<Vec<_>>>()?
                        } else {
                            return Ok(());
                        }
                    }
                    lume_hir::Node::Pattern(pattern) => {
                        if let lume_hir::PatternKind::Variant(pattern) = &pattern.kind {
                            let field_ids = &pattern.fields;

                            field_ids
                                .iter()
                                .map(|subpattern| self.tcx.type_of_pattern(subpattern))
                                .collect::<Result<Vec<_>>>()?
                        } else {
                            return Ok(());
                        }
                    }
                    _ => return Ok(()),
                };

                let params = &enum_case_def.parameters;

                for (param, arg) in params.iter().zip(arguments) {
                    let param_ty = self.tcx.mk_type_ref_from(param, enum_def.id)?;

                    if !is_type_contained_within(type_parameter_id, &param_ty) {
                        continue;
                    }

                    self.eq(type_variable, arg, param_ty);
                }

                for bound_type in enum_def.name().all_bound_types() {
                    if bound_type.id != type_parameter_id {
                        continue;
                    }

                    let type_param_id = bound_type.id.as_node_id();
                    let type_param = self.tcx.hir_expect_type_parameter(type_param_id);

                    for type_param_constraint in &type_param.constraints {
                        let constraint_type = self.tcx.mk_type_ref_from(type_param_constraint, type_param.id)?;

                        self.sub(type_variable, constraint_type, type_param.id);
                    }
                }
            }
            lume_hir::PathSegment::Namespace { .. } => {}
        }

        if let Some(lume_hir::Node::Pattern(pattern)) = self.tcx.hir_node(expr)
            && let Some(expected_type) = self.tcx.expected_type_of(expr)?
        {
            self.eq(type_variable, self.tcx.type_of_pattern(pattern)?, expected_type);
        }

        if self.tcx.hir_expr(expr).is_some()
            && !path.is_variant()
            && let Some(expected_type) = self.tcx.expected_type_of(expr)?
        {
            self.eq(type_variable, self.tcx.type_of(expr)?, expected_type);
        }

        Ok(())
    }
}
