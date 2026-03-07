use std::collections::HashSet;

use indexmap::IndexMap;
use lume_errors::{Diagnostic, Result};
use lume_span::Location;
use lume_types::{NamedTypeRef, TypeRef};

use crate::constraints::Constraint;
use crate::{TypeVariable, TypeVariableId, UnificationPass, normalize_equality_constraints};

impl UnificationPass<'_> {
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub(crate) fn create_type_substitutions(&mut self) -> Result<()> {
        let mut removals = HashSet::new();
        let mut substitution_map = IndexMap::<TypeVariableId, TypeRef>::new();
        let tcx_constraints = &self.env.try_read().unwrap().type_vars;

        'type_var: for (&type_variable_id, type_variable) in tcx_constraints {
            let mut constraints = type_variable.constraints.clone();

            let eq_constraints = constraints
                .extract_if(.., |c| matches!(c, Constraint::Equal { .. }))
                .collect::<Vec<_>>();

            if eq_constraints.is_empty() {
                let type_var_hir = self.tcx.hir().expect_type_variable(type_variable_id.0.as_node_id())?;
                let type_param = self.tcx.hir_expect_type_parameter(type_var_hir.binding.as_node_id());

                self.tcx.dcx().emit(
                    TypeArgumentInferenceFailed {
                        source: self.tcx.hir_span_of_node(type_variable_id.0.as_node_id()),
                        type_param_name: type_param.name.to_string(),
                    }
                    .into(),
                );

                removals.insert(type_variable_id);
                continue;
            }

            let expected_type = normalize_equality_constraints(self.tcx, type_variable_id, eq_constraints)?;

            for constraint in constraints {
                match constraint {
                    Constraint::Equal { .. } => unreachable!(),
                    Constraint::Subtype { of, param } => {
                        debug_assert!(
                            self.tcx.is_trait(&of)?,
                            "expected subtype-constraint to reference trait"
                        );

                        if !self.tcx.trait_impl_by(&of, &expected_type)? {
                            let type_param = self.tcx.hir_expect_type_parameter(param);
                            let type_param_constraint = type_param
                                .constraints
                                .iter()
                                .find(|c| c.id.as_node_id() == of.instance_of)
                                .unwrap();

                            self.tcx.dcx().emit(
                                TypeParameterConstraintUnsatisfied {
                                    source: self.tcx.hir_span_of_node(type_variable_id.0.as_node_id()),
                                    constraint_loc: type_param_constraint.location,
                                    param_name: type_param.name.to_string(),
                                    type_name: self.tcx.new_named_type(&expected_type, true)?,
                                    constraint_name: self.tcx.new_named_type(&of, true)?,
                                }
                                .into(),
                            );

                            continue 'type_var;
                        }
                    }
                }
            }

            substitution_map.insert(type_variable_id, expected_type.clone());
        }

        if !removals.is_empty() {
            let tcx_constraints = &mut self.env.try_write().unwrap().type_vars;
            for removal in removals {
                tcx_constraints.shift_remove(&removal);
            }
        }

        for (type_variable, substitution) in substitution_map {
            self.subst(type_variable, substitution);
        }

        self.tcx.dcx.ensure_untainted()?;

        Ok(())
    }
}

impl UnificationPass<'_> {
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub(crate) fn apply_substitutions(&mut self) -> Result<()> {
        let tcx_constraints = &self.env.try_read().unwrap().type_vars;

        for (&type_variable_id, TypeVariable { substitute, .. }) in tcx_constraints {
            let Some(substitute) = substitute else {
                let type_var_hir = self.tcx.hir().expect_type_variable(type_variable_id.0.as_node_id())?;
                let type_param = self.tcx.hir_expect_type_parameter(type_var_hir.binding.as_node_id());

                self.tcx.dcx().emit(
                    TypeArgumentInferenceFailed {
                        source: self.tcx.hir_span_of_node(type_variable_id.0.as_node_id()),
                        type_param_name: type_param.name.to_string(),
                    }
                    .into(),
                );

                continue;
            };

            let Some(expr) = self.tcx.hir().expression(type_variable_id.0.as_node_id()) else {
                panic!("bug!: expected ID in type variable to reference Expression");
            };

            let (mut replacement_path, target_path) = match &expr.kind {
                lume_hir::ExpressionKind::Cast(expr) => {
                    let Some(type_def) = self.tcx.tdb().find_type(&expr.target.name) else {
                        return Ok(());
                    };

                    (expr.target.name.clone(), type_def.name.clone())
                }
                lume_hir::ExpressionKind::Construct(expr) => {
                    let Some(type_def) = self.tcx.tdb().find_type(&expr.path) else {
                        return Ok(());
                    };

                    (expr.path.clone(), type_def.name.clone())
                }
                lume_hir::ExpressionKind::StaticCall(call) => {
                    let Some(callable) = self.tcx.callable_with_name(&call.name) else {
                        unreachable!()
                    };

                    (call.name.clone(), callable.name().clone())
                }
                lume_hir::ExpressionKind::Variant(expr) => {
                    let parent_path = expr.name.clone().parent().unwrap();
                    let enum_def = self.tcx.enum_def_with_name(&parent_path)?;

                    let mut enum_name = enum_def.name.clone();
                    enum_name.place_bound_types(self.tcx.type_params_as_types(&enum_def.type_parameters)?);

                    let enum_case_name = lume_hir::Path::with_root(enum_name, expr.name.name.clone());

                    (expr.name.clone(), enum_case_name)
                }
                _ => unreachable!(),
            };

            for (num_segment, segment) in target_path.segments().into_iter().enumerate() {
                match segment {
                    lume_hir::PathSegment::Type { bound_types, .. }
                    | lume_hir::PathSegment::Callable { bound_types, .. } => {
                        for (num_type, bound_type) in bound_types.iter().enumerate() {
                            if bound_type.id != type_variable_id.0 {
                                continue;
                            }

                            let replacement_ty = self.tcx.hir_lift_type(substitute)?;
                            replacement_path.segments_mut()[num_segment].put_bound_type(num_type, replacement_ty);
                        }
                    }
                    _ => {}
                }
            }

            let Some(expr) = self.tcx.hir_mut().expression_mut(type_variable_id.0.as_node_id()) else {
                panic!("bug!: expected ID in type variable to reference Expression");
            };

            match &mut expr.kind {
                lume_hir::ExpressionKind::Cast(expr) => {
                    expr.target.name = replacement_path;
                }
                lume_hir::ExpressionKind::Construct(expr) => {
                    expr.path = replacement_path;
                }
                lume_hir::ExpressionKind::StaticCall(call) => {
                    call.name = replacement_path;
                }
                lume_hir::ExpressionKind::Variant(expr) => {
                    expr.name = replacement_path;
                }
                _ => unreachable!(),
            }
        }

        Ok(())
    }
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "type constraint not satisfied", code = "LM4120")]
pub(crate) struct TypeParameterConstraintUnsatisfied {
    #[label(source, "type {type_name} does not implement {constraint_name}...")]
    pub source: Location,

    #[label(source, help, "...which is required by the type parameter {param_name}")]
    pub constraint_loc: Location,

    pub param_name: String,
    pub type_name: NamedTypeRef,
    pub constraint_name: NamedTypeRef,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "could not infer type argument",
    code = "LM4139",
    help = "try specifying the type arguments explicitly"
)]
pub struct TypeArgumentInferenceFailed {
    #[label(source, "could not infer type argument {type_param_name}")]
    pub source: Location,

    pub type_param_name: String,
}
