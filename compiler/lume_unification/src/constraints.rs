use lume_errors::Result;
use lume_hir::TypeId;
use lume_span::NodeId;
use lume_types::TypeRef;

use crate::{TypeVariableId, UnificationPass, is_type_contained_within, verify};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Constraint {
    Equal { lhs: TypeRef, rhs: TypeRef },
    Subtype { of: TypeRef, param: NodeId },
}

impl UnificationPass<'_> {
    #[tracing::instrument(level = "INFO", skip_all, err)]
    pub(crate) fn create_constraints(&mut self) -> Result<()> {
        for node in self.tcx.hir().nodes().values() {
            match node {
                lume_hir::Node::Pattern(pattern) => {
                    if let lume_hir::PatternKind::Variant(variant) = &pattern.kind {
                        self.create_type_constraints(pattern.id, &variant.name)?;
                    }
                }
                lume_hir::Node::Statement(stmt) => {
                    if let lume_hir::StatementKind::Variable(decl) = &stmt.kind
                        && let Some(declared_type) = &decl.declared_type
                    {
                        verify::verify_type_name(self.tcx, &declared_type.name, declared_type.location);
                    }
                }
                lume_hir::Node::Expression(expr) => match &expr.kind {
                    lume_hir::ExpressionKind::Cast(cast) => {
                        self.create_type_constraints(expr.id, &cast.target.name)?;
                    }
                    lume_hir::ExpressionKind::Construct(construct) => {
                        self.create_type_constraints(expr.id, &construct.path)?;
                    }
                    lume_hir::ExpressionKind::StaticCall(call) => {
                        self.create_type_constraints(expr.id, &call.name)?;
                    }
                    lume_hir::ExpressionKind::Variant(variant) => {
                        self.create_type_constraints(expr.id, &variant.name)?;
                    }
                    _ => {}
                },
                _ => {}
            }
        }

        Ok(())
    }
}

impl UnificationPass<'_> {
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn create_type_constraints(&self, expr: NodeId, path: &lume_hir::Path) -> Result<()> {
        match &path.name {
            lume_hir::PathSegment::Type { .. } => {
                let Some(type_def) = self.tcx.tdb().find_type(path) else {
                    return Ok(());
                };

                let declared_type_args = path.bound_types().len();
                let struct_definition = self.tcx.hir_expect_struct(type_def.id);

                for &type_param_id in struct_definition.type_parameters.iter().skip(declared_type_args) {
                    let type_var_id = TypeVariableId(expr, TypeId::from(type_param_id));

                    self.create_constraints_of(expr, path, type_var_id)?;
                }
            }
            lume_hir::PathSegment::Callable { .. } => {
                let Some(callable) = self.tcx.callable_with_name(path) else {
                    return Ok(());
                };

                let path_segments = path.segments();

                for (idx, callable_segment) in callable.name().segments().iter().enumerate() {
                    let declared_type_args = path_segments[idx].bound_types().len();

                    for type_param in callable_segment.bound_types().iter().skip(declared_type_args) {
                        let type_var_id = TypeVariableId(expr, type_param.id);

                        self.create_constraints_of(expr, path, type_var_id)?;
                    }
                }
            }
            lume_hir::PathSegment::Variant { .. } => {
                let parent_path = path
                    .clone()
                    .parent()
                    .expect("expected Variant path segment to have Type parent");

                let enum_def = self.tcx.enum_def_with_name(&parent_path)?;
                let declared_type_args = path.all_bound_types().len();

                for type_param in enum_def.type_parameters.iter().skip(declared_type_args) {
                    let type_var_id = TypeVariableId(expr, lume_hir::TypeId::from(*type_param));

                    self.create_constraints_of(expr, path, type_var_id)?;
                }
            }
            lume_hir::PathSegment::Namespace { .. } => {}
        }

        Ok(())
    }
}

impl UnificationPass<'_> {
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn create_constraints_of(&self, expr: NodeId, path: &lume_hir::Path, type_variable: TypeVariableId) -> Result<()> {
        self.ensure_entry_for(type_variable);

        match &path.name {
            lume_hir::PathSegment::Type { .. } => {
                let Some(type_def) = self.tcx.tdb().find_type(path) else {
                    return Ok(());
                };

                for bound_type in type_def.name.all_bound_types() {
                    if bound_type.id != type_variable.1 {
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
                    if !is_type_contained_within(type_variable.1, &param.ty) {
                        continue;
                    }

                    self.eq(type_variable, self.tcx.type_of(arg)?, param.ty.clone());
                }

                for bound_type in callable.name().all_bound_types() {
                    if bound_type.id != type_variable.1 {
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

                    if !is_type_contained_within(type_variable.1, &param_ty) {
                        continue;
                    }

                    self.eq(type_variable, arg, param_ty);
                }

                for bound_type in enum_def.name().all_bound_types() {
                    if bound_type.id != type_variable.1 {
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
