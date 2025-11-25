use indexmap::IndexMap;
use lume_errors::Result;
use lume_hir::TypeId;
use lume_span::NodeId;
use lume_types::TypeRef;

use crate::TyInferCtx;

mod diagnostics;
pub mod verify;

pub(crate) use verify::verify_type_names;

#[derive(Default)]
pub(crate) struct UnificationPass {
    constraints: IndexMap<TypeVariableId, Vec<Constraint>>,
    substitution_map: IndexMap<TypeVariableId, TypeRef>,
}

impl UnificationPass {
    #[libftrace::traced(level = Info, err)]
    pub(crate) fn invoke<'tcx>(mut self, tcx: &'tcx mut TyInferCtx) -> Result<()> {
        for node in tcx.hir.nodes().values() {
            match node {
                lume_hir::Node::Pattern(pattern) => match &pattern.kind {
                    _ => {}
                },
                lume_hir::Node::Statement(stmt) => match &stmt.kind {
                    lume_hir::StatementKind::Variable(decl) => {
                        if let Some(declared_type) = &decl.declared_type {
                            verify::verify_type_name(tcx, &declared_type.name)?;
                        }
                    }
                    _ => {}
                },
                lume_hir::Node::Expression(expr) => match &expr.kind {
                    lume_hir::ExpressionKind::Cast(cast) => {
                        self.create_type_constraints(tcx, expr.id, &cast.target.name)?;
                    }
                    lume_hir::ExpressionKind::Construct(construct) => {
                        self.create_type_constraints(tcx, expr.id, &construct.path)?;
                    }
                    lume_hir::ExpressionKind::StaticCall(call) => {
                        self.create_type_constraints(tcx, expr.id, &call.name)?;
                    }
                    lume_hir::ExpressionKind::Variant(variant) => {
                        self.create_type_constraints(tcx, expr.id, &variant.name)?;
                    }
                    _ => {}
                },
                _ => {}
            }
        }

        self.update_substitution_map(tcx)?;
        self.apply_substitutions(tcx)?;

        Ok(())
    }
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
struct TypeVariableId(NodeId, TypeId);

impl UnificationPass {
    #[libftrace::traced(level = Trace, err)]
    fn create_type_constraints<'tcx>(
        &mut self,
        tcx: &'tcx TyInferCtx,
        expr: NodeId,
        path: &lume_hir::Path,
    ) -> Result<()> {
        match &path.name {
            lume_hir::PathSegment::Type { .. } => {
                let Some(type_def) = tcx.tdb().find_type(path) else {
                    return Ok(());
                };

                let declared_type_args = path.bound_types().len();

                for type_param in type_def.name.bound_types().iter().skip(declared_type_args) {
                    let type_var_id = TypeVariableId(expr, type_param.id);

                    self.create_constraints_of(tcx, expr, path, type_var_id)?;
                }
            }
            lume_hir::PathSegment::Callable { .. } => {
                let Some(callable) = tcx.callable_with_name(path) else {
                    return Ok(());
                };

                let path_segments = path.segments();

                for (idx, callable_segment) in callable.name().segments().iter().enumerate() {
                    let declared_type_args = path_segments[idx].bound_types().len();

                    for type_param in callable_segment.bound_types().iter().skip(declared_type_args) {
                        let type_var_id = TypeVariableId(expr, type_param.id);

                        self.create_constraints_of(tcx, expr, path, type_var_id)?;
                    }
                }
            }
            lume_hir::PathSegment::Variant { .. } => {
                let parent_path = path
                    .clone()
                    .parent()
                    .expect("expected Variant path segment to have Type parent");

                let enum_def = tcx.enum_def_of_name(&parent_path)?;
                let declared_type_args = path.all_bound_types().len();

                for type_param in enum_def.type_parameters.iter().skip(declared_type_args) {
                    let type_var_id = TypeVariableId(expr, lume_hir::TypeId::from(type_param.id));

                    self.create_constraints_of(tcx, expr, path, type_var_id)?;
                }
            }
            lume_hir::PathSegment::Namespace { .. } => {}
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Constraint {
    Equal { lhs: TypeRef, rhs: TypeRef },
    Subtype { of: TypeRef, param: NodeId },
}

impl UnificationPass {
    #[libftrace::traced(level = Trace, err)]
    fn create_constraints_of<'tcx>(
        &mut self,
        tcx: &'tcx TyInferCtx,
        expr: NodeId,
        path: &lume_hir::Path,
        type_variable: TypeVariableId,
    ) -> Result<()> {
        // Ensure there is an entry for constraints for the given type variable. So,
        // even if no constraints could be generated, we would still notice and
        // empty constraint list and throw an error.
        self.constraints.entry(type_variable).or_default();

        match &path.name {
            lume_hir::PathSegment::Type { .. } => {
                let Some(type_def) = tcx.tdb().find_type(path) else {
                    return Ok(());
                };

                for bound_type in type_def.name.all_bound_types() {
                    if bound_type.id != type_variable.1 {
                        continue;
                    }

                    let type_param = tcx.tdb().type_parameter(bound_type.id.as_node_id()).unwrap();

                    for type_param_constraint in &type_param.constraints {
                        self.sub(type_variable, type_param_constraint.clone(), type_param.id);
                    }
                }
            }
            lume_hir::PathSegment::Callable { .. } => {
                let Some(callable) = tcx.callable_with_name(path) else {
                    return Ok(());
                };

                let Some(call_expr) = tcx.hir_call_expr(expr) else {
                    return Ok(());
                };

                let args = call_expr.arguments();
                let params = callable.signature().params.inner();

                for (param, arg) in params.into_iter().zip(args) {
                    if !is_type_contained_within(type_variable.1.as_node_id(), &param.ty) {
                        continue;
                    }

                    self.eq(type_variable, tcx.type_of(arg)?, param.ty.clone());
                }

                for bound_type in callable.name().all_bound_types() {
                    if bound_type.id != type_variable.1 {
                        continue;
                    }

                    let type_param = tcx.tdb().type_parameter(bound_type.id.as_node_id()).unwrap();

                    for type_param_constraint in &type_param.constraints {
                        self.sub(type_variable, type_param_constraint.clone(), type_param.id);
                    }
                }
            }
            lume_hir::PathSegment::Variant { .. } => {
                let parent_path = path
                    .clone()
                    .parent()
                    .expect("expected Variant path segment to have Type parent");

                let enum_def = tcx.enum_def_of_name(&parent_path)?;
                let enum_case_def = tcx.enum_case_with_name(path)?;

                let lume_hir::ExpressionKind::Variant(variant) = &tcx.hir_expect_expr(expr).kind else {
                    return Ok(Vec::new());
                };

                let args = &variant.arguments;
                let params = &enum_case_def.parameters;

                for (param, arg) in params.into_iter().zip(args) {
                    let param_ty = tcx.mk_type_ref_from(param, enum_def.id)?;

                    if !is_type_contained_within(type_variable.1.as_node_id(), &param_ty) {
                        continue;
                    }

                    constraints.push(Constraint::Equal {
                        lhs: tcx.type_of(*arg)?,
                        rhs: param_ty,
                    });
                }

                for bound_type in enum_def.name().all_bound_types() {
                    if bound_type.id != type_variable.1 {
                        continue;
                    }

                    let type_param = tcx.tdb().type_parameter(bound_type.id.as_node_id()).unwrap();

                    for type_param_constraint in &type_param.constraints {
                        constraints.push(Constraint::Subtype {
                            of: type_param_constraint.clone(),
                            param: type_param.id,
                        });
                    }
                }
            }
            lume_hir::PathSegment::Namespace { .. } => {}
        }

        if let Some(expected_type) = tcx.expected_type_of(expr)? {
            constraints.push(Constraint::Equal {
                lhs: tcx.type_of(expr)?,
                rhs: expected_type,
            });
        };

        Ok(())
    }

    #[libftrace::traced(level = Trace)]
    fn eq(&mut self, type_variable: TypeVariableId, lhs: TypeRef, rhs: TypeRef) {
        if lhs == rhs {
            return;
        }

        self.constraints
            .entry(type_variable)
            .or_default()
            .push(Constraint::Equal { lhs, rhs });
    }

    #[libftrace::traced(level = Trace)]
    fn sub(&mut self, type_variable: TypeVariableId, of: TypeRef, param: NodeId) {
        self.constraints
            .entry(type_variable)
            .or_default()
            .push(Constraint::Subtype { of, param });
    }
}

impl UnificationPass {
    #[libftrace::traced(level = Trace, err)]
    fn update_substitution_map<'tcx>(&mut self, tcx: &'tcx TyInferCtx) -> Result<()> {
        'type_var: for (type_variable_id, mut constraints) in std::mem::take(&mut self.constraints) {
            let eq_constraints = constraints
                .extract_if(.., |c| matches!(c, Constraint::Equal { .. }))
                .collect::<Vec<_>>();

            if eq_constraints.is_empty() {
                let type_param = tcx.tdb().type_parameter(type_variable_id.1.as_node_id()).unwrap();

                tcx.dcx().emit(
                    crate::errors::TypeArgumentInferenceFailed {
                        source: tcx.hir_span_of_node(type_variable_id.0),
                        type_param_name: type_param.name.clone(),
                    }
                    .into(),
                );

                continue;
            }

            let expected_type = normalize_equality_constraints(tcx, type_variable_id, eq_constraints)?;

            for constraint in constraints {
                match constraint {
                    Constraint::Equal { .. } => unreachable!(),
                    Constraint::Subtype { of, param } => {
                        debug_assert!(tcx.is_trait(&of)?, "expected subtype-constraint to reference trait");

                        if !tcx.trait_impl_by(&of, &expected_type)? {
                            let type_param = tcx.tdb().type_parameter(param).unwrap();
                            let type_param_constraint = type_param.constraints.iter().find(|c| *c == &of).unwrap();

                            tcx.dcx().emit(
                                crate::errors::TypeParameterConstraintUnsatisfied {
                                    source: tcx.hir_span_of_node(type_variable_id.0),
                                    constraint_loc: type_param_constraint.location,
                                    param_name: type_param.name.clone(),
                                    type_name: tcx.new_named_type(&expected_type, true)?,
                                    constraint_name: tcx.new_named_type(&of, true)?,
                                }
                                .into(),
                            );

                            continue 'type_var;
                        }
                    }
                }
            }

            self.substitution_map.insert(type_variable_id, expected_type.clone());
        }

        Ok(())
    }
}

#[libftrace::traced(level = Trace, err)]
fn normalize_equality_constraints<'tcx>(
    tcx: &'tcx TyInferCtx,
    type_variable: TypeVariableId,
    constraints: Vec<Constraint>,
) -> Result<TypeRef> {
    debug_assert!(!constraints.is_empty(), "equality constraint list must not be empty");
    debug_assert!(constraints.iter().all(|c| matches!(c, Constraint::Equal { .. })));

    let mut normalized_types: Option<(TypeRef, TypeRef)> = None;

    for constraint in constraints {
        let Constraint::Equal { lhs, rhs } = constraint else {
            unreachable!();
        };

        let (normalized_lhs, normalized_rhs) = normalize_constraint_types(tcx, type_variable, &lhs, &rhs)?;

        match normalized_types.as_ref() {
            None => {
                // Define a "primary" set of normalized types, which will be compared against
                // if multiple equality sets exist within the constraint list.
                normalized_types = Some((normalized_lhs, normalized_rhs));
            }
            Some((expected_lhs, expected_rhs)) => {
                // If there's more than a single equality constraint, we have to check
                // whether they resolve to the same type. Otherwise, we must raise errors.

                if &normalized_lhs != expected_lhs {
                    tcx.dcx().emit(
                        crate::errors::MismatchedTypes {
                            reason_loc: expected_lhs.location,
                            found_loc: normalized_lhs.location,
                            expected: tcx.new_named_type(expected_lhs, true)?,
                            found: tcx.new_named_type(&normalized_lhs, true)?,
                        }
                        .into(),
                    );
                }

                if &normalized_rhs != expected_rhs {
                    tcx.dcx().emit(
                        crate::errors::MismatchedTypes {
                            reason_loc: expected_rhs.location,
                            found_loc: normalized_rhs.location,
                            expected: tcx.new_named_type(expected_rhs, true)?,
                            found: tcx.new_named_type(&normalized_rhs, true)?,
                        }
                        .into(),
                    );
                }
            }
        }
    }

    let type_variable_target = type_variable.1.as_node_id();
    let (normalized_lhs, normalized_rhs) = normalized_types.expect("expected constraints to be normalized and exist");

    // Return the normalized type which does *not* contain the type variable itself,
    // since that wouldn't be very useful to the type checker.
    if normalized_lhs.instance_of == type_variable_target {
        Ok(normalized_rhs)
    } else {
        Ok(normalized_lhs)
    }
}

#[libftrace::traced(level = Trace, err)]
fn normalize_constraint_types<'tcx>(
    tcx: &'tcx TyInferCtx,
    type_variable: TypeVariableId,
    lhs: &TypeRef,
    rhs: &TypeRef,
) -> Result<(TypeRef, TypeRef)> {
    let type_variable_target = type_variable.1.as_node_id();

    // If either of the items in the set are the target, we send them back.
    if type_variable_target == lhs.instance_of || type_variable_target == rhs.instance_of {
        return Ok((lhs.to_owned(), rhs.to_owned()));
    }

    // If the two types being normalized don't refer to the type parent type, we
    // cannot normalize them. For example, image a set of constraints like this:
    // ```
    // U = [Option<?T> = Option<String>]
    // ```
    // can be normalized, since they both refer to the same containing type,
    // `Option`.
    //
    // Contrarily, this example cannot be normalized since they do not
    // refer to the same containing type:
    // ```
    // U = [Array<?T> = Option<String>]
    // ```
    if lhs.instance_of == rhs.instance_of {
        for (bound_lhs, bound_rhs) in lhs.bound_types.iter().zip(rhs.bound_types.iter()) {
            if !is_type_contained_within(type_variable_target, bound_lhs)
                && !is_type_contained_within(type_variable_target, bound_rhs)
            {
                continue;
            }

            return normalize_constraint_types(tcx, type_variable, bound_lhs, bound_rhs);
        }
    }

    Err(crate::errors::MismatchedTypes {
        reason_loc: lhs.location,
        found_loc: rhs.location,
        expected: tcx.new_named_type(lhs, true)?,
        found: tcx.new_named_type(rhs, true)?,
    }
    .into())
}

fn is_type_contained_within(target: NodeId, ty: &TypeRef) -> bool {
    if target == ty.instance_of {
        return true;
    }

    for bound_type in &ty.bound_types {
        if is_type_contained_within(target, bound_type) {
            return true;
        }
    }

    false
}

impl UnificationPass {
    #[libftrace::traced(level = Trace, err)]
    fn apply_substitutions<'tcx>(&mut self, tcx: &'tcx mut TyInferCtx) -> Result<()> {
        for (type_variable_id, substitution) in std::mem::take(&mut self.substitution_map) {
            let Some(expr) = tcx.hir.expression(type_variable_id.0) else {
                panic!("bug!: expected ID in type variable to reference Expression");
            };

            let (mut replacement_path, target_path) = match &expr.kind {
                lume_hir::ExpressionKind::Cast(expr) => {
                    let Some(type_def) = tcx.tdb().find_type(&expr.target.name) else {
                        return Ok(());
                    };

                    (expr.target.name.clone(), type_def.name.clone())
                }
                lume_hir::ExpressionKind::Construct(expr) => {
                    let Some(type_def) = tcx.tdb().find_type(&expr.path) else {
                        return Ok(());
                    };

                    (expr.path.clone(), type_def.name.clone())
                }
                lume_hir::ExpressionKind::StaticCall(call) => {
                    let Some(callable) = tcx.callable_with_name(&call.name) else {
                        unreachable!()
                    };

                    (call.name.clone(), callable.name().clone())
                }
                lume_hir::ExpressionKind::Variant(expr) => {
                    let parent_path = expr.name.clone().parent().unwrap();
                    let enum_def = tcx.enum_def_of_name(&parent_path)?;
                    let enum_case_name = lume_hir::Path::with_root(enum_def.name.clone(), expr.name.name.clone());

                    (expr.name.clone(), enum_case_name)
                }
                _ => unreachable!(),
            };

            for (num_segment, segment) in target_path.segments().into_iter().enumerate() {
                match segment {
                    lume_hir::PathSegment::Type { bound_types, .. }
                    | lume_hir::PathSegment::Callable { bound_types, .. } => {
                        for (num_type, bound_type) in bound_types.into_iter().enumerate() {
                            if bound_type.id != type_variable_id.1 {
                                continue;
                            }

                            let replacement_ty = tcx.hir_lift_type(&substitution)?;
                            replacement_path.segments_mut()[num_segment].put_bound_type(num_type, replacement_ty);
                        }
                    }
                    _ => {}
                }
            }

            let Some(expr) = tcx.hir.expression_mut(type_variable_id.0) else {
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
