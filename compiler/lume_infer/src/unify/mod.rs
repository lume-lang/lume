use indexmap::IndexMap;
use lume_errors::Result;
use lume_hir::TypeId;
use lume_span::NodeId;
use lume_types::TypeRef;

use crate::TyInferCtx;

mod diagnostics;
pub mod verify;

pub(crate) use verify::verify_type_names;

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct TypeVariableId(NodeId, TypeId);

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub(crate) struct TypeVariable {
    pub constraints: Vec<Constraint>,
    pub substitute: Option<TypeRef>,
}

impl TyInferCtx {
    /// Ensure there is an entry for constraints for the given type variable.
    ///
    /// So, even if no constraints could be generated, we would still notice
    /// and empty constraint list and throw an error.
    #[libftrace::traced(level = Trace)]
    fn ensure_entry_for(&self, type_variable: TypeVariableId) {
        self.type_vars.try_write().unwrap().entry(type_variable).or_default();
    }

    #[libftrace::traced(level = Trace)]
    pub(crate) fn eq(&self, type_variable: TypeVariableId, lhs: TypeRef, rhs: TypeRef) {
        if lhs == rhs {
            return;
        }

        let mut type_vars = self.type_vars.try_write().unwrap();

        type_vars
            .entry(type_variable)
            .or_default()
            .constraints
            .push(Constraint::Equal { lhs, rhs });
    }

    #[libftrace::traced(level = Trace)]
    pub(crate) fn sub(&self, type_variable: TypeVariableId, of: TypeRef, param: NodeId) {
        let mut type_vars = self.type_vars.try_write().unwrap();

        type_vars
            .entry(type_variable)
            .or_default()
            .constraints
            .push(Constraint::Subtype { of, param });
    }

    #[libftrace::traced(level = Trace)]
    pub(crate) fn subst(&self, type_variable: TypeVariableId, with: TypeRef) {
        let mut type_vars = self.type_vars.try_write().unwrap();
        let existing = type_vars.entry(type_variable).or_default().substitute.replace(with);

        if existing.is_some() {
            panic!("bug!: replaced existing substitution of {type_variable:?}");
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Constraint {
    Equal { lhs: TypeRef, rhs: TypeRef },
    Subtype { of: TypeRef, param: NodeId },
}

impl TyInferCtx {
    #[libftrace::traced(level = Info, err)]
    pub(crate) fn unify_ctx(&mut self) -> Result<()> {
        for node in self.hir.nodes().values() {
            match node {
                lume_hir::Node::Pattern(pattern) => match &pattern.kind {
                    lume_hir::PatternKind::Variant(variant) => {
                        self.create_type_constraints(pattern.id, &variant.name)?;
                    }
                    _ => {}
                },
                lume_hir::Node::Statement(stmt) => match &stmt.kind {
                    lume_hir::StatementKind::Variable(decl) => {
                        if let Some(declared_type) = &decl.declared_type {
                            verify::verify_type_name(self, &declared_type.name)?;
                        }
                    }
                    _ => {}
                },
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

        self.create_type_substitutions()?;
        self.apply_substitutions()?;

        Ok(())
    }
}

impl TyInferCtx {
    #[libftrace::traced(level = Trace, err)]
    fn create_type_constraints<'tcx>(&self, expr: NodeId, path: &lume_hir::Path) -> Result<()> {
        match &path.name {
            lume_hir::PathSegment::Type { .. } => {
                let Some(type_def) = self.tdb().find_type(path) else {
                    return Ok(());
                };

                let declared_type_args = path.bound_types().len();

                for type_param in type_def.name.bound_types().iter().skip(declared_type_args) {
                    let type_var_id = TypeVariableId(expr, type_param.id);

                    self.create_constraints_of(expr, path, type_var_id)?;
                }
            }
            lume_hir::PathSegment::Callable { .. } => {
                let Some(callable) = self.callable_with_name(path) else {
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

                let enum_def = self.enum_def_of_name(&parent_path)?;
                let declared_type_args = path.all_bound_types().len();

                for type_param in enum_def.type_parameters.iter().skip(declared_type_args) {
                    let type_var_id = TypeVariableId(expr, lume_hir::TypeId::from(type_param.id));

                    self.create_constraints_of(expr, path, type_var_id)?;
                }
            }
            lume_hir::PathSegment::Namespace { .. } => {}
        }

        Ok(())
    }
}

impl TyInferCtx {
    #[libftrace::traced(level = Trace, err)]
    fn create_constraints_of<'tcx>(
        &self,
        expr: NodeId,
        path: &lume_hir::Path,
        type_variable: TypeVariableId,
    ) -> Result<()> {
        self.ensure_entry_for(type_variable);

        match &path.name {
            lume_hir::PathSegment::Type { .. } => {
                let Some(type_def) = self.tdb().find_type(path) else {
                    return Ok(());
                };

                for bound_type in type_def.name.all_bound_types() {
                    if bound_type.id != type_variable.1 {
                        continue;
                    }

                    let type_param = self.tdb().type_parameter(bound_type.id.as_node_id()).unwrap();

                    for type_param_constraint in &type_param.constraints {
                        self.sub(type_variable, type_param_constraint.clone(), type_param.id);
                    }
                }
            }
            lume_hir::PathSegment::Callable { .. } => {
                let Some(callable) = self.callable_with_name(path) else {
                    return Ok(());
                };

                let Some(call_expr) = self.hir_call_expr(expr) else {
                    return Ok(());
                };

                let args = call_expr.arguments();
                let params = callable.signature().params.inner();

                for (param, arg) in params.into_iter().zip(args) {
                    if !is_type_contained_within(type_variable.1.as_node_id(), &param.ty) {
                        continue;
                    }

                    self.eq(type_variable, self.type_of(arg)?, param.ty.clone());
                }

                for bound_type in callable.name().all_bound_types() {
                    if bound_type.id != type_variable.1 {
                        continue;
                    }

                    let type_param = self.tdb().type_parameter(bound_type.id.as_node_id()).unwrap();

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

                let enum_def = self.enum_def_of_name(&parent_path)?;
                let enum_case_def = self.enum_case_with_name(path)?;

                let arguments = match &self.hir_expect_node(expr) {
                    lume_hir::Node::Expression(expr) => {
                        if let lume_hir::ExpressionKind::Variant(variant) = &expr.kind {
                            let argument_ids = &variant.arguments;

                            argument_ids
                                .iter()
                                .map(|id| self.type_of(*id))
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
                                .map(|subpattern| self.type_of_pattern(subpattern))
                                .collect::<Result<Vec<_>>>()?
                        } else {
                            return Ok(());
                        }
                    }
                    _ => return Ok(()),
                };

                let params = &enum_case_def.parameters;

                for (param, arg) in params.into_iter().zip(arguments) {
                    let param_ty = self.mk_type_ref_from(param, enum_def.id)?;

                    if !is_type_contained_within(type_variable.1.as_node_id(), &param_ty) {
                        continue;
                    }

                    self.eq(type_variable, arg, param_ty);
                }

                for bound_type in enum_def.name().all_bound_types() {
                    if bound_type.id != type_variable.1 {
                        continue;
                    }

                    let type_param = self.tdb().type_parameter(bound_type.id.as_node_id()).unwrap();

                    for type_param_constraint in &type_param.constraints {
                        self.sub(type_variable, type_param_constraint.clone(), type_param.id);
                    }
                }
            }
            lume_hir::PathSegment::Namespace { .. } => {}
        }

        if self.hir_expr(expr).is_some()
            && !path.is_variant()
            && let Some(expected_type) = self.expected_type_of(expr)?
        {
            self.eq(type_variable, self.type_of(expr)?, expected_type);
        };

        Ok(())
    }
}

impl TyInferCtx {
    #[libftrace::traced(level = Trace, err)]
    fn create_type_substitutions(&self) -> Result<()> {
        let mut substitution_map = IndexMap::<TypeVariableId, TypeRef>::new();
        let tcx_constraints = self.type_vars.try_read().unwrap();

        'type_var: for (&type_variable_id, type_variable) in tcx_constraints.iter() {
            let mut constraints = type_variable.constraints.clone();

            let eq_constraints = constraints
                .extract_if(.., |c| matches!(c, Constraint::Equal { .. }))
                .collect::<Vec<_>>();

            if eq_constraints.is_empty() {
                let type_param = self.tdb().type_parameter(type_variable_id.1.as_node_id()).unwrap();

                self.dcx().emit(
                    crate::errors::TypeArgumentInferenceFailed {
                        source: self.hir_span_of_node(type_variable_id.0),
                        type_param_name: type_param.name.clone(),
                    }
                    .into(),
                );

                continue;
            }

            let expected_type = normalize_equality_constraints(self, type_variable_id, eq_constraints)?;

            for constraint in constraints {
                match constraint {
                    Constraint::Equal { .. } => unreachable!(),
                    Constraint::Subtype { of, param } => {
                        debug_assert!(self.is_trait(&of)?, "expected subtype-constraint to reference trait");

                        if !self.trait_impl_by(&of, &expected_type)? {
                            let type_param = self.tdb().type_parameter(param).unwrap();
                            let type_param_constraint = type_param.constraints.iter().find(|c| *c == &of).unwrap();

                            self.dcx().emit(
                                crate::errors::TypeParameterConstraintUnsatisfied {
                                    source: self.hir_span_of_node(type_variable_id.0),
                                    constraint_loc: type_param_constraint.location,
                                    param_name: type_param.name.clone(),
                                    type_name: self.new_named_type(&expected_type, true)?,
                                    constraint_name: self.new_named_type(&of, true)?,
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

        // Force the read-lock to drop.
        drop(tcx_constraints);

        for (type_variable, substitution) in substitution_map {
            self.subst(type_variable, substitution);
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

impl TyInferCtx {
    #[libftrace::traced(level = Trace, err)]
    fn apply_substitutions(&mut self) -> Result<()> {
        let tcx_constraints = self.type_vars.try_read().unwrap();

        for (&type_variable_id, TypeVariable { substitute, .. }) in tcx_constraints.iter() {
            let Some(substitute) = substitute else {
                let type_param = self
                    .tdb()
                    .type_parameter(type_variable_id.1.as_node_id())
                    .expect("expected type variable to reference valid type parameter");

                self.dcx().emit(
                    crate::errors::TypeArgumentInferenceFailed {
                        source: self.hir_span_of_node(type_variable_id.0),
                        type_param_name: type_param.name.clone(),
                    }
                    .into(),
                );

                continue;
            };

            let Some(expr) = self.hir.expression(type_variable_id.0) else {
                panic!("bug!: expected ID in type variable to reference Expression");
            };

            let (mut replacement_path, target_path) = match &expr.kind {
                lume_hir::ExpressionKind::Cast(expr) => {
                    let Some(type_def) = self.tdb().find_type(&expr.target.name) else {
                        return Ok(());
                    };

                    (expr.target.name.clone(), type_def.name.clone())
                }
                lume_hir::ExpressionKind::Construct(expr) => {
                    let Some(type_def) = self.tdb().find_type(&expr.path) else {
                        return Ok(());
                    };

                    (expr.path.clone(), type_def.name.clone())
                }
                lume_hir::ExpressionKind::StaticCall(call) => {
                    let Some(callable) = self.callable_with_name(&call.name) else {
                        unreachable!()
                    };

                    (call.name.clone(), callable.name().clone())
                }
                lume_hir::ExpressionKind::Variant(expr) => {
                    let parent_path = expr.name.clone().parent().unwrap();
                    let enum_def = self.enum_def_of_name(&parent_path)?;

                    let mut enum_name = enum_def.name.clone();
                    enum_name.place_bound_types(enum_def.type_parameters.as_types());

                    let enum_case_name = lume_hir::Path::with_root(enum_name, expr.name.name.clone());

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

                            let replacement_ty = self.hir_lift_type(substitute)?;
                            replacement_path.segments_mut()[num_segment].put_bound_type(num_type, replacement_ty);
                        }
                    }
                    _ => {}
                }
            }

            let Some(expr) = self.hir.expression_mut(type_variable_id.0) else {
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
