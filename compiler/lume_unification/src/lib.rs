mod verify;

use std::collections::HashSet;
use std::sync::RwLock;

use indexmap::IndexMap;
use lume_errors::{Diagnostic, Result};
use lume_hir::TypeId;
use lume_infer::TyInferCtx;
use lume_span::{Location, NodeId};
use lume_types::{NamedTypeRef, TypeRef};

pub(crate) struct UnificationPass<'tcx> {
    tcx: &'tcx mut TyInferCtx,

    type_vars: RwLock<IndexMap<TypeVariableId, TypeVariable>>,
}

impl<'tcx> UnificationPass<'tcx> {
    pub fn new(tcx: &'tcx mut TyInferCtx) -> Self {
        UnificationPass {
            tcx,
            type_vars: RwLock::new(IndexMap::new()),
        }
    }
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct TypeVariableId(NodeId, TypeId);

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub(crate) struct TypeVariable {
    pub constraints: Vec<Constraint>,
    pub substitute: Option<TypeRef>,
}

impl UnificationPass<'_> {
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

        assert!(
            existing.is_none(),
            "bug!: replaced existing substitution of {type_variable:?}"
        );
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Constraint {
    Equal { lhs: TypeRef, rhs: TypeRef },
    Subtype { of: TypeRef, param: NodeId },
}

impl UnificationPass<'_> {
    #[libftrace::traced(level = Info, err)]
    fn create_constraints(&mut self) -> Result<()> {
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
    #[libftrace::traced(level = Trace, err)]
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
    #[libftrace::traced(level = Trace, err)]
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

impl UnificationPass<'_> {
    #[libftrace::traced(level = Trace, err)]
    fn create_type_substitutions(&mut self) -> Result<()> {
        let mut removals = HashSet::new();
        let mut substitution_map = IndexMap::<TypeVariableId, TypeRef>::new();
        let tcx_constraints = self.type_vars.try_read().unwrap();

        'type_var: for (&type_variable_id, type_variable) in tcx_constraints.iter() {
            let mut constraints = type_variable.constraints.clone();

            let eq_constraints = constraints
                .extract_if(.., |c| matches!(c, Constraint::Equal { .. }))
                .collect::<Vec<_>>();

            if eq_constraints.is_empty() {
                let type_param = self.tcx.hir_expect_type_parameter(type_variable_id.1.as_node_id());

                self.tcx.dcx().emit(
                    TypeArgumentInferenceFailed {
                        source: self.tcx.hir_span_of_node(type_variable_id.0),
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
                                    source: self.tcx.hir_span_of_node(type_variable_id.0),
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

        // Force the read-lock to drop.
        drop(tcx_constraints);

        if !removals.is_empty() {
            let mut tcx_constraints = self.type_vars.try_write().unwrap();
            for removal in removals {
                tcx_constraints.shift_remove(&removal);
            }
        }

        for (type_variable, substitution) in substitution_map {
            self.subst(type_variable, substitution);
        }

        Ok(())
    }
}

#[libftrace::traced(level = Trace, err)]
fn normalize_equality_constraints(
    tcx: &TyInferCtx,
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

        let (normalized_lhs, normalized_rhs) = normalize_constraint_types(tcx, type_variable.1, &lhs, &rhs)?;

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
                    tcx.raise_mismatched_types(expected_lhs, &normalized_lhs);
                }

                if &normalized_rhs != expected_rhs {
                    tcx.raise_mismatched_types(expected_rhs, &normalized_rhs);
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
pub(crate) fn normalize_constraint_types(
    tcx: &TyInferCtx,
    target: TypeId,
    lhs: &TypeRef,
    rhs: &TypeRef,
) -> Result<(TypeRef, TypeRef)> {
    // If either of the items in the set are the target, we send them back.
    if target == lhs.instance_of || target == rhs.instance_of {
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
            if !is_type_contained_within(target, bound_lhs) && !is_type_contained_within(target, bound_rhs) {
                continue;
            }

            return normalize_constraint_types(tcx, target, bound_lhs, bound_rhs);
        }
    }

    Err(tcx.mismatched_types(lhs, rhs))
}

pub(crate) fn is_type_contained_within(target: TypeId, ty: &TypeRef) -> bool {
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

impl UnificationPass<'_> {
    #[libftrace::traced(level = Trace, err)]
    fn apply_substitutions(&mut self) -> Result<()> {
        let tcx_constraints = self.type_vars.try_read().unwrap();

        for (&type_variable_id, TypeVariable { substitute, .. }) in tcx_constraints.iter() {
            let Some(substitute) = substitute else {
                let type_param = self.tcx.hir_expect_type_parameter(type_variable_id.1.as_node_id());

                self.tcx.dcx().emit(
                    TypeArgumentInferenceFailed {
                        source: self.tcx.hir_span_of_node(type_variable_id.0),
                        type_param_name: type_param.name.to_string(),
                    }
                    .into(),
                );

                continue;
            };

            let Some(expr) = self.tcx.hir().expression(type_variable_id.0) else {
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
                            if bound_type.id != type_variable_id.1 {
                                continue;
                            }

                            let replacement_ty = self.tcx.hir_lift_type(substitute)?;
                            replacement_path.segments_mut()[num_segment].put_bound_type(num_type, replacement_ty);
                        }
                    }
                    _ => {}
                }
            }

            let Some(expr) = self.tcx.hir_mut().expression_mut(type_variable_id.0) else {
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

/// Performs unification on the given type inference context.
///
/// # Errors
///
/// If any errors are raised during unification, this method will either:
/// - return early with the error wrapped in a `Result::Err`,
/// - or raise the inside error inside the diagnostics context in `tcx`.
#[libftrace::traced(level = Info, err)]
pub fn unify(tcx: &mut TyInferCtx) -> Result<()> {
    verify::verify_type_names(tcx);

    let mut ucx = UnificationPass::new(tcx);

    ucx.create_constraints()?;
    ucx.create_type_substitutions()?;
    ucx.apply_substitutions()?;

    // We need to invalidate the global cache for method calls, since the
    // unification pass has altered some items in the HIR, making those
    // entries in the cache incorrect and/or invalid.
    //
    // Very few method calls would've been cached at this point in the compile
    // process, so we can safetly clear the entire thing, without having
    // to worry too much about the potential performance loss.
    invalidate_type_cache(tcx);

    tcx.dcx().ensure_untainted()
}

/// Invalidate the type cache.
#[libftrace::traced(level = Debug)]
fn invalidate_type_cache(tcx: &mut TyInferCtx) {
    let ctx: &lume_session::GlobalCtx = &*tcx;
    lume_architect::DatabaseContext::db(ctx).clear_all();
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

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "could not infer type argument on callable",
    code = "LM4145",
    help = "try specifying the type arguments explicitly"
)]
pub struct TypeArgumentInferenceFailedCallable {
    #[label(
        source,
        "could not infer type argument {type_param_name} on callable {callable_name}"
    )]
    pub source: Location,

    pub type_param_name: String,
    pub callable_name: String,
}
