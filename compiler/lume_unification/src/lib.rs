mod constraints;
mod introduce;
mod subst;
mod verify;

use std::fmt::Display;
use std::sync::RwLock;

use indexmap::IndexMap;
use lume_errors::Result;
use lume_hir::TypeId;
use lume_infer::TyInferCtx;
use lume_span::NodeId;
use lume_types::TypeRef;

use crate::constraints::Constraint;

pub(crate) struct UnificationPass<'tcx> {
    tcx: &'tcx mut TyInferCtx,
    env: RwLock<Env>,
}

impl<'tcx> UnificationPass<'tcx> {
    pub fn new(tcx: &'tcx mut TyInferCtx) -> Self {
        UnificationPass {
            tcx,
            env: RwLock::new(Env::default()),
        }
    }

    pub(crate) fn add_affected_node(&self, id: NodeId, type_variable: TypeVariableId) {
        self.env.try_write().unwrap().affected_nodes.push((id, type_variable));
    }

    /// Takes all the affected nodes from the pass.
    pub(crate) fn take_affected_nodes(&self) -> Vec<(NodeId, TypeVariableId)> {
        let mut env = self.env.try_write().unwrap();

        env.affected_nodes.drain(..).collect()
    }

    /// Creates a new equality containt, stating that `lhs` must be equal to
    /// `rhs`.
    ///
    /// As a good convention in relation to error messaging, the left-hand side
    /// should be the expected type, and the right-hand side be the found type.
    #[tracing::instrument(
        level = "TRACE",
        skip_all,
        fields(
            type_var = %type_variable,
            lhs = %self.tcx.new_named_type(&lhs, true).unwrap(),
            rhs = %self.tcx.new_named_type(&rhs, true).unwrap(),
        )
    )]
    pub(crate) fn eq(&self, type_variable: TypeVariableId, lhs: TypeRef, rhs: TypeRef) {
        self.env.try_write().unwrap().eq(type_variable, lhs, rhs);
    }

    #[tracing::instrument(
        level = "TRACE",
        skip_all,
        fields(
            type_var = %type_variable,
            operand = %self.tcx.new_named_type(&of, true).unwrap(),
            sub = %self.tcx.hir_path_of_node(param).to_wide_string(),
        )
    )]
    pub(crate) fn sub(&self, type_variable: TypeVariableId, of: TypeRef, param: NodeId) {
        self.env.try_write().unwrap().sub(type_variable, of, param);
    }

    #[tracing::instrument(
        level = "TRACE",
        skip_all,
        fields(
            type_var = %type_variable,
            substitute = %self.tcx.new_named_type(&with, true).unwrap(),
        )
    )]
    pub(crate) fn subst(&self, type_variable: TypeVariableId, with: TypeRef) {
        self.env.try_write().unwrap().subst(type_variable, with);
    }
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct TypeVariableId(TypeId);

impl Display for TypeVariableId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "T?{:X}", self.0.as_node_id().as_usize())
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub(crate) struct TypeVariable {
    pub constraints: Vec<Constraint>,
    pub substitute: Option<TypeRef>,
}

#[derive(Default)]
pub(crate) struct Env {
    affected_nodes: Vec<(NodeId, TypeVariableId)>,
    type_vars: IndexMap<TypeVariableId, TypeVariable>,
}

impl Env {
    /// Ensure there is an entry for constraints for the given type variable.
    ///
    /// So, even if no constraints could be generated, we would still notice
    /// and empty constraint list and throw an error.
    fn ensure_entry_for(&mut self, type_variable: TypeVariableId) {
        self.type_vars.entry(type_variable).or_default();
    }

    /// Creates a new equality containt, stating that `lhs` must be equal to
    /// `rhs`.
    ///
    /// As a good convention in relation to error messaging, the left-hand side
    /// should be the expected type, and the right-hand side be the found type.
    pub(crate) fn eq(&mut self, type_variable: TypeVariableId, lhs: TypeRef, rhs: TypeRef) {
        if lhs == rhs {
            return;
        }

        self.type_vars
            .entry(type_variable)
            .or_default()
            .constraints
            .push(Constraint::Equal { lhs, rhs });
    }

    pub(crate) fn sub(&mut self, type_variable: TypeVariableId, of: TypeRef, param: NodeId) {
        self.type_vars
            .entry(type_variable)
            .or_default()
            .constraints
            .push(Constraint::Subtype { of, param });
    }

    pub(crate) fn subst(&mut self, type_variable: TypeVariableId, with: TypeRef) {
        let existing = self
            .type_vars
            .entry(type_variable)
            .or_default()
            .substitute
            .replace(with);

        assert!(
            existing.is_none(),
            "bug!: replaced existing substitution of {type_variable:?}"
        );
    }
}

#[tracing::instrument(level = "TRACE", skip_all, err)]
fn normalize_equality_constraints(
    tcx: &TyInferCtx,
    type_variable: TypeVariableId,
    constraints: Vec<Constraint>,
) -> Result<TypeRef> {
    debug_assert!(!constraints.is_empty(), "equality constraint list must not be empty");
    debug_assert!(constraints.iter().all(|c| matches!(c, Constraint::Equal { .. })));

    let type_var_hir = tcx.hir().expect_type_variable(type_variable.0.as_node_id())?;
    let mut normalized_types: Option<(TypeRef, TypeRef)> = None;

    for constraint in constraints {
        let Constraint::Equal { lhs, rhs } = constraint else {
            unreachable!();
        };

        let (normalized_lhs, normalized_rhs) = normalize_constraint_types(tcx, type_var_hir.binding, &lhs, &rhs)?;

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

    let type_variable_target = type_var_hir.binding.as_node_id();
    let (normalized_lhs, normalized_rhs) = normalized_types.expect("expected constraints to be normalized and exist");

    // Return the normalized type which does *not* contain the type variable itself,
    // since that wouldn't be very useful to the type checker.
    if normalized_lhs.instance_of == type_variable_target {
        Ok(normalized_rhs)
    } else {
        Ok(normalized_lhs)
    }
}

#[tracing::instrument(
    level = "TRACE",
    skip_all,
    fields(
        lhs = %tcx.new_named_type(lhs, true).unwrap(),
        rhs = %tcx.new_named_type(rhs, true).unwrap(),
    ),
    err
)]
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
            if !bound_lhs.contains(target) && !bound_rhs.contains(target) {
                continue;
            }

            return normalize_constraint_types(tcx, target, bound_lhs, bound_rhs);
        }
    }

    Err(tcx.mismatched_types(lhs, rhs))
}

/// Performs unification on the given type inference context.
///
/// # Errors
///
/// If any errors are raised during unification, this method will either:
/// - return early with the error wrapped in a `Result::Err`,
/// - or raise the inside error inside the diagnostics context in `tcx`.
#[tracing::instrument(level = "INFO", skip_all, err)]
pub fn unify(tcx: &mut TyInferCtx) -> Result<()> {
    verify::verify_type_names(tcx);

    let mut ucx = UnificationPass::new(tcx);

    ucx.introduce_type_variables()?;
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
#[tracing::instrument(level = "DEBUG", skip_all)]
fn invalidate_type_cache(tcx: &mut TyInferCtx) {
    let ctx: &lume_session::GlobalCtx = &*tcx;
    lume_architect::DatabaseContext::db(ctx).clear_all();
}
