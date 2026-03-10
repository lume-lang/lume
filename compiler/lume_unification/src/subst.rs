use indexmap::{IndexMap, IndexSet};
use lume_errors::{Diagnostic, Result};
use lume_infer::TyInferCtx;
use lume_span::Location;
use lume_types::TypeRef;

use crate::constraints::Constraint;
use crate::introduce::InferedNode;
use crate::{Env, TypeVariable, TypeVariableId, UnificationPass, normalize_equality_constraints};

#[derive(Default)]
pub(crate) struct LinkedVariableList {
    ltr: IndexMap<TypeVariableId, IndexSet<TypeVariableId>>,
    rtl: IndexMap<TypeVariableId, IndexSet<TypeVariableId>>,
}

impl LinkedVariableList {
    pub fn add_relation(&mut self, lhs: TypeVariableId, rhs: TypeVariableId) {
        tracing::debug!(%lhs, %rhs, "add_resolution_link");

        self.ltr.entry(lhs).or_default().insert(rhs);
        self.rtl.entry(rhs).or_default().insert(lhs);
    }

    pub fn walk(&self, id: TypeVariableId) -> impl Iterator<Item = TypeVariableId> {
        let ltr = LinkedListIter::create_from(&self.ltr, id);
        let rtl = LinkedListIter::create_from(&self.rtl, id);

        ltr.chain(rtl)
    }
}

struct LinkedListIter<'l> {
    stack: smallvec::SmallVec<[TypeVariableId; 8]>,
    map: &'l IndexMap<TypeVariableId, IndexSet<TypeVariableId>>,
}

impl<'l> LinkedListIter<'l> {
    pub fn create_from(map: &'l IndexMap<TypeVariableId, IndexSet<TypeVariableId>>, root: TypeVariableId) -> Self {
        Self {
            stack: smallvec::smallvec![root],
            map,
        }
    }
}

impl Iterator for LinkedListIter<'_> {
    type Item = TypeVariableId;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.stack.pop()?;
        if let Some(set) = self.map.get(&item) {
            self.stack.extend(set.iter().copied());
        }

        Some(item)
    }
}

impl Env {
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub fn coalesce_type_variables(&self, tcx: &TyInferCtx) -> Result<LinkedVariableList> {
        let mut linked_list = LinkedVariableList::default();

        for (&type_var_id, type_var) in self.type_vars.iter().rev() {
            for constraint in &type_var.constraints {
                // INVARIANT:
                // Type variables should always exist as the right-hand side of the constraint,
                // since the left-hand side is meant for the "expected" type.
                let Constraint::Equal { rhs, .. } = constraint else {
                    continue;
                };

                let Some(constraint_variable) = tcx.as_type_variable(rhs) else {
                    continue;
                };

                let constraint_variable_id = TypeVariableId(constraint_variable.id.into());

                if type_var_id != constraint_variable_id {
                    linked_list.add_relation(type_var_id, constraint_variable_id);
                }
            }
        }

        Ok(linked_list)
    }

    /// Gets an iterator of all the constraints of the given type variable.
    fn constraints_of(&self, id: TypeVariableId) -> impl Iterator<Item = &Constraint> {
        static EMPTY: &indexmap::set::Slice<Constraint> = indexmap::set::Slice::<Constraint>::new();

        self.type_vars
            .get(&id)
            .map_or(EMPTY.iter(), |type_var| type_var.constraints.iter())
    }
}

impl UnificationPass<'_> {
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub(crate) fn create_type_substitutions(&mut self) -> Result<()> {
        let env = self.env.try_read().unwrap();
        let linked_list = env.coalesce_type_variables(self.tcx)?;

        let mut substitution_map = IndexMap::<TypeVariableId, TypeRef>::new();

        for source_type_var in env.type_vars.keys().copied() {
            let eq_constraints = linked_list.walk(source_type_var).flat_map(|id| {
                env.constraints_of(id).filter_map(|constraint| {
                    let Constraint::Equal { lhs, rhs } = constraint else {
                        return None;
                    };

                    if !self.tcx.is_type_variable(rhs) {
                        return Some((lhs, rhs));
                    }

                    None
                })
            });

            substitution_map.insert(
                source_type_var,
                normalize_equality_constraints(self.tcx, &substitution_map, source_type_var, eq_constraints)?,
            );
        }

        // After having unified all type variables to, hopefully, some substitute, all
        // the other subtype constraints needs to be checked.
        'type_var: for type_var_id in env.type_vars.keys().copied() {
            let type_parameter_binding = self.tcx.hir_tyvar_binding_of(type_var_id.0.as_node_id()).unwrap();
            let type_parameter = self.tcx.hir_expect_type_parameter(type_parameter_binding.as_node_id());

            let Some(substitute) = substitution_map.get(&type_var_id) else {
                self.tcx.dcx().emit(
                    TypeArgumentInferenceFailed {
                        source: self.tcx.hir_span_of_node(type_var_id.0.as_node_id()),
                        type_param_name: type_parameter.name.to_string(),
                    }
                    .into(),
                );

                continue;
            };

            for constraint in env.constraints_of(type_var_id) {
                let Constraint::Subtype { of, param } = constraint else {
                    continue;
                };

                debug_assert!(
                    self.tcx.is_trait(of).unwrap(),
                    "expected subtype-constraint to reference trait"
                );

                if !self.tcx.trait_impl_by(of, substitute)? {
                    let type_param = self.tcx.hir_expect_type_parameter(*param);
                    let type_param_constraint = type_param
                        .constraints
                        .iter()
                        .find(|c| c.name == self.tcx.hir_path_of_node(of.instance_of))
                        .unwrap();

                    self.tcx.dcx().emit(
                        TypeParameterConstraintUnsatisfied {
                            source: substitute.location,
                            constraint_loc: type_param_constraint.location,
                            param_name: type_param.name.to_string(),
                            type_name: self
                                .tcx
                                .ty_stringifier(substitute)
                                .include_namespace(true)
                                .stringify()?,
                            constraint_name: self.tcx.ty_stringifier(of).include_namespace(true).stringify()?,
                        }
                        .into(),
                    );

                    continue 'type_var;
                }
            }
        }

        // Ensure the read-lock of `env` is dropped before
        // adding the substitutes.
        drop(env);

        self.tcx.dcx().ensure_untainted()?;

        for (type_variable, substitution) in substitution_map {
            self.subst(type_variable, substitution);
        }

        Ok(())
    }
}

impl UnificationPass<'_> {
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub(crate) fn apply_substitutions(&mut self) -> Result<()> {
        fn replace_in(bound_types: &mut [lume_hir::Type], type_var_id: TypeVariableId, replacement: lume_hir::Type) {
            let idx = bound_types
                .iter()
                .position(|ty| ty.id == type_var_id.0)
                .expect("expected type variable in node");

            bound_types[idx] = replacement;
        }

        let tcx_constraints = &self.env.try_read().unwrap().type_vars;

        for (node_id, type_var_id) in self.affected_nodes() {
            let type_parameter_binding = self.tcx.hir_tyvar_binding_of(type_var_id.0.as_node_id()).unwrap();
            let Some(TypeVariable { substitute, .. }) = tcx_constraints.get(&type_var_id) else {
                unreachable!();
            };

            let Some(substitute) = substitute else {
                let type_param = self.tcx.hir_expect_type_parameter(type_parameter_binding.as_node_id());

                self.tcx.dcx().emit(
                    TypeArgumentInferenceFailed {
                        source: self.tcx.hir_span_of_node(type_var_id.0.as_node_id()),
                        type_param_name: type_param.name.to_string(),
                    }
                    .into(),
                );

                continue;
            };

            let replacement_ty = self.tcx.hir_lift_type(substitute)?;

            let mut node = if let Some(node) = self.tcx.hir_node(node_id)
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
                    let callable = self.tcx.probe_callable_instance(call)?;
                    let is_bound_to_method = self
                        .tcx
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
                    let callable = self.tcx.probe_callable_static(call)?;
                    let is_bound_to_method = self
                        .tcx
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

            self.tcx
                .hir_mut()
                .nodes
                .insert(node_id, Into::<lume_hir::Node>::into(node));

            // Remove the temporary type variable node from the HIR and type database.
            //
            // This is to prevent problems in later stages of the compiler, which aren't
            // meant to handle them.
            self.tcx.hir_mut().nodes.swap_remove(&type_var_id.0.as_node_id());
            self.tcx.tdb_mut().types.swap_remove(&type_var_id.0.as_node_id());
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
    pub type_name: String,
    pub constraint_name: String,
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
