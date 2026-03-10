use indexmap::IndexSet;

use crate::engine::*;

pub(crate) struct LinkedVariableList<C: Context> {
    ltr: IndexMap<TypeVar<C>, IndexSet<TypeVar<C>>>,
    rtl: IndexMap<TypeVar<C>, IndexSet<TypeVar<C>>>,
}

impl<C: Context> LinkedVariableList<C> {
    pub fn add_relation(&mut self, lhs: TypeVar<C>, rhs: TypeVar<C>) {
        tracing::debug!(%lhs, %rhs, "add_resolution_link");

        self.ltr.entry(lhs).or_default().insert(rhs);
        self.rtl.entry(rhs).or_default().insert(lhs);
    }

    pub fn walk(&self, id: TypeVar<C>) -> impl Iterator<Item = TypeVar<C>> {
        let ltr = LinkedListIter::create_from(&self.ltr, id);
        let rtl = LinkedListIter::create_from(&self.rtl, id);

        ltr.chain(rtl)
    }
}

impl<C: Context> Default for LinkedVariableList<C> {
    fn default() -> Self {
        Self {
            ltr: IndexMap::new(),
            rtl: IndexMap::new(),
        }
    }
}

struct LinkedListIter<'l, C: Context> {
    stack: smallvec::SmallVec<[TypeVar<C>; 8]>,
    map: &'l IndexMap<TypeVar<C>, IndexSet<TypeVar<C>>>,
}

impl<'l, C: Context> LinkedListIter<'l, C> {
    pub fn create_from(map: &'l IndexMap<TypeVar<C>, IndexSet<TypeVar<C>>>, root: TypeVar<C>) -> Self {
        Self {
            stack: smallvec::smallvec![root],
            map,
        }
    }
}

impl<C: Context> Iterator for LinkedListIter<'_, C> {
    type Item = TypeVar<C>;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.stack.pop()?;
        if let Some(set) = self.map.get(&item) {
            self.stack.extend(set.iter().copied());
        }

        Some(item)
    }
}

impl<C: Context> Engine<'_, C> {
    #[tracing::instrument(level = "TRACE", skip_all, err(Debug))]
    pub(crate) fn create_type_substitutions(&mut self) -> std::result::Result<(), Error<C>> {
        let env = self.env.try_read().unwrap();
        let linked_list = env.coalesce_type_variables(self.ctx);

        let mut substitution_map = IndexMap::<TypeVar<C>, C::Ty>::new();

        for source_type_var in env.type_vars.keys().copied() {
            let eq_constraints = linked_list.walk(source_type_var).flat_map(|id| {
                env.constraints_of(id).filter_map(|constraint| {
                    let Constraint::Equal { lhs, rhs } = constraint else {
                        return None;
                    };

                    if !self.ctx.is_type_variable(rhs) {
                        return Some((lhs, rhs));
                    }

                    None
                })
            });

            substitution_map.insert(
                source_type_var,
                normalize_equality_constraints(self.ctx, source_type_var, eq_constraints)?,
            );
        }

        // After having unified all type variables to, hopefully, some substitute, all
        // the other subtype constraints needs to be checked.
        for type_var_id in env.type_vars.keys().copied() {
            let Some(substitute) = substitution_map.get(&type_var_id) else {
                return Err(Error::Unsolved(type_var_id));
            };

            for constraint in env.constraints_of(type_var_id) {
                let Constraint::Subtype { of, type_parameter_id } = constraint else {
                    continue;
                };

                if !self.ctx.implements_subtype(substitute, of) {
                    return Err(Error::BoundUnsatisfied {
                        ty: substitute.to_owned(),
                        bound: of.to_owned(),
                        type_parameter: *type_parameter_id,
                    });
                }
            }
        }

        // Ensure the read-lock of `env` is dropped before
        // adding the substitutes.
        drop(env);

        for (type_variable, substitution) in substitution_map {
            self.subst(type_variable, substitution);
        }

        // Verify that all type variables have been resolved.
        for (&type_variable_id, type_variable) in &self.env.try_read().unwrap().type_vars {
            if type_variable.substitute.is_none() {
                return Err(Error::Unsolved(type_variable_id));
            }
        }

        Ok(())
    }
}

impl<C: Context> Env<C> {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub fn coalesce_type_variables(&self, tcx: &C) -> LinkedVariableList<C> {
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

                if type_var_id != constraint_variable {
                    linked_list.add_relation(type_var_id, constraint_variable);
                }
            }
        }

        linked_list
    }

    /// Gets an iterator of all the constraints of the given type variable.
    fn constraints_of(&self, id: TypeVar<C>) -> impl Iterator<Item = &Constraint<C>> {
        self.type_vars
            .get(&id)
            .map_or([].iter(), |type_var| type_var.constraints.iter())
    }
}
