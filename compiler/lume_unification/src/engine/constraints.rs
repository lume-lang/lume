use crate::engine::*;

impl<C: Context> Engine<'_, C> {
    /// Ensure there is an entry for constraints for the given type variable.
    ///
    /// So, even if no constraints could be generated, we would still notice
    /// and empty constraint list and throw an error.
    pub(crate) fn ensure_entry_for(&self, type_variable: TypeVar<C>) {
        self.env
            .try_write()
            .unwrap()
            .type_vars
            .entry(type_variable)
            .or_default();
    }

    /// Create a fresh type variable for the given owner, bound to the type
    /// variable `binding`.
    #[tracing::instrument(
        level = "TRACE",
        skip_all,
        fields(
            owner = %self.ctx.name_of(owner).unwrap(),
            binding = %self.ctx.name_of(binding).unwrap(),
        ),
        ret(Display)
    )]
    pub(crate) fn fresh_var(&mut self, owner: C::ID, binding: C::ID, location: Location) -> TypeVar<C> {
        let tyvar = self.ctx.fresh_var(owner, binding, location);

        self.env.try_write().unwrap().ensure_entry_for(tyvar);

        tyvar
    }

    /// Creates a new equality containt, stating that `type_variable` must be
    /// equal to `ty`.
    #[tracing::instrument(
        level = "TRACE",
        skip_all,
        fields(
            type_var = %type_variable,
            ty = %self.ctx.name_of_type(&ty).unwrap(),
        )
    )]
    pub(crate) fn eq(&self, type_variable: TypeVar<C>, ty: C::Ty) {
        self.env.try_write().unwrap().eq(type_variable, ty);
    }

    /// Creates a new subtyping containt, stating that the given type variable,
    /// `type_variable`, must subtype `of`.
    #[tracing::instrument(
        level = "TRACE",
        skip_all,
        fields(
            type_var = %type_variable,
            operand = %self.ctx.name_of_type(&of).unwrap(),
            type_parameter = %self.ctx.name_of(type_parameter).unwrap(),
        )
    )]
    pub(crate) fn sub(&self, type_variable: TypeVar<C>, of: C::Ty, type_parameter: C::ID) {
        self.env.try_write().unwrap().sub(type_variable, of, type_parameter);
    }

    /// Declares the substitution type for the given type variable.
    ///
    /// # Panics
    ///
    /// If a type has already been substituted for this type variable, this
    /// method panics.
    #[tracing::instrument(
        level = "TRACE",
        skip_all,
        fields(
            type_var = %type_variable,
            substitute = %self.ctx.name_of_type(&with).unwrap(),
            location = %self.ctx.span_of(type_variable.0),
        ),
        err(Debug)
    )]
    pub(crate) fn subst(&self, type_variable: TypeVar<C>, with: C::Ty) -> std::result::Result<(), Error<C>> {
        self.occurs_check(type_variable, &with)?;
        self.env.try_write().unwrap().subst(type_variable, with);

        Ok(())
    }

    /// Checks the given resolved type for any recursion.
    ///
    /// Ensures that the resolved types don't contain the type variable that
    /// they resolve. For example, adding a constraint like `T?0 = Option<T?0>`
    /// would cause an infinite type.
    ///
    /// # Errors
    ///
    /// If the given type contains the type variable, returns
    /// [`Error::InfiniteType`] wrapped in [`Err`].
    #[tracing::instrument(
        level = "TRACE",
        skip_all,
        fields(
            type_var = %type_variable,
            ty = %self.ctx.name_of_type(ty).unwrap(),
        ),
        err(Debug)
    )]
    fn occurs_check(&self, type_variable: TypeVar<C>, ty: &C::Ty) -> std::result::Result<(), Error<C>> {
        if let Some(variable_env) = self.ctx.as_type_variable(ty) {
            if variable_env.0 == type_variable.0 {
                return Err(Error::InfiniteType {
                    var: type_variable,
                    ty: ty.clone(),
                });
            }

            if let Some(resolved) = self.resolved_of(variable_env) {
                self.occurs_check(type_variable, &resolved)?;
            }

            Ok(())
        } else {
            for bound_type in ty.bound_types() {
                self.occurs_check(type_variable, bound_type)?;
            }

            Ok(())
        }
    }

    /// Attempts to unify `lhs` and `rhs` to be the same type.
    ///
    /// If successful, the substitution chain is updated so that both types will
    /// resolve to the same value.
    ///
    /// # Errors
    ///
    /// Returns [`Error::Mismatch`] if the two types cannot be unified.
    #[tracing::instrument(
        level = "TRACE",
        skip_all,
        fields(
            lhs = %self.ctx.name_of_type(&lhs).unwrap(),
            rhs = %self.ctx.name_of_type(&rhs).unwrap(),
        ),
        err(Debug)
    )]
    pub(crate) fn unify(&self, lhs: C::Ty, rhs: C::Ty) -> std::result::Result<(), Error<C>> {
        let lhs = self.walk(lhs);
        let rhs = self.walk(rhs);

        match (self.ctx.as_type_variable(&lhs), self.ctx.as_type_variable(&rhs)) {
            // Identical type variables - no reason to bind
            (Some(a), Some(b)) if a.0 == b.0 => Ok(()),

            // Type variable on the left - regular bind
            (Some(var), _) => self.subst(var, rhs),

            // Type variable on the right - symmetric case
            (_, Some(var)) => self.subst(var, lhs),

            // Concrete types - ensure matching type argument count and unify each argument.
            (None, None) => {
                let lhs_bindings = lhs.bound_types().to_vec();
                let rhs_bindings = rhs.bound_types().to_vec();

                if lhs.id() != rhs.id() || lhs_bindings.len() != rhs_bindings.len() {
                    return Err(Error::Mismatch { lhs, rhs });
                }

                for (lhs_bound_type, rhs_bound_type) in lhs_bindings.into_iter().zip(rhs_bindings.into_iter()) {
                    self.unify(lhs_bound_type, rhs_bound_type)?;
                }

                Ok(())
            }
        }
    }

    /// Walks the substitution chain from the given type, until we reach either
    /// an unresolved type variable or valid substitute.
    pub(crate) fn walk(&self, ty: C::Ty) -> C::Ty {
        if let Some(variable_env) = self.ctx.as_type_variable(&ty) {
            match self.resolved_of(variable_env) {
                Some(ty) => self.walk(ty),
                None => ty,
            }
        } else {
            ty
        }
    }

    /// Attempts to unify all the constraints of the given type variable into a
    /// single type.
    fn coalesce_constraints(&self, var: TypeVar<C>) -> std::result::Result<C::Ty, Error<C>> {
        let env = self.env.try_read().unwrap();
        let mut result: Option<C::Ty> = None;

        for constraint in env.constraints_of(var) {
            let Constraint::Equal { ty } = constraint else {
                continue;
            };

            let resolved_type = self.walk(ty.to_owned());

            match result {
                None => result = Some(resolved_type),
                Some(existing) => {
                    self.unify(existing, resolved_type)?;

                    let resolved_var_type = self.walk(self.ctx.as_type(var));
                    result = Some(resolved_var_type);
                }
            }
        }

        result.ok_or(Error::Unsolved(var))
    }

    pub(crate) fn solve(&self, type_var: TypeVar<C>) -> std::result::Result<(), Error<C>> {
        let coalesced_type = self.coalesce_constraints(type_var)?;
        let resolved_ty = self.walk(coalesced_type);

        self.subst(type_var, resolved_ty)
    }

    pub(crate) fn check_bounds_of(&self, type_var: TypeVar<C>) -> std::result::Result<(), Error<C>> {
        let env = self.env.try_read().unwrap();
        let type_env = env.env(type_var).unwrap();
        let subst_type = type_env.substitute.as_ref().unwrap();

        for constraint in &type_env.constraints {
            let Constraint::Subtype { of, type_parameter } = constraint else {
                continue;
            };

            if !self.ctx.implements_subtype(subst_type, of) {
                return Err(Error::BoundUnsatisfied {
                    ty: subst_type.to_owned(),
                    bound: of.to_owned(),
                    type_parameter: *type_parameter,
                });
            }
        }

        Ok(())
    }

    /// Gets a vector of all type variables in the environment.
    #[inline]
    fn type_variables(&self) -> Vec<TypeVar<C>> {
        self.env.try_read().unwrap().type_vars.keys().copied().collect()
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err(Debug))]
    fn resolve_all(&self) -> std::result::Result<(), Error<C>> {
        for type_variable in self.type_variables() {
            let resolved = self.resolve(type_variable)?;

            self.subst(type_variable, resolved)?;
            self.check_bounds_of(type_variable)?;
        }

        Ok(())
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err(Debug))]
    pub(crate) fn substitute_all(&self) -> std::result::Result<(), Error<C>> {
        // Solve each variable against its constraints...
        for type_variable in self.type_variables() {
            self.solve(type_variable)?;
        }

        // ...then flatten all substitution chains.
        self.resolve_all()?;

        Ok(())
    }
}
