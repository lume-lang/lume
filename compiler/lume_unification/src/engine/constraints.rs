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
            %type_variable,
            ty = %self.ctx.name_of_type(&ty).unwrap(),
        )
    )]
    pub(crate) fn eq(&self, type_variable: TypeVar<C>, ty: C::Ty) {
        if let Some(ty_type_variable) = self.ctx.as_type_variable(&ty) {
            self.env.try_write().unwrap().union(type_variable, ty_type_variable);
        }

        self.env.try_write().unwrap().eq(type_variable, ty);
    }

    /// Creates a new subtyping containt, stating that the given type variable,
    /// `type_variable`, must subtype `of`.
    #[tracing::instrument(
        level = "TRACE",
        skip_all,
        fields(
            %type_variable,
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
            %type_variable,
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
            %type_variable,
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

            if let Some(resolved) = self.substitute_of(variable_env) {
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

        match (self.ctx.kind_of_type(&lhs), self.ctx.kind_of_type(&rhs)) {
            // Identical type variables - no reason to bind
            (TypeKind::Variable(a), TypeKind::Variable(b)) if a == b => Ok(()),

            // Identical type parameters - no reason to bind
            (TypeKind::Parameter(a), TypeKind::Parameter(b)) if a == b => Ok(()),

            // Type parameters cannot be equal in the same scope
            (TypeKind::Parameter(_), TypeKind::Parameter(_)) => Err(Error::RigidMismatch { lhs, rhs }),

            // Type variable on the left - regular bind
            (TypeKind::Variable(var), _) => self.subst(var, rhs),

            // Type variable on the right - symmetric case
            (_, TypeKind::Variable(var)) => self.subst(var, lhs),

            // Concrete types - ensure matching type argument count and unify each argument.
            (TypeKind::Concrete(_), TypeKind::Concrete(_)) => {
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

            // Concrete types can never equate to type parameters
            (TypeKind::Concrete(_), TypeKind::Parameter(_)) | (TypeKind::Parameter(_), TypeKind::Concrete(_)) => {
                Err(Error::RigidMismatch { lhs, rhs })
            }
        }
    }

    /// Walks the substitution chain from the given type, until we reach either
    /// an unresolved type variable or valid substitute.
    #[tracing::instrument(level = "TRACE", skip_all, fields(ty = %self.ctx.name_of_type(&ty).unwrap()))]
    pub(crate) fn walk(&self, ty: C::Ty) -> C::Ty {
        if let Some(variable_env) = self.ctx.as_type_variable(&ty) {
            match self.substitute_of(variable_env) {
                Some(ty) => self.walk(ty),
                None => ty,
            }
        } else {
            tracing::trace!(result = %self.ctx.name_of_type(&ty).unwrap());

            ty
        }
    }

    /// Attempts to unify all the constraints of the given type variable into a
    /// single type.
    #[tracing::instrument(level = "DEBUG", skip_all, fields(type_variable = %var), err(Debug))]
    fn coalesce_constraints(&self, var: TypeVar<C>) -> std::result::Result<C::Ty, Error<C>> {
        let env = self.env.try_read().unwrap();
        let constraints = env.constraints_of(var);

        let mut solution: Option<C::Ty> = None;

        // Force the read lock to drop, since `unify` might want to acquite a write lock
        // for substitution.
        drop(env);

        if tracing::enabled!(tracing::Level::DEBUG) {
            for constraint in &constraints {
                match constraint {
                    Constraint::Equal { ty } => {
                        tracing::debug!(
                            type_variable = %var,
                            ty = %self.ctx.name_of_type(ty).unwrap(),
                            "constraint_eq"
                        );
                    }
                    Constraint::Subtype { of, .. } => {
                        tracing::debug!(
                            type_variable = %var,
                            sub = %self.ctx.name_of_type(of).unwrap(),
                            "constraint_sub"
                        );
                    }
                }
            }
        }

        for constraint in constraints {
            let Constraint::Equal { ty } = constraint else {
                continue;
            };

            let resolved_type = self.walk(ty.to_owned());

            match solution.take() {
                None => solution = Some(resolved_type),
                Some(existing) => {
                    self.unify(existing, resolved_type)?;

                    solution = Some(self.walk(self.ctx.as_type(var)));
                }
            }
        }

        solution.ok_or(Error::Unsolved(var))
    }

    #[tracing::instrument(level = "DEBUG", skip_all, fields(type_variable = %type_var), err(Debug))]
    pub(crate) fn solve(&self, type_var: TypeVar<C>) -> std::result::Result<(), Error<C>> {
        let coalesced_type = self.coalesce_constraints(type_var)?;
        let resolved_ty = self.walk(coalesced_type);

        self.subst(type_var, resolved_ty)
    }

    #[tracing::instrument(level = "DEBUG", skip_all, fields(type_variable = %type_var), err(Debug))]
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
    fn resolve_all(&self) -> std::result::Result<(), AggregateError<C>> {
        let mut errors = AggregateError::default();

        for type_variable in self.type_variables() {
            let result = || -> std::result::Result<(), Error<C>> {
                let mut resolved = self.resolve(type_variable)?;

                // Ensure that all type variables within the resolved substitute is resolved
                // into concrete types.
                resolved.walk_mut().for_each(|ty| {
                    if matches!(self.ctx.kind_of_type(ty), TypeKind::Variable(_)) {
                        *ty = self.walk(ty.to_owned());
                    }
                });

                self.subst(type_variable, resolved)?;
                self.check_bounds_of(type_variable)?;

                Ok(())
            }();

            if let Err(err) = result {
                errors.push(type_variable, err);
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(())
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err(Debug))]
    pub(crate) fn substitute_all(&self) -> std::result::Result<(), AggregateError<C>> {
        let mut errors = AggregateError::default();

        // Solve each variable against its constraints...
        for type_variable in self.type_variables() {
            if let Err(err) = self.solve(type_variable) {
                errors.push(type_variable, err);
            }
        }

        // ...then flatten all substitution chains.
        if let Err(err) = self.resolve_all() {
            errors.extend(err);
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(())
    }
}
