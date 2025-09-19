pub(crate) mod diagnostics;

use crate::TyCheckCtx;
use error_snippet::Result;
pub use lume_infer::query::Callable;
use lume_span::ExpressionId;
use lume_types::{Function, Method, TypeRef};

impl TyCheckCtx {
    /// Checks whether the given [`Method`] is valid, in terms of provided
    /// arguments, type arguments, visibility and type of callee.
    ///
    /// If the [`Method`] is not valid for the given expression, returns
    /// [`CallableCheckResult::Failure`] with one-or-more reasons.
    #[tracing::instrument(level = "TRACE", skip_all, err, ret)]
    pub(crate) fn check_method<'a>(&self, method: &'a Method, expr: lume_hir::CallExpression) -> Result<bool> {
        self.check_signature(Callable::Method(method), expr)
    }

    /// Checks whether the given [`Function`] is valid, in terms of provided
    /// arguments, type arguments and visibility.
    ///
    /// If the [`Function`] is not valid for the given expression, returns
    /// [`CallableCheckResult::Failure`] with one-or-more reasons.
    #[tracing::instrument(level = "TRACE", skip_all, err, ret)]
    pub(crate) fn check_function<'a>(&self, function: &'a Function, expr: &'a lume_hir::StaticCall) -> Result<bool> {
        self.check_signature(Callable::Function(function), lume_hir::CallExpression::Static(expr))
    }

    /// Checks whether the given invocation signature matches the signature of the
    /// corresponding callable.
    #[tracing::instrument(level = "TRACE", skip_all, err, ret)]
    fn check_signature<'a>(&self, callable: Callable<'a>, expr: lume_hir::CallExpression<'a>) -> Result<bool> {
        let narrow_signature = callable.signature();
        let is_instance_method = narrow_signature.is_instanced();

        if !self.check_type_params(expr, narrow_signature.type_params, expr.type_arguments())? {
            return Ok(false);
        }

        let argument_ids = match (expr, is_instance_method) {
            // For any instanced call where the method is also instanced, we
            // combine the callee and arguments list into one, such that:
            // ```lm
            // a.foo();
            // ```
            //
            // is implicitly transformed into:
            // ```lm
            // Foo::foo(a);
            // ```
            (lume_hir::CallExpression::Instanced(call), true) => &[&[call.callee][..], &call.arguments[..]].concat(),
            (lume_hir::CallExpression::Intrinsic(call), true) => &call.arguments,
            (lume_hir::CallExpression::Static(call), _) => &call.arguments,
            (lume_hir::CallExpression::Instanced(_) | lume_hir::CallExpression::Intrinsic(_), false) => {
                self.dcx().emit(
                    diagnostics::InstanceCallOnStaticMethod {
                        source: expr.location(),
                        method_name: callable.name().clone(),
                    }
                    .into(),
                );

                return Ok(false);
            }
        };

        let arguments = argument_ids
            .iter()
            .map(|id| self.hir().expect_expression(*id))
            .collect::<Result<Vec<_>>>()?;

        let signature = self.signature_of_instantiated(callable, expr)?;

        self.check_params(expr, &signature.params, &arguments)
    }

    /// Checks whether the given type arguments matches the signature of the given
    /// type parameters.
    #[tracing::instrument(level = "TRACE", skip_all, err, ret)]
    fn check_type_params<'a>(
        &self,
        expr: lume_hir::CallExpression<'a>,
        type_params: &'a [lume_hir::TypeParameterId],
        type_args: &'a [lume_hir::Type],
    ) -> Result<bool> {
        // Verify that the amount of type arguments match the
        // expected number of type parameters.
        if type_params.len() != type_args.len() {
            self.dcx().emit(
                diagnostics::TypeArgumentCountMismatch {
                    source: expr.location(),
                    expected: type_params.len(),
                    actual: type_args.len(),
                }
                .into(),
            );

            return Ok(false);
        }

        let mut success = true;

        for (param_id, hir_arg) in type_params.iter().zip(type_args.iter()) {
            let param = self.tdb().type_parameter(*param_id).unwrap();
            let arg = self.mk_type_ref_from_expr(hir_arg, expr.id())?;

            for constraint in &param.constraints {
                if !self.check_type_compatibility(&arg, constraint)? {
                    success = false;

                    self.dcx().emit(
                        diagnostics::TypeParameterConstraintUnsatisfied {
                            source: arg.location,
                            constraint_loc: constraint.location,
                            param_name: param.name.clone(),
                            type_name: self.new_named_type(&arg, false)?,
                            constraint_name: self.new_named_type(constraint, false)?,
                        }
                        .into(),
                    );
                }
            }
        }

        Ok(success)
    }

    /// Checks whether the given arguments matches the signature of the given
    /// parameters.
    #[tracing::instrument(level = "TRACE", skip_all, fields(name = %expr.name()), err, ret)]
    fn check_params<'a>(
        &self,
        expr: lume_hir::CallExpression<'a>,
        parameters: &'a lume_types::Parameters,
        arguments: &'a [&'a lume_hir::Expression],
    ) -> Result<bool> {
        // Verify that the expected argument count is met, as defined
        // by the parameter definition.
        if (parameters.is_vararg() && parameters.len() - 1 > arguments.len())
            || (!parameters.is_vararg() && parameters.len() != arguments.len())
        {
            if parameters.is_vararg() {
                self.dcx().emit(
                    diagnostics::VariableArgumentCountMismatch {
                        source: expr.location(),
                        expected: parameters.len(),
                        actual: arguments.len(),
                    }
                    .into(),
                );
            } else {
                self.dcx().emit(
                    diagnostics::ArgumentCountMismatch {
                        source: expr.location(),
                        expected: parameters.len(),
                        actual: arguments.len(),
                    }
                    .into(),
                );
            }

            return Ok(false);
        }

        let mut success = true;

        if parameters.is_vararg() {
            // If the parameter count is variable, expect at least
            // the amount of required parameters as arguments.
            //
            // If a function/method takes N parameters + 1 vararg parameter,
            // we expect at least N arguments.
            if parameters.len() - 1 > arguments.len() {
                self.dcx().emit(
                    diagnostics::VariableArgumentCountMismatch {
                        source: expr.location(),
                        expected: parameters.len(),
                        actual: arguments.len(),
                    }
                    .into(),
                );

                return Ok(false);
            }

            let fixed_param_count = parameters.len() - 1;

            let (vararg_param, fixed_params) = parameters.inner().split_last().unwrap();
            let (fixed_args, vararg_args) = arguments.split_at(fixed_param_count);

            // Verify that all the fixed parameters are compatible with the arguments
            // passed to the method.
            for (param, arg) in fixed_params.iter().zip(fixed_args.iter()) {
                let arg_type = self.type_of_expr(arg)?;

                if let Err(err) = self.ensure_type_compatibility(&arg_type, &param.ty) {
                    self.dcx().emit(err);
                    success = false;
                }
            }

            // Verify that the vararg parameter has the same type as all arguments,
            // which sits after the last fixed parameter.
            for arg in vararg_args {
                let arg_type = self.type_of_expr(arg)?;

                if let Err(err) = self.ensure_type_compatibility(&arg_type, &vararg_param.ty) {
                    self.dcx().emit(err);
                    success = false;
                }
            }
        } else {
            // If the parameter count is fixed, we need exactly
            // that amount of arguments.
            if parameters.len() != arguments.len() {
                self.dcx().emit(
                    diagnostics::ArgumentCountMismatch {
                        source: expr.location(),
                        expected: parameters.len(),
                        actual: arguments.len(),
                    }
                    .into(),
                );

                return Ok(false);
            }

            // Verify that all the parameters are compatible with the arguments
            // passed to the method.
            for (param, arg) in parameters.inner().iter().zip(arguments.iter()) {
                let arg_type = self.type_of_expr(arg)?;

                if let Err(err) = self.ensure_type_compatibility(&arg_type, &param.ty) {
                    self.dcx().emit(err);
                    success = false;
                }
            }
        }

        Ok(success)
    }

    /// Looks up all [`Method`]s and [`Function`]s and attempts to find one, which matches the
    /// signature of the given call expression.
    ///
    /// Callables returned by this method are checked for validity within the current
    /// context, including visibility, arguments and type arguments. To look up methods
    /// which only match the callee type and method name, see [`ThirBuildCtx::lookup_methods_on()`].
    #[tracing::instrument(level = "DEBUG", skip_all, fields(name = %expr.name(), args = ?expr.arguments()), err)]
    pub fn lookup_callable(&self, expr: lume_hir::CallExpression) -> Result<Callable<'_>> {
        match expr {
            lume_hir::CallExpression::Instanced(_) | lume_hir::CallExpression::Intrinsic(_) => {
                let method = self.lookup_methods(expr)?;

                Ok(Callable::Method(method))
            }
            lume_hir::CallExpression::Static(call) => {
                if let Some(callee_ty) = call.name.clone().parent()
                    && callee_ty.is_type()
                {
                    let method = self.lookup_methods(expr)?;

                    Ok(Callable::Method(method))
                } else {
                    let function = self.lookup_functions(call)?;

                    Ok(Callable::Function(function))
                }
            }
        }
    }

    /// Looks up all [`Method`]s and attempts to find one, which matches the
    /// signature of the given call expression.
    ///
    /// Methods returned by this method are checked for validity within the current
    /// context, including visibility, arguments and type arguments. To look up methods
    /// which only match the callee type and method name, see [`ThirBuildCtx::lookup_methods_on()`].
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub fn lookup_methods(&self, expr: lume_hir::CallExpression) -> Result<&'_ Method> {
        let Callable::Method(probed_method) = self.probe_callable(expr)? else {
            panic!("bug!: expected expression to yield a method, found function: {expr:#?}");
        };

        self.check_method(probed_method, expr)?;

        Ok(probed_method)
    }

    /// Looks up all [`Function`]s and attempts to find one, which matches the
    /// signature of the given call expression.
    ///
    /// Functions returned by this method are checked for validity within the current
    /// context, including visibility, arguments and type arguments. To look up functions
    /// which only match function name, see [`ThirBuildCtx::probe_functions()`].
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub fn lookup_functions(&self, expr: &lume_hir::StaticCall) -> Result<&'_ Function> {
        let Callable::Function(probed_func) = self.probe_callable(lume_hir::CallExpression::Static(expr))? else {
            panic!("bug!: expected expression to yield a function, found method: {expr:#?}");
        };

        self.check_function(probed_func, expr)?;

        Ok(probed_func)
    }

    /// Looks up all [`Method`]s and attempts to find a single [`Method`], which matches the
    /// signature of the given instance call expression.
    ///
    /// Methods returned by this method are checked for validity within the current
    /// context, including visibility, arguments and type arguments. The look up methods
    /// which only match the callee type and method name, see [`ThirBuildCtx::lookup_methods_on()`].
    ///
    /// For a generic callable lookup, see [`ThirBuildCtx::lookup_callable()`]. For a static callable
    /// lookup, see [`ThirBuildCtx::lookup_callable_static()`].
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub fn lookup_callable_instance(&self, call: &lume_hir::InstanceCall) -> Result<Callable<'_>> {
        self.lookup_callable(lume_hir::CallExpression::Instanced(call))
    }

    /// Looks up all [`Callable`]s and attempts to find one, which matches the
    /// signature of the given static call expression.
    ///
    /// Callables returned by this method are checked for validity within the current
    /// context, including visibility, arguments and type arguments. To look up methods
    /// which only match the callee type and method name, see [`ThirBuildCtx::lookup_methods_on()`].
    ///
    /// For a generic callable lookup, see [`ThirBuildCtx::lookup_callable()`]. For an instance callable
    /// lookup, see [`ThirBuildCtx::lookup_callable_instance()`].
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub fn lookup_callable_static(&self, call: &lume_hir::StaticCall) -> Result<Callable<'_>> {
        self.lookup_callable(lume_hir::CallExpression::Static(call))
    }

    /// Ensures that the return type of a block matches the expected type.
    ///
    /// # Errors
    ///
    /// If not all branches return a value, or if different branches return different types,
    /// the method returns `Err`.
    #[tracing::instrument(level = "TRACE", skip(self), err, ret)]
    pub(crate) fn ensure_block_ty_match(&self, block: &lume_hir::Block, expected: &TypeRef) -> Result<()> {
        self.ensure_type_compatibility(&self.type_of_block_ret(block)?, expected)
    }

    /// Attempts to find the return type of a block. The return type is inferred
    /// from the last statement within the block, or `void` if empty.
    ///
    /// # Errors
    ///
    /// If not all branches return a value, or if different branches return different types,
    /// the method returns `Err`.
    #[tracing::instrument(level = "TRACE", skip_all, err, ret)]
    pub(crate) fn type_of_block_ret(&self, block: &lume_hir::Block) -> Result<TypeRef> {
        let last_statement = block.statements.last();

        if let Some(stmt) = last_statement {
            let stmt = self.hir().expect_statement(*stmt)?;

            self.matching_type_of_stmt(stmt)
        } else {
            Ok(TypeRef::void())
        }
    }

    /// Returns the *type* of the given [`lume_hir::Statement`]. All branches
    /// of the statement are asserted to return the same type.
    ///
    /// # Errors
    ///
    /// Returns `Err` if different branches return different types.
    ///
    /// # Panics
    ///
    /// This method will panic if no definition with the given ID exists
    /// within it's declared module. This also applies to any recursive calls this
    /// method makes, in the case of some expressions, such as assignments.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub(crate) fn matching_type_of_stmt(&self, stmt: &lume_hir::Statement) -> Result<TypeRef> {
        match &stmt.kind {
            lume_hir::StatementKind::Expression(expr) => {
                let expr = self.hir().expect_expression(*expr)?;

                if let lume_hir::ExpressionKind::If(cond) = &expr.kind {
                    self.matching_type_of_cond(cond.id, &cond.cases)
                } else {
                    self.type_of_stmt(stmt)
                }
            }
            _ => self.type_of_stmt(stmt),
        }
    }

    /// Returns the *type* of the given [`lume_hir::Condition`]s. All conditions
    /// of the given list are asserted to return the same type.
    ///
    /// # Errors
    ///
    /// Returns `Err` if different conditions return different types.
    ///
    /// # Panics
    ///
    /// This method will panic if no definition with the given ID exists
    /// within it's declared module. This also applies to any recursive calls this
    /// method makes, in the case of some expressions, such as assignments.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub(crate) fn matching_type_of_cond(&self, id: ExpressionId, cases: &[lume_hir::Condition]) -> Result<TypeRef> {
        let first_case = cases.first().expect("expected at least 1 case");
        let last_case = cases.last().expect("expected at least 1 case");

        let has_else_case = cases.iter().any(|case| case.condition.is_none());

        // If there is no `else` block, we expect the condition to return `void`,
        // since there would otherwise be no fallback.
        let expected_type = if has_else_case {
            self.type_of_condition_scope(first_case)?
        } else {
            self.expected_type_of(id)?.unwrap_or_else(|| TypeRef::void())
        };

        // Ensure that all branches return the same type.
        for case in cases {
            let found_type = self.type_of_condition_scope(case)?;

            if found_type != expected_type {
                let found = self.new_named_type(&found_type, false)?;
                let expected = self.new_named_type(&expected_type, false)?;

                if has_else_case {
                    return Err(crate::check::errors::MismatchedTypesBranches {
                        found,
                        expected,
                        found_loc: found_type.location,
                        reason_loc: expected_type.location,
                    }
                    .into());
                } else {
                    return Err(crate::check::errors::MismatchedTypesBranchesCondition {
                        found,
                        expected,
                        found_loc: found_type.location,
                    }
                    .into());
                }
            }
        }

        // Ensure that all possible combinations of conditions return a value
        if !expected_type.is_void() {
            let Some(else_block) = cases.iter().rfind(|case| case.condition.is_none()) else {
                return Err(crate::check::errors::MissingReturnBranch {
                    source: last_case.location,
                    expected: self.new_named_type(&expected_type, false)?,
                }
                .into());
            };

            self.ensure_block_ty_match(&else_block.block, &expected_type)?;
        }

        Ok(expected_type)
    }
}
