pub(crate) mod diagnostics;

use error_snippet::Result;
use lume_architect::cached_query;
use lume_hir::{Node, Visibility};
pub use lume_infer::query::Callable;
use lume_span::NodeId;
use lume_types::{Function, Method, TypeRef};

use crate::TyCheckCtx;

impl TyCheckCtx {
    /// Checks whether the given [`Method`] is valid, in terms of provided
    /// arguments, type arguments, visibility and type of callee.
    ///
    /// If the [`Method`] is not valid for the given expression, returns
    /// [`CallableCheckResult::Failure`] with one-or-more reasons.
    #[libftrace::traced(level = Trace, err, ret)]
    pub(crate) fn check_method(&self, method: &Method, expr: lume_hir::CallExpression) -> Result<bool> {
        self.check_signature(Callable::Method(method), expr)
    }

    /// Checks whether the given [`Function`] is valid, in terms of provided
    /// arguments, type arguments and visibility.
    ///
    /// If the [`Function`] is not valid for the given expression, returns
    /// [`CallableCheckResult::Failure`] with one-or-more reasons.
    #[libftrace::traced(level = Trace, err, ret)]
    pub(crate) fn check_function<'a>(&self, function: &'a Function, expr: &'a lume_hir::StaticCall) -> Result<bool> {
        self.check_signature(Callable::Function(function), lume_hir::CallExpression::Static(expr))
    }

    /// Checks whether the given invocation signature matches the signature of
    /// the corresponding callable.
    #[libftrace::traced(level = Trace, err, ret)]
    fn check_signature<'a>(&self, callable: Callable<'a>, expr: lume_hir::CallExpression<'a>) -> Result<bool> {
        let narrow_signature = self.signature_of(callable)?;
        let is_instance_method = narrow_signature.is_instanced();

        if !self.check_type_params(expr, &narrow_signature.type_params, expr.type_arguments())? {
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
            (lume_hir::CallExpression::Instanced(call), true) => [&[call.callee][..], &call.arguments[..]].concat(),
            (lume_hir::CallExpression::Intrinsic(call), true) => call.kind.arguments(),
            (lume_hir::CallExpression::Static(call), _) => call.arguments.clone(),
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

        let signature = self.instantiated_signature_of(callable, expr)?;

        self.check_params(expr, &signature.params, &arguments)
    }

    /// Checks whether the given type arguments matches the signature of the
    /// given type parameters.
    #[libftrace::traced(level = Trace, err, ret)]
    fn check_type_params<'a>(
        &self,
        expr: lume_hir::CallExpression<'a>,
        type_params: &'a [lume_span::NodeId],
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

        for (&param_id, hir_arg) in type_params.iter().zip(type_args.iter()) {
            let param = self.hir_expect_type_parameter(param_id);
            let arg = self.mk_type_ref_from_expr(hir_arg, expr.id())?;

            for constraint in &param.constraints {
                let constraint_type = self.mk_type_ref_from(constraint, param_id)?;

                if !self.check_type_compatibility(&arg, &constraint_type)? {
                    success = false;

                    self.dcx().emit(
                        diagnostics::TypeParameterConstraintUnsatisfied {
                            source: arg.location,
                            constraint_loc: constraint.location,
                            param_name: param.name.to_string(),
                            type_name: self.new_named_type(&arg, false)?,
                            constraint_name: self.new_named_type(&constraint_type, false)?,
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
    #[libftrace::traced(level = Trace, fields(name = expr.name()), err, ret)]
    fn check_params<'a>(
        &self,
        expr: lume_hir::CallExpression<'a>,
        parameters: &'a [lume_types::Parameter],
        arguments: &'a [&'a lume_hir::Expression],
    ) -> Result<bool> {
        let is_vararg = parameters.iter().any(|param| param.vararg);

        // Verify that the expected argument count is met, as defined
        // by the parameter definition.
        if (is_vararg && parameters.len() - 1 > arguments.len()) || (!is_vararg && parameters.len() != arguments.len())
        {
            if is_vararg {
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

        if is_vararg {
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

            let (vararg_param, fixed_params) = parameters.split_last().unwrap();
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
            for (param, arg) in parameters.iter().zip(arguments.iter()) {
                let arg_type = self.type_of_expr(arg)?;

                if let Err(err) = self.ensure_type_compatibility(&arg_type, &param.ty) {
                    self.dcx().emit(err);
                    success = false;
                }
            }
        }

        Ok(success)
    }

    /// Looks up all [`Method`]s and [`Function`]s and attempts to find one,
    /// which matches the signature of the given call expression.
    ///
    /// Callables returned by this method are checked for validity within the
    /// current context, including visibility, arguments and type arguments.
    /// To look up methods which only match the callee type and method name,
    /// see [`lume_infer::TyInferCtx::lookup_methods_on`].
    #[libftrace::traced(level = Trace, fields(name = expr.name(), args = expr.arguments()), err, ret)]
    pub fn lookup_callable(&self, expr: lume_hir::CallExpression) -> Result<Callable<'_>> {
        match expr {
            lume_hir::CallExpression::Instanced(_) | lume_hir::CallExpression::Intrinsic(_) => {
                let method = self.lookup_methods(expr)?;
                self.dcx.ensure_untainted()?;

                Ok(Callable::Method(method))
            }
            lume_hir::CallExpression::Static(call) => {
                if call.receiving_type().is_some() {
                    let method = self.lookup_methods(expr)?;
                    self.dcx.ensure_untainted()?;

                    Ok(Callable::Method(method))
                } else {
                    let function = self.lookup_functions(call)?;
                    self.dcx.ensure_untainted()?;

                    Ok(Callable::Function(function))
                }
            }
        }
    }

    /// Looks up all [`Method`]s and attempts to find one, which matches the
    /// signature of the given call expression.
    ///
    /// Methods returned by this method are checked for validity within the
    /// current context, including visibility, arguments and type arguments.
    /// To look up methods which only match the callee type and method name,
    /// see [`lume_infer::TyInferCtx::lookup_methods_on`].
    #[libftrace::traced(level = Trace, err)]
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
    /// Functions returned by this method are checked for validity within the
    /// current context, including visibility, arguments and type arguments.
    /// To look up functions which only match function name, see
    /// [`lume_infer::TyInferCtx::probe_functions`].
    #[libftrace::traced(level = Trace, err)]
    pub fn lookup_functions(&self, expr: &lume_hir::StaticCall) -> Result<&'_ Function> {
        let Callable::Function(probed_func) = self.probe_callable(lume_hir::CallExpression::Static(expr))? else {
            panic!("bug!: expected expression to yield a function, found method: {expr:#?}");
        };

        self.check_function(probed_func, expr)?;

        Ok(probed_func)
    }

    /// Looks up all [`Method`]s and attempts to find a single [`Method`], which
    /// matches the signature of the given instance call expression.
    ///
    /// Methods returned by this method are checked for validity within the
    /// current context, including visibility, arguments and type arguments.
    /// The look up methods which only match the callee type and method
    /// name, see [`lume_infer::TyInferCtx::lookup_methods_on`].
    ///
    /// For a generic callable lookup, see [`Self::lookup_callable`].
    /// For a static callable lookup, see
    /// [`Self::lookup_callable_static`].
    #[libftrace::traced(level = Trace, err)]
    pub fn lookup_callable_instance(&self, call: &lume_hir::InstanceCall) -> Result<Callable<'_>> {
        self.lookup_callable(lume_hir::CallExpression::Instanced(call))
    }

    /// Looks up all [`Callable`]s and attempts to find one, which matches the
    /// signature of the given static call expression.
    ///
    /// Callables returned by this method are checked for validity within the
    /// current context, including visibility, arguments and type arguments.
    /// To look up methods which only match the callee type and method name,
    /// see [`lume_infer::TyInferCtx::lookup_methods_on`].
    ///
    /// For a generic callable lookup, see [`Self::lookup_callable`].
    /// For an instance callable lookup, see
    /// [`Self::lookup_callable_instance`].
    #[libftrace::traced(level = Trace, err)]
    pub fn lookup_callable_static(&self, call: &lume_hir::StaticCall) -> Result<Callable<'_>> {
        self.lookup_callable(lume_hir::CallExpression::Static(call))
    }

    /// Determines whether the given call expression is a dynamic dispatch.
    #[libftrace::traced(level = Trace)]
    pub fn is_dynamic_dispatch(&self, expr: lume_hir::CallExpression) -> Result<bool> {
        let callable = self.lookup_callable(expr)?;

        // Handles static calls on type parameters, such as `T::default()`
        if callable.is_trait_definition() && self.is_generic_static(expr) {
            return Ok(false);
        }

        if callable.is_trait_definition() && self.hir_body_of_node(callable.id()).is_none() {
            return Ok(true);
        }

        Ok(false)
    }

    /// Ensures that the return type of a block matches the expected type.
    ///
    /// # Errors
    ///
    /// If not all branches return a value, or if different branches return
    /// different types, the method returns [`Err`].
    #[libftrace::traced(level = Trace, err, ret)]
    pub(crate) fn ensure_block_ty_match(&self, block: &lume_hir::Block, expected: &TypeRef) -> Result<()> {
        self.ensure_type_compatibility(&self.type_of_block_ret(block)?, expected)
    }

    /// Attempts to find the return type of a block. The return type is inferred
    /// from the last statement within the block, or `void` if empty.
    ///
    /// # Errors
    ///
    /// If not all branches return a value, or if different branches return
    /// different types, the method returns [`Err`].
    #[libftrace::traced(level = Trace, err, ret)]
    pub(crate) fn type_of_block_ret(&self, block: &lume_hir::Block) -> Result<TypeRef> {
        // If there's any `break`, `continue` or `return` statement directly within the
        // block, we need to return the value of those.
        //
        // For example, the following block should always return `Never`, since it has a
        // `break` statement in the middle:
        // ```lm
        // loop {
        //     // should return type of `Never` instead of `Boolean`
        //     let _ = {
        //         break;
        //         false
        //     };
        // }
        // ```
        for id in &block.statements {
            let stmt = self.hir_expect_stmt(*id);

            if let lume_hir::StatementKind::Break(_) | lume_hir::StatementKind::Continue(_) = &stmt.kind {
                return Ok(self
                    .never_type()
                    .expect("expected `Never` type to exist")
                    .with_location(stmt.location));
            }
        }

        let last_statement = block.statements.last();

        if let Some(stmt) = last_statement {
            let stmt = self.hir_expect_stmt(*stmt);

            self.matching_type_of_stmt(stmt)
        } else {
            Ok(TypeRef::void().with_location(block.location))
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
    /// within it's declared module. This also applies to any recursive calls
    /// this method makes, in the case of some expressions, such as
    /// assignments.
    #[libftrace::traced(level = Trace, err)]
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
    /// within it's declared module. This also applies to any recursive calls
    /// this method makes, in the case of some expressions, such as
    /// assignments.
    #[libftrace::traced(level = Trace, err)]
    pub(crate) fn matching_type_of_cond(&self, id: NodeId, cases: &[lume_hir::Condition]) -> Result<TypeRef> {
        let first_case = cases.first().expect("expected at least 1 case");
        let last_case = cases.last().expect("expected at least 1 case");

        let has_else_case = cases.iter().any(|case| case.condition.is_none());

        // If there is no `else` block, we expect the condition to return `void`,
        // since there would otherwise be no fallback.
        let expected_type = if has_else_case {
            self.type_of_condition_scope(first_case)?
        } else {
            self.expected_type_of(id)?
                .unwrap_or_else(|| TypeRef::void().with_location(first_case.location))
        };

        // Ensure that all branches return the same type.
        for case in cases {
            let found_type = self.type_of_condition_scope(case)?;

            if !self.check_type_compatibility(&found_type, &expected_type)? {
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
                }

                return Err(crate::check::errors::MismatchedTypesBranchesCondition {
                    found,
                    expected,
                    found_loc: found_type.location,
                }
                .into());
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

    /// Determines whether the given node is visible outside it's owning
    /// package.
    ///
    /// If no node with the given ID is found or if the node cannot hold
    /// a visibility modifier, returns `false`.
    #[cached_query]
    #[libftrace::traced(level = Trace)]
    pub fn is_visible_outside_package(&self, id: NodeId) -> bool {
        self.visibility_of(id) == Some(Visibility::Public)
    }

    /// Determines whether the node `a` can "see" node `b`, with regard to
    /// the visibility rules of `b`.
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn is_visible_to(&self, a: NodeId, b: NodeId) -> Result<bool> {
        // Since trait methods cannot have visibilities, we have them separately here.
        if let Some(Node::TraitMethodImpl(method_impl)) = self.hir_node(b) {
            let trait_def = self.trait_definition_of_method_impl(method_impl)?;

            return self.is_visible_to(a, trait_def.id);
        }

        if let Some(Node::TraitMethodDef(method_def)) = self.hir_node(b) {
            let trait_id = self.hir_parent_of(method_def.id).expect("expected trait def parent");
            let trait_def = self.hir_expect_trait(trait_id);

            return self.is_visible_to(a, trait_def.id);
        }

        let Some(b_vis) = self.visibility_of(b) else {
            return Err(diagnostics::CannotHoldVisibility {
                source: self.hir_span_of_node(b),
            }
            .into());
        };

        match b_vis {
            // Public items are always visible
            Visibility::Public => return Ok(true),

            // Internal items are visible to everything within the same package
            Visibility::Internal => return Ok(a.package == b.package),

            Visibility::Private => match self.hir_expect_node(b) {
                Node::Function(_) | Node::Type(_) => Ok(self.is_same_source(a, b)),

                Node::Field(field) => {
                    let struct_def = self.owning_struct_of_field(field.id)?;

                    // If the struct itself isn't visible, the field visibility won't matter.
                    if !self.is_visible_to(a, struct_def.id)? {
                        return Ok(false);
                    }

                    let Some(parent_type) = self.parent_type_of(a)? else {
                        return Ok(false);
                    };

                    if struct_def.id == parent_type.instance_of {
                        return Ok(true);
                    }

                    Ok(self.is_same_source(a, b))
                }

                Node::Method(method_def) => {
                    let impl_type = self.impl_type_of_method(method_def.id)?;

                    if let Some(parent_type) = self.parent_type_of(a)? {
                        return self.check_type_compatibility(&parent_type, &impl_type);
                    }

                    Ok(false)
                }

                Node::TraitMethodDef(_) | Node::TraitMethodImpl(_) => unreachable!("handled in start of method"),

                Node::TraitImpl(_) | Node::Impl(_) | Node::Pattern(_) | Node::Statement(_) | Node::Expression(_) => {
                    return Err(diagnostics::CannotHoldVisibility {
                        source: self.hir_span_of_node(b),
                    }
                    .into());
                }
            },
        }
    }

    /// Determines whether the given type `ty` is visible for the node `from`,
    /// with regard to the visibility rules of `ty`.
    ///
    /// This method also checks whether the type arguments within the type
    /// are visible to `from`.
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn is_type_visible_to(&self, ty: &TypeRef, from: NodeId) -> Result<bool> {
        // All standard types are always implicitly visible from any node.
        if !matches!(
            &self.tdb().expect_type(ty.instance_of)?.kind,
            lume_types::TypeKind::Struct | lume_types::TypeKind::Enum | lume_types::TypeKind::Trait
        ) {
            return Ok(true);
        }

        if !self.is_visible_to(from, ty.instance_of)? {
            return Ok(false);
        }

        for type_arg in &ty.bound_types {
            if !self.is_type_visible_to(type_arg, from)? {
                return Ok(false);
            }
        }

        Ok(true)
    }

    /// Determines whether the nodes `a` and `b` share the same source file.
    #[cached_query]
    #[libftrace::traced(level = Trace)]
    pub fn is_same_source(&self, a: NodeId, b: NodeId) -> bool {
        self.hir_span_of_node(a).file.id == self.hir_span_of_node(b).file.id
    }
}
