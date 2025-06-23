mod diagnostics;

use crate::{TyCheckCtx, *};
use error_snippet::Result;
use lume_hir::{self, Identifier};
use lume_infer::query::Callable;
use lume_types::{Function, Method};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum CallableCheckError {
    /// The name of the callable does not match the expected name, but it
    /// was similar enough to be considered a typo or a mistake.
    NameMismatch,

    /// The number of arguments provided does not match the expected number.
    ArgumentCountMismatch,

    /// The types of the arguments provided do not match the expected types.
    ArgumentTypeMismatch(usize),

    /// The number of type parameters provided does not match the expected number.
    TypeParameterCountMismatch,

    /// The expression which attempted to call the expression is instanced, while the
    /// method itself is statically declared.
    InstancedCallOnStaticMethod,
}

impl std::fmt::Display for CallableCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NameMismatch => f.write_str("name mismatch"),
            Self::ArgumentCountMismatch => f.write_str("argument count mismatch"),
            Self::ArgumentTypeMismatch(_) => f.write_str("argument type mismatch"),
            Self::TypeParameterCountMismatch => f.write_str("type parameter count mismatch"),
            Self::InstancedCallOnStaticMethod => f.write_str("instanced call to static method"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum CallableCheckResult {
    /// The [`Callable`] is valid within the context and
    /// is a suitable candiate for the given expression.
    Success,

    /// The [`Callable`] is invalid for one-or-more reasons, which
    /// are defined within the variant.
    Failure(Vec<CallableCheckError>),
}

#[derive(Clone, Debug)]
pub(crate) struct CallableLookupSuggestion<'a> {
    /// Defines the callable definition.
    pub def: Callable<'a>,

    /// Defines the reason for disqualification.
    pub reason: CallableCheckError,
}

#[derive(Debug)]
pub(crate) struct MethodLookupError<'a> {
    /// Defines the name of the type on which the method was being looked up.
    pub type_name: SymbolName,

    /// Defines the name of the method that was being looked up.
    pub method_name: Identifier,

    /// Defines a list of possible suggestions for the method lookup.
    pub suggestions: Vec<CallableLookupSuggestion<'a>>,
}

impl MethodLookupError<'_> {
    /// Compound the error and inner suggestions, if any, into a suitable error.
    pub fn into_compound_err(self) -> error_snippet::Error {
        let suggestions = self
            .suggestions
            .into_iter()
            .map(|suggestion| {
                let method_name = suggestion.def.name().clone();

                diagnostics::SuggestedMethod {
                    source: method_name.location.clone(),
                    type_name: self.type_name.clone(),
                    method_name: method_name.name,
                    reason: suggestion.reason,
                }
                .into()
            })
            .collect::<Vec<error_snippet::Error>>();

        diagnostics::MissingMethod {
            source: self.method_name.location.clone(),
            type_name: self.type_name,
            method_name: self.method_name,
            suggestions,
        }
        .into()
    }
}

#[derive(Debug)]
pub(crate) struct FunctionLookupError<'a> {
    /// Defines the name of the function that was being looked up.
    pub function_name: SymbolName,

    /// Defines a list of possible suggestions for the function lookup.
    pub suggestions: Vec<CallableLookupSuggestion<'a>>,
}

impl FunctionLookupError<'_> {
    /// Compound the error and inner suggestions, if any, into a suitable error.
    pub fn into_compound_err(self) -> error_snippet::Error {
        let suggestions = self
            .suggestions
            .into_iter()
            .map(|suggestion| {
                let function_name = suggestion.def.name().clone();

                diagnostics::SuggestedFunction {
                    source: function_name.location.clone(),
                    function_name: function_name.name,
                    reason: suggestion.reason,
                }
                .into()
            })
            .collect::<Vec<error_snippet::Error>>();

        diagnostics::MissingFunction {
            source: self.function_name.location.clone(),
            function_name: self.function_name.as_ident().clone(),
            suggestions,
        }
        .into()
    }
}

impl TyCheckCtx {
    /// Checks whether the given [`Method`] is valid, in terms of provided
    /// arguments, type arguments, visibility and type of callee.
    ///
    /// If the [`Method`] is not valid for the given expression, returns
    /// [`CallableCheckResult::Failure`] with one-or-more reasons.
    #[tracing::instrument(level = "TRACE", skip_all, err, ret)]
    pub(crate) fn check_method<'a>(
        &self,
        method: &'a Method,
        expr: &'a lume_hir::CallExpression,
    ) -> Result<CallableCheckResult> {
        let mut failures = Vec::new();
        let is_instance_method = method.is_instanced();

        // Verify that the amount of type arguments match the
        // expected number of type parameters.
        if method.type_parameters.len() != expr.type_arguments().len() {
            failures.push(CallableCheckError::TypeParameterCountMismatch);
        }

        let arguments = match (expr, is_instance_method) {
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
            (lume_hir::CallExpression::Instanced(call), true) => {
                &[&[call.callee.clone()][..], &call.arguments[..]].concat()
            }
            (lume_hir::CallExpression::Static(call), _) => &call.arguments,
            (lume_hir::CallExpression::Instanced(_), false) => {
                failures.push(CallableCheckError::InstancedCallOnStaticMethod);

                &vec![]
            }
        };

        if !failures.is_empty() {
            return Ok(CallableCheckResult::Failure(failures));
        }

        if let CallableCheckResult::Failure(err) = self.check_params(&method.parameters, arguments)? {
            failures.extend(err.iter());

            return Ok(CallableCheckResult::Failure(failures));
        }

        if failures.is_empty() {
            Ok(CallableCheckResult::Success)
        } else {
            Ok(CallableCheckResult::Failure(failures))
        }
    }

    /// Checks whether the given [`Function`] is valid, in terms of provided
    /// arguments, type arguments and visibility.
    ///
    /// If the [`Function`] is not valid for the given expression, returns
    /// [`CallableCheckResult::Failure`] with one-or-more reasons.
    #[tracing::instrument(level = "TRACE", skip_all, err, ret)]
    pub(crate) fn check_function<'a>(
        &self,
        function: &'a Function,
        expr: &'a lume_hir::StaticCall,
    ) -> Result<CallableCheckResult> {
        let mut failures = Vec::new();

        // Verify that the amount of type arguments match the
        // expected number of type parameters.
        if function.type_parameters.len() != expr.type_arguments.len() {
            failures.push(CallableCheckError::TypeParameterCountMismatch);

            return Ok(CallableCheckResult::Failure(failures));
        }

        if let CallableCheckResult::Failure(err) = self.check_params(&function.parameters, &expr.arguments)? {
            failures.extend(err.iter());

            return Ok(CallableCheckResult::Failure(failures));
        }

        if failures.is_empty() {
            Ok(CallableCheckResult::Success)
        } else {
            Ok(CallableCheckResult::Failure(failures))
        }
    }

    /// Checks whether the given arguments matches the signature of the given
    /// parameters.
    #[tracing::instrument(level = "TRACE", skip_all, err, ret)]
    pub(crate) fn check_params<'a>(
        &self,
        parameters: &'a lume_types::Parameters,
        arguments: &'a [lume_hir::Expression],
    ) -> Result<CallableCheckResult> {
        let mut failures = Vec::new();

        // Verify that the expected argument count is met, as defined
        // by the parameter definition.
        if (parameters.is_vararg() && parameters.len() - 1 > arguments.len())
            || (!parameters.is_vararg() && parameters.len() != arguments.len())
        {
            failures.push(CallableCheckError::ArgumentCountMismatch);

            return Ok(CallableCheckResult::Failure(failures));
        }

        if parameters.is_vararg() {
            // If the parameter count is variable, expect at least
            // the amount of required parameters as arguments.
            //
            // If a function/method takes N parameters + 1 vararg parameter,
            // we expect at least N arguments.
            if parameters.len() - 1 > arguments.len() {
                failures.push(CallableCheckError::ArgumentCountMismatch);

                return Ok(CallableCheckResult::Failure(failures));
            }

            let fixed_param_count = parameters.len() - 1;

            let (vararg_param, fixed_params) = parameters.inner().split_last().unwrap();
            let (fixed_args, vararg_args) = arguments.split_at(fixed_param_count);

            // Verify that all the fixed parameters are compatible with the arguments
            // passed to the method.
            for (param, arg) in fixed_params.iter().zip(fixed_args.iter()) {
                let arg_type = self.type_of_expr(arg)?;

                if !self.check_type_compatibility(&arg_type, &param.ty)? {
                    failures.push(CallableCheckError::ArgumentTypeMismatch(param.idx));
                }
            }

            // Verify that the vararg parameter has the same type as all arguments,
            // which sits after the last fixed parameter.
            for arg in vararg_args {
                let arg_type = self.type_of_expr(arg)?;

                if !self.check_type_compatibility(&arg_type, &vararg_param.ty)? {
                    failures.push(CallableCheckError::ArgumentTypeMismatch(vararg_param.idx));
                }
            }
        } else {
            // If the parameter count is fixed, we need exactly
            // that amount of arguments.
            if parameters.len() != arguments.len() {
                failures.push(CallableCheckError::ArgumentCountMismatch);

                return Ok(CallableCheckResult::Failure(failures));
            }

            // Verify that all the parameters are compatible with the arguments
            // passed to the method.
            for (param, arg) in parameters.inner().iter().zip(arguments.iter()) {
                let arg_type = self.type_of_expr(arg)?;

                if !self.check_type_compatibility(&arg_type, &param.ty)? {
                    failures.push(CallableCheckError::ArgumentTypeMismatch(param.idx));
                }
            }
        }

        if failures.is_empty() {
            Ok(CallableCheckResult::Success)
        } else {
            Ok(CallableCheckResult::Failure(failures))
        }
    }

    /// Looks up all [`Method`]s and [`Function`]s and attempts to find one, which matches the
    /// signature of the given call expression.
    ///
    /// Callables returned by this method are checked for validity within the current
    /// context, including visibility, arguments and type arguments. To look up methods
    /// which only match the callee type and method name, see [`ThirBuildCtx::lookup_methods_on()`].
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub(crate) fn lookup_callable(&self, expr: &lume_hir::CallExpression) -> Result<Callable<'_>> {
        match &expr {
            lume_hir::CallExpression::Instanced(call) => {
                let callee_type = self.infer.type_of(call.callee.id)?;
                let method = self.lookup_methods(expr, &callee_type)?;

                Ok(Callable::Method(method))
            }
            lume_hir::CallExpression::Static(call) => {
                if let Some(callee_ty_name) = call.name.clone().parent() {
                    let callee_type = self.infer.find_type_ref(&callee_ty_name)?.unwrap();
                    let method = self.lookup_methods(expr, &callee_type)?;

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
    pub(crate) fn lookup_methods(
        &self,
        expr: &lume_hir::CallExpression,
        callee_type: &lume_types::TypeRef,
    ) -> Result<&'_ Method> {
        let method_name = match &expr {
            lume_hir::CallExpression::Instanced(call) => call.name.identifier(),
            lume_hir::CallExpression::Static(call) => call.name.name.identifier(),
        };

        let mut suggestions = Vec::new();

        for method in self.infer.lookup_methods_on(callee_type, method_name) {
            if let CallableCheckResult::Failure(failures) = self.check_method(method, expr)? {
                suggestions.push((CallReference::Method(method.id), failures));

                continue;
            }

            return Ok(method);
        }

        for suggestion in self.infer.lookup_method_suggestions(callee_type, method_name) {
            let failures = if let CallableCheckResult::Failure(failures) = self.check_method(suggestion, expr)? {
                // We're explicitly removing name mismatches, as they should
                // not have matching names in suggested methods.
                failures
                    .into_iter()
                    .filter(|f| matches!(f, CallableCheckError::NameMismatch))
                    .collect()
            } else {
                vec![]
            };

            suggestions.push((CallReference::Method(suggestion.id), failures));
        }

        // Sort the suggestions, so the most plausible method is reported
        // to the user.
        self.sort_callable_suggestions(&mut suggestions);

        let diagnostic_suggestions = suggestions
            .into_iter()
            .map(|(call, errors)| {
                let CallReference::Method(method_id) = call else {
                    panic!();
                };

                CallableLookupSuggestion {
                    def: Callable::Method(self.tdb().method(method_id).unwrap()),
                    reason: *errors.first().unwrap_or(&CallableCheckError::NameMismatch),
                }
            })
            .collect::<Vec<_>>();

        Err(MethodLookupError {
            type_name: self.infer.type_ref_name(callee_type)?.clone(),
            method_name: method_name.clone(),
            suggestions: diagnostic_suggestions,
        }
        .into_compound_err())
    }

    /// Looks up all [`Function`]s and attempts to find one, which matches the
    /// signature of the given call expression.
    ///
    /// Functions returned by this method are checked for validity within the current
    /// context, including visibility, arguments and type arguments. To look up functions
    /// which only match function name, see [`ThirBuildCtx::lookup_functions_unchecked()`].
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub(crate) fn lookup_functions(&self, expr: &lume_hir::StaticCall) -> Result<&'_ Function> {
        let function_name = &expr.name;
        let mut suggestions = Vec::new();

        for function in self.infer.lookup_functions_unchecked(function_name) {
            if let CallableCheckResult::Failure(failures) = self.check_function(function, expr)? {
                suggestions.push((CallReference::Function(function.id), failures));

                continue;
            }

            return Ok(function);
        }

        for suggestion in self.infer.lookup_function_suggestions(function_name) {
            let failures = if let CallableCheckResult::Failure(failures) = self.check_function(suggestion, expr)? {
                // We're explicitly removing name mismatches, as they should
                // not have matching names in suggested methods.
                failures
                    .into_iter()
                    .filter(|f| matches!(f, CallableCheckError::NameMismatch))
                    .collect()
            } else {
                vec![]
            };

            suggestions.push((CallReference::Function(suggestion.id), failures));
        }

        // Sort the suggestions, so the most plausible method is reported
        // to the user.
        self.sort_callable_suggestions(&mut suggestions);

        let diagnostic_suggestions = suggestions
            .into_iter()
            .map(|(call, errors)| {
                let CallReference::Function(function_id) = call else {
                    panic!();
                };

                CallableLookupSuggestion {
                    def: Callable::Function(self.tdb().function(function_id).unwrap()),
                    reason: *errors.first().unwrap_or(&CallableCheckError::NameMismatch),
                }
            })
            .collect::<Vec<_>>();

        Err(FunctionLookupError {
            function_name: function_name.clone(),
            suggestions: diagnostic_suggestions,
        }
        .into_compound_err())
    }

    /// Sorts the given [`Callable`] suggestions after which [`Callable`] seems the most
    /// "plausable", given the reasons for disqualification. The array is sorted
    /// so the first lement is the most plausible [`Callable`] and the last element is
    /// the least plausible.
    #[allow(clippy::unused_self)]
    fn sort_callable_suggestions(&self, suggestions: &mut Vec<(CallReference, Vec<CallableCheckError>)>) {
        fn score_of_callable(_: CallReference, errors: &[CallableCheckError]) -> usize {
            let mut score = 0;

            for error in errors {
                score += match error {
                    CallableCheckError::NameMismatch => 0,
                    CallableCheckError::ArgumentCountMismatch => 1,
                    CallableCheckError::TypeParameterCountMismatch => 2,
                    CallableCheckError::InstancedCallOnStaticMethod => 3,
                    CallableCheckError::ArgumentTypeMismatch(_) => 4,
                };
            }

            score
        }

        // Sort the suggestions themselves
        suggestions.sort_by(|a, b| score_of_callable(a.0, &a.1).cmp(&score_of_callable(b.0, &b.1)));

        // And sort the errors, as well.
        for (_, errors) in suggestions {
            errors.sort();
        }
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
    pub(crate) fn lookup_callable_instance(&self, call: &lume_hir::InstanceCall) -> Result<Callable<'_>> {
        self.lookup_callable(&lume_hir::CallExpression::Instanced(call))
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
    pub(crate) fn lookup_callable_static(&self, call: &lume_hir::StaticCall) -> Result<Callable<'_>> {
        self.lookup_callable(&lume_hir::CallExpression::Static(call))
    }
}
