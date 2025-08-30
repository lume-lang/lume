use crate::TyInferCtx;
use crate::query::{CallReference, Callable};

use error_snippet::{IntoDiagnostic, Result};
use levenshtein::levenshtein;
use lume_hir::{self, Identifier, Node, Path};
use lume_types::{Function, FunctionSigOwned, Method, MethodKind, TypeKind, TypeRef};

use super::diagnostics::{self};

/// Defines the maximum Levenshtein distance allowed for method name suggestions.
pub const MAX_LEVENSHTEIN_DISTANCE: usize = 3;

/// Represents the option of whether a lookup should include
/// blanket implementations or not.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlanketLookup {
    /// Include blanket implementations in the lookup.
    Include,

    /// Exclude blanket implementations from the lookup.
    Exclude,
}

impl TyInferCtx {
    /// Gets the [`Callable`] with the given ID.
    #[tracing::instrument(level = "TRACE", skip_all, err, ret)]
    pub fn callable_of(&self, id: CallReference) -> Result<Callable<'_>> {
        match id {
            CallReference::Method(id) => {
                let method = self
                    .tdb()
                    .method(id)
                    .ok_or_else(|| lume_types::errors::MethodNotFound { id }.into_diagnostic())?;

                Ok(Callable::Method(method))
            }
            CallReference::Function(id) => {
                let func = self
                    .tdb()
                    .function(id)
                    .ok_or_else(|| lume_types::errors::FunctionNotFound { id }.into_diagnostic())?;

                Ok(Callable::Function(func))
            }
        }
    }

    /// Looks up all [`Method`]s on the given [`TypeRef`] of name `name`, which are
    /// implemented via an `impl` tag on the type.
    ///
    /// Methods returned by this method are not checked for validity within the current
    /// context, such as visibility, arguments or type arguments. To check whether any given
    /// [`Method`] is valid for a given context, see [`ThirBuildCtx::check_method()`].
    #[tracing::instrument(level = "TRACE", skip_all)]
    pub fn lookup_impl_methods_on(
        &self,
        ty: &'_ lume_types::TypeRef,
        name: &'_ Identifier,
    ) -> impl Iterator<Item = &'_ Method> {
        self.methods_defined_on(ty)
            .filter(move |method| method.name.name() == name)
    }

    /// Looks up all [`Method`]s on the given [`TypeRef`] of name `name`, which are
    /// implemented through trait implementations.
    ///
    /// Methods returned by this method are not checked for validity within the current
    /// context, such as visibility, arguments or type arguments. To check whether any given
    /// [`Method`] is valid for a given context, see [`ThirBuildCtx::check_method()`].
    #[tracing::instrument(level = "TRACE", skip_all)]
    pub fn lookup_trait_methods_on(
        &self,
        ty: &'_ TypeRef,
        name: &'_ Identifier,
        blanket_lookup: BlanketLookup,
    ) -> Vec<&'_ Method> {
        match blanket_lookup {
            BlanketLookup::Exclude => self
                .tdb()
                .uses_on(ty)
                .filter_map(|trait_impl| {
                    for method_id in &trait_impl.methods {
                        let method = self.tdb().method(*method_id).unwrap();

                        if method.name.name() == name {
                            return Some(method);
                        }
                    }

                    None
                })
                .collect(),
            BlanketLookup::Include => {
                let mut methods = self
                    .tdb()
                    .methods()
                    .filter(|method| {
                        matches!(
                            method.kind,
                            MethodKind::TraitDefinition | MethodKind::TraitImplementation
                        ) && method.name.name() == name
                    })
                    .collect::<Vec<_>>();

                // We need to prioritize the methods defined in the trait implementation,
                // as any method defined on a trait definition should only function as a fallback.
                //
                // The `MethodKind` enum is implemented via derive-macro and will place any
                // trait implementation methods before trait definition methods.
                methods.sort_by_key(|m| m.kind);

                methods
            }
        }
    }

    /// Looks up all [`Method`]s on the given [`TypeRef`] of name `name`.
    ///
    /// Methods returned by this method are not checked for validity within the current
    /// context, such as visibility, arguments or type arguments. To check whether any given
    /// [`Method`] is valid for a given context, see [`ThirBuildCtx::check_method()`].
    #[tracing::instrument(level = "TRACE", skip_all)]
    pub fn lookup_methods_on(
        &self,
        ty: &'_ lume_types::TypeRef,
        name: &'_ Identifier,
        blanket_lookup: BlanketLookup,
    ) -> Vec<&'_ Method> {
        let direct_impl = self.lookup_impl_methods_on(ty, name);

        match blanket_lookup {
            BlanketLookup::Exclude => self.lookup_impl_methods_on(ty, name).collect(),
            BlanketLookup::Include => {
                let trait_impl = self.lookup_trait_methods_on(ty, name, blanket_lookup);

                direct_impl.chain(trait_impl).collect()
            }
        }
    }

    /// Looks up the first [`Method`] on the given [`TypeRef`] of name `name`.
    ///
    /// The method returned by this method is not checked for validity within the current
    /// context, such as visibility, arguments or type arguments. To check whether any given
    /// [`Method`] is valid for a given context, see [`ThirBuildCtx::check_method()`].
    #[tracing::instrument(level = "TRACE", skip_all)]
    pub fn lookup_method_on(&self, ty: &'_ lume_types::TypeRef, name: &'_ Identifier) -> Option<&Method> {
        // First check whether any method is defined directly on the type
        let methods = self.lookup_methods_on(ty, name, BlanketLookup::Exclude);

        match methods.first() {
            Some(method) => Some(method),

            // If not, attempt to look for blanket implementations as well.
            None => self
                .lookup_methods_on(ty, name, BlanketLookup::Include)
                .first()
                .map(|m| *m),
        }
    }

    /// Looks up all [`Method`]s on the given [`TypeRef`], where the name isn't an
    /// exact match to `name`, but not too disimilar.
    ///
    /// Methods returned by this method are not checked for validity within the current
    /// context, such as visibility, arguments or type arguments. To check whether any given
    /// [`Method`] is otherwise valid for a given context, see [`ThirBuildCtx::check_method()`].
    #[tracing::instrument(level = "TRACE", skip_all)]
    pub fn lookup_method_suggestions(&self, ty: &lume_types::TypeRef, name: &Identifier) -> Vec<&'_ Method> {
        self.methods_defined_on(ty)
            .filter(|method| {
                let expected = &name.name;
                let actual = &method.name.name.name().name;
                let distance = levenshtein(expected, actual);

                distance != 0 && distance < MAX_LEVENSHTEIN_DISTANCE
            })
            .collect()
    }

    /// Folds all the functions which could be suggested from the given call expression
    /// into a single, emittable error message.
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn fold_function_suggestions<'a>(&self, expr: &lume_hir::StaticCall) -> Result<diagnostics::MissingFunction> {
        let suggestion: Option<Result<error_snippet::Error>> =
            self.lookup_function_suggestions(&expr.name).first().map(|suggestion| {
                let function_name = suggestion.name.clone();

                Ok(diagnostics::SuggestedFunction {
                    source: function_name.location.file.clone(),
                    range: function_name.location.index.clone(),
                    function_name: function_name.name,
                }
                .into())
            });

        let suggestions = if let Some(suggested) = suggestion {
            vec![suggested?]
        } else {
            Vec::new()
        };

        Ok(diagnostics::MissingFunction {
            source: expr.name.location,
            function_name: expr.name.name().clone(),
            suggestions,
        })
    }

    /// Folds all the methods which could be suggested from the given call expression
    /// into a single, emittable error message.
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn fold_method_suggestions<'a>(&self, expr: lume_hir::CallExpression) -> Result<diagnostics::MissingMethod> {
        let name = match expr {
            lume_hir::CallExpression::Static(call) => &call.name.name,
            lume_hir::CallExpression::Instanced(call) => &call.name,
            lume_hir::CallExpression::Intrinsic(call) => &call.name,
        };

        let callee_type = match expr {
            lume_hir::CallExpression::Static(call) => {
                self.find_type_ref(&call.name.clone().parent().unwrap())?.unwrap()
            }
            lume_hir::CallExpression::Instanced(call) => self.type_of(call.callee)?,
            lume_hir::CallExpression::Intrinsic(call) => self.type_of(call.callee())?,
        };

        let suggestion: Option<Result<error_snippet::Error>> = self
            .lookup_method_suggestions(&callee_type, name.name())
            .first()
            .map(|suggestion| {
                let method_name = suggestion.name.clone();

                Ok(diagnostics::SuggestedMethod {
                    source: method_name.location,
                    method_name: method_name.name,
                }
                .into())
            });

        let suggestions = if let Some(suggested) = suggestion {
            vec![suggested?]
        } else {
            Vec::new()
        };

        Ok(diagnostics::MissingMethod {
            source: name.location(),
            type_name: self.new_named_type(&callee_type, false)?,
            method_name: name.name().clone(),
            suggestions,
        })
    }

    /// Looks up all [`Function`]s, where the name isn't an exact match to `name`,
    /// but not too disimilar.
    ///
    /// Functions returned by this method are not checked for validity within the current
    /// context, such as visibility, arguments or type arguments. To check whether any given
    /// [`Function`] is valid for a given context, see [`ThirBuildCtx::check_function()`].
    #[tracing::instrument(level = "TRACE", skip(self), fields(name = %name))]
    pub fn lookup_function_suggestions(&self, name: &Path) -> Vec<&'_ Function> {
        self.tdb()
            .functions()
            .filter(|func| {
                // The namespaces on the function names must match,
                // so we don't match functions outside of the the
                // expected namespace.
                if name.root != func.name.root {
                    return false;
                }

                let expected = &name.name.name().as_str();
                let actual = &func.name.name().as_str();
                let distance = levenshtein(expected, actual);

                distance != 0 && distance < MAX_LEVENSHTEIN_DISTANCE
            })
            .collect()
    }

    /// Looks up all registered [`Functions`]s of name `name`.
    ///
    /// Functions returned by this method are not checked for validity within the current
    /// context, such as visibility, arguments or type arguments. To check whether any given
    /// [`Function`] is valid for a given context, see [`ThirBuildCtx::check_function()`].
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn probe_functions(&self, name: &Path) -> Vec<&'_ Function> {
        self.tdb().functions().filter(|func| &func.name == name).collect()
    }

    /// Looks up all registered [`Method`]s and [`Functions`]s of name `name`.
    ///
    /// Functions returned by this method are not checked for validity within the current
    /// context, such as visibility, arguments or type arguments.
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub fn probe_callable(&self, expr: lume_hir::CallExpression) -> Result<Callable<'_>> {
        match expr {
            expr @ lume_hir::CallExpression::Instanced(call) => {
                let callee_type = self.type_of(call.callee)?;
                let method = self.lookup_method_on(&callee_type, call.name.name());

                let Some(method) = method else {
                    let missing_method_err = self.fold_method_suggestions(expr)?;

                    return Err(missing_method_err.into());
                };

                Ok(Callable::Method(method))
            }
            expr @ lume_hir::CallExpression::Intrinsic(call) => {
                let callee_type = self.type_of(call.callee())?;
                let method = self.lookup_method_on(&callee_type, call.name.name());

                let Some(method) = method else {
                    let missing_method_err = self.fold_method_suggestions(expr)?;

                    return Err(missing_method_err.into());
                };

                Ok(Callable::Method(method))
            }
            lume_hir::CallExpression::Static(call) => {
                if let Some(callee_ty_name) = call.name.clone().parent()
                    && callee_ty_name.is_type()
                {
                    let callee_type = self.find_type_ref(&callee_ty_name)?.unwrap();
                    let method = self.lookup_method_on(&callee_type, call.name.name());

                    let Some(method) = method else {
                        let missing_method_err = self.fold_method_suggestions(expr)?;

                        return Err(missing_method_err.into());
                    };

                    Ok(Callable::Method(method))
                } else {
                    let functions = self.probe_functions(&call.name);

                    let Some(function) = functions.first() else {
                        let missing_func_err = self.fold_function_suggestions(call)?;

                        return Err(missing_func_err.into());
                    };

                    Ok(Callable::Function(function))
                }
            }
        }
    }

    /// Looks up all [`Method`]s and attempts to find a single [`Method`], which matches the
    /// signature of the given instance call expression.
    ///
    /// Methods returned by this method are checked for validity within the current
    /// context, including visibility, arguments and type arguments. The look up methods
    /// which only match the callee type and method name, see [`TyInferCtx::lookup_methods_on()`].
    ///
    /// For a generic callable lookup, see [`TyInferCtx::lookup_callable()`]. For a static callable
    /// lookup, see [`TyInferCtx::lookup_callable_static()`].
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub fn probe_callable_instance(&self, call: &lume_hir::InstanceCall) -> Result<Callable<'_>> {
        self.probe_callable(lume_hir::CallExpression::Instanced(call))
    }

    /// Looks up all [`Method`]s and attempts to find a single [`Method`], which matches the
    /// signature of the given intrinsic call expression.
    ///
    /// Methods returned by this method are checked for validity within the current
    /// context, including visibility, arguments and type arguments. The look up methods
    /// which only match the callee type and method name, see [`TyInferCtx::lookup_methods_on()`].
    ///
    /// For a generic callable lookup, see [`TyInferCtx::lookup_callable()`]. For a static callable
    /// lookup, see [`TyInferCtx::lookup_callable_static()`].
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub fn probe_callable_intrinsic(&self, call: &lume_hir::IntrinsicCall) -> Result<Callable<'_>> {
        self.probe_callable(lume_hir::CallExpression::Intrinsic(call))
    }

    /// Looks up all [`Callable`]s and attempts to find one, which matches the
    /// signature of the given static call expression.
    ///
    /// Callables returned by this method are checked for validity within the current
    /// context, including visibility, arguments and type arguments. To look up methods
    /// which only match the callee type and method name, see [`TyInferCtx::lookup_methods_on()`].
    ///
    /// For a generic callable lookup, see [`TyInferCtx::lookup_callable()`]. For an instance callable
    /// lookup, see [`TyInferCtx::lookup_callable_instance()`].
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub fn probe_callable_static(&self, call: &lume_hir::StaticCall) -> Result<Callable<'_>> {
        self.probe_callable(lume_hir::CallExpression::Static(call))
    }

    /// Instantiates a call expression against the given function signature, resolving
    /// any type arguments within and returning the instantiated signature.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn instantiate_call_expression<'a>(
        &self,
        signature: lume_types::FunctionSig<'a>,
        expr: lume_hir::CallExpression<'a>,
    ) -> Result<lume_types::FunctionSigOwned> {
        let type_arguments = self.type_args_in_call(expr)?;

        Ok(self.instantiate_function(signature, &type_arguments))
    }

    /// Instantiates a function signature against the given type arguments, resolving
    /// the parameters and return type within the signature.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn instantiate_function<'a>(
        &self,
        sig: lume_types::FunctionSig<'a>,
        type_args: &[TypeRef],
    ) -> lume_types::FunctionSigOwned {
        self.instantiate_signature_isolate(sig, sig.type_params, type_args)
    }

    /// Instantiates a function signature against the given type arguments, resolving
    /// the parameters and return type within the signature.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn instantiate_signature_isolate<'a>(
        &self,
        sig: lume_types::FunctionSig<'a>,
        type_params: &[lume_hir::TypeParameterId],
        type_args: &[TypeRef],
    ) -> lume_types::FunctionSigOwned {
        let mut inst = lume_types::FunctionSigOwned {
            params: lume_types::Parameters::new(),
            ret_ty: TypeRef::unknown(),
            type_params: Vec::new(),
        };

        for param in sig.params.inner() {
            let param_ty = self.instantiate_type_from(&param.ty, type_params, type_args);

            inst.params.params.push(lume_types::Parameter {
                idx: param.idx,
                name: param.name.clone(),
                ty: param_ty,
                vararg: param.vararg,
                location: param.location,
            });
        }

        inst.ret_ty = self.instantiate_type_from(sig.ret_ty, type_params, type_args);

        inst
    }

    /// Instantiates a a single type reference against the given type arguments.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn instantiate_type_from<'a>(
        &self,
        ty: &'a lume_types::TypeRef,
        type_params: &[lume_hir::TypeParameterId],
        type_args: &'a [TypeRef],
    ) -> lume_types::TypeRef {
        let mut inst_ty = self.instantiate_flat_type_from(ty, type_params, type_args).to_owned();

        inst_ty.type_arguments.clear();
        inst_ty.type_arguments.reserve_exact(ty.type_arguments.len());

        for type_arg in &ty.type_arguments {
            let inst_type_arg = self.instantiate_type_from(type_arg, type_params, type_args).to_owned();

            inst_ty.type_arguments.push(inst_type_arg);
        }

        inst_ty
    }

    /// Instantiates a a single flat type reference against the given type arguments.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn instantiate_flat_type_from<'a>(
        &self,
        ty: &'a lume_types::TypeRef,
        type_params: &[lume_hir::TypeParameterId],
        type_args: &'a [TypeRef],
    ) -> &'a lume_types::TypeRef {
        let Some(ty_as_param) = self.db().type_as_param(ty.instance_of) else {
            return ty;
        };

        for (type_param, type_arg) in type_params.iter().zip(type_args.iter()) {
            if *type_param == ty_as_param.id {
                return type_arg;
            }
        }

        ty
    }

    /// Gets all the type arguments defined within the given call expression.
    ///
    /// Type arguments are fetched from the expression itself, as well as type arguments
    /// defined on the callee of the exprssion, if any.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn type_args_in_call(&self, expr: lume_hir::CallExpression) -> Result<Vec<TypeRef>> {
        let type_parameters_hir = self.hir_avail_type_params_expr(expr.id());
        let type_parameters = type_parameters_hir.iter().map(AsRef::as_ref).collect::<Vec<_>>();

        match &expr {
            lume_hir::CallExpression::Static(call) => {
                let hir_type_args = &call.all_type_arguments();

                self.mk_type_refs_generic(hir_type_args, &type_parameters)
            }
            lume_hir::CallExpression::Instanced(_) | lume_hir::CallExpression::Intrinsic(_) => {
                let callee = match expr {
                    lume_hir::CallExpression::Instanced(call) => call.callee,
                    lume_hir::CallExpression::Intrinsic(call) => call.callee(),
                    lume_hir::CallExpression::Static(_) => unreachable!(),
                };

                let hir_type_args = expr.type_arguments();
                let mut type_args = self.mk_type_refs_generic(hir_type_args, &type_parameters)?;

                let callee_type = self.type_of(callee)?;
                type_args.extend(callee_type.type_arguments.into_iter());

                Ok(type_args)
            }
        }
    }

    /// Gets the expanded signature of the given [`Callable`].
    ///
    /// The expanded signature of a [`Callable`] will include all the type parameters
    /// defined on parent definitions, such as on implementation blocks.
    #[tracing::instrument(level = "TRACE", skip_all, err, ret)]
    pub fn signature_of(&self, callable: Callable) -> Result<FunctionSigOwned> {
        match callable {
            Callable::Method(method) => {
                let mut signature = method.sig().to_owned();

                // Append all the type parameters which are available on the method,
                // such as the type parameters on the implementation.
                signature.type_params.extend(
                    self.hir_avail_type_params(method.hir)
                        .into_iter()
                        .filter_map(|param| param.type_param_id),
                );

                Ok(signature)
            }
            Callable::Function(function) => Ok(function.sig().to_owned()),
        }
    }

    /// Gets the expanded signature of the [`Callable`] with the given ID.
    ///
    /// The expanded signature of a [`Callable`] will include all the type parameters
    /// defined on parent definitions, such as on implementation blocks.
    #[tracing::instrument(level = "TRACE", skip_all, err, ret)]
    pub fn signature_of_call_ref(&self, id: CallReference) -> Result<FunctionSigOwned> {
        let callable = self.callable_of(id)?;

        self.signature_of(callable)
    }

    /// Gets the instantiated signature of the given [`Callable`], w.r.t the given expression.
    ///
    /// The instantiated signature is a version of a signature where all resolvable type
    /// arguments within the expression have been resolved. For example, given an expression:
    ///
    /// ```lm
    /// fn foo<T>(val: T) -> T {
    ///     return val;
    /// }
    ///
    /// let a = foo<Boolean>(false);
    /// ```
    ///
    /// The resulting variable `a` will have the type of `Boolean`, since it was resolved from the
    /// type arguments on the callable expression.
    #[tracing::instrument(level = "TRACE", skip_all, fields(callable = %callable.name()), err, ret)]
    pub fn signature_of_instantiated(
        &self,
        callable: Callable,
        expr: lume_hir::CallExpression,
    ) -> Result<FunctionSigOwned> {
        let full_signature = self.signature_of(callable)?;

        self.instantiate_call_expression(full_signature.as_ref(), expr)
    }

    /// Returns all the methods defined directly within the given type.
    ///
    /// Methods returned by this method are not checked for validity within the current
    /// context, such as visibility, arguments or type arguments. To check whether any given
    /// [`Method`] is valid for a given context, see [`ThirBuildCtx::check_method()`].
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn methods_defined_on(&self, self_ty: &lume_types::TypeRef) -> impl Iterator<Item = &'_ Method> {
        self.tdb().methods_on(self_ty.instance_of)
    }

    /// Determines whether the given [`TypeRef`] is a kind of [`TypeKindRef::TypeParameter`].
    #[tracing::instrument(level = "TRACE", skip(self), err, ret)]
    pub fn is_type_parameter(&self, ty: &TypeRef) -> Result<bool> {
        match self.tdb().ty_expect(ty.instance_of)?.kind {
            TypeKind::TypeParameter(_) => Ok(true),
            _ => Ok(false),
        }
    }

    /// Determines whether the given [`TypeRef`] is a kind of [`TypeKindRef::TypeParameter`].
    #[tracing::instrument(level = "TRACE", skip(self), err, ret)]
    pub fn as_type_parameter(&self, ty: &TypeRef) -> Result<Option<&lume_types::TypeParameter>> {
        match self.tdb().ty_expect(ty.instance_of)?.kind {
            TypeKind::TypeParameter(id) => Ok(self.tdb().type_parameter(id)),
            _ => Ok(None),
        }
    }
}
