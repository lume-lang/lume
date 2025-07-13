use crate::{TyInferCtx, query::Callable};
use error_snippet::Result;
use levenshtein::levenshtein;
use lume_hir::{self, Identifier, Node, Path};
use lume_types::{Function, Method, TypeRef};

use super::diagnostics::{self};

/// Defines the maximum Levenshtein distance allowed for method name suggestions.
pub const MAX_LEVENSHTEIN_DISTANCE: usize = 3;

impl TyInferCtx {
    /// Looks up all [`Method`]s on the given [`TypeRef`] of name `name`.
    ///
    /// Methods returned by this method are not checked for validity within the current
    /// context, such as visibility, arguments or type arguments. To check whether any given
    /// [`Method`] is valid for a given context, see [`ThirBuildCtx::check_method()`].
    #[tracing::instrument(level = "TRACE", skip_all)]
    pub fn lookup_methods_on<'a>(&self, ty: &'a lume_types::TypeRef, name: &'a Identifier) -> Vec<&'_ Method> {
        self.methods_defined_on(ty)
            .filter(|method| method.name.name() == name)
            .collect()
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
            lume_hir::CallExpression::Instanced(call) => self.type_of(call.callee.id)?,
            lume_hir::CallExpression::Intrinsic(call) => self.type_of(call.callee().id)?,
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
            type_name: self.new_named_type(&callee_type)?,
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
                let callee_type = self.type_of(call.callee.id)?;
                let methods = self.lookup_methods_on(&callee_type, call.name.name());

                let Some(method) = methods.first() else {
                    let missing_method_err = self.fold_method_suggestions(expr)?;

                    return Err(missing_method_err.into());
                };

                Ok(Callable::Method(method))
            }
            expr @ lume_hir::CallExpression::Intrinsic(call) => {
                let callee_type = self.type_of(call.callee().id)?;
                let methods = self.lookup_methods_on(&callee_type, call.name.name());

                let Some(method) = methods.first() else {
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
                    let methods = self.lookup_methods_on(&callee_type, call.name.name());

                    let Some(method) = methods.first() else {
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
        let type_parameters_hir = self.hir_avail_type_params_expr(expr.id());
        let type_arguments_hir = match &expr {
            lume_hir::CallExpression::Static(call) => &call.all_type_arguments(),
            _ => expr.type_arguments(),
        };

        let type_arguments = self.mk_type_refs_generic(type_arguments_hir, &type_parameters_hir)?;

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
        let mut inst = lume_types::FunctionSigOwned {
            params: lume_types::Parameters::new(),
            ret_ty: TypeRef::unknown(),
            type_params: Vec::new(),
        };

        for param in sig.params.inner() {
            let param_ty = self.instantiate_type_from(&param.ty, sig.type_params, type_args);

            inst.params.params.push(lume_types::Parameter {
                idx: param.idx,
                name: param.name.clone(),
                ty: param_ty.clone(),
                vararg: param.vararg,
            });
        }

        inst.ret_ty = self
            .instantiate_type_from(sig.ret_ty, sig.type_params, type_args)
            .clone();

        inst
    }

    /// Instantiates a a single type reference against the given type arguments.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn instantiate_type_from<'a>(
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

    /// Returns all the methods defined directly within the given type.
    ///
    /// Methods returned by this method are not checked for validity within the current
    /// context, such as visibility, arguments or type arguments. To check whether any given
    /// [`Method`] is valid for a given context, see [`ThirBuildCtx::check_method()`].
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn methods_defined_on(&self, self_ty: &lume_types::TypeRef) -> impl Iterator<Item = &'_ Method> {
        self.tdb().methods_on(self_ty.instance_of)
    }
}
