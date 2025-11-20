use error_snippet::{IntoDiagnostic, Result};
use levenshtein::levenshtein;
use lume_architect::cached_query;
use lume_hir::{Identifier, Path, WithLocation};
use lume_span::{Location, NodeId};
use lume_types::{Function, FunctionSigOwned, Method, MethodKind, TypeKind, TypeRef};

use super::diagnostics::{self};
use crate::TyInferCtx;
use crate::query::{CallReference, Callable};

/// Defines the maximum Levenshtein distance allowed for method name
/// suggestions.
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
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn callable_of(&self, id: CallReference) -> Result<Callable<'_>> {
        match id {
            CallReference::Method(id) => {
                let method = self
                    .tdb()
                    .method(id)
                    .ok_or_else(|| lume_types::errors::NodeNotFound { id }.into_diagnostic())?;

                Ok(Callable::Method(method))
            }
            CallReference::Function(id) => {
                let func = self
                    .tdb()
                    .function(id)
                    .ok_or_else(|| lume_types::errors::NodeNotFound { id }.into_diagnostic())?;

                Ok(Callable::Function(func))
            }
        }
    }

    /// Looks up all [`Method`]s on the given [`TypeRef`] of name `name`, which
    /// are implemented via an `impl` tag on the type.
    ///
    /// Methods returned by this method are not checked for validity within the
    /// current context, such as visibility, arguments or type arguments. To
    /// check whether any given [`Method`] is valid for a given context, see
    /// [`ThirBuildCtx::check_method()`].
    pub fn lookup_impl_methods_on(
        &self,
        ty: &'_ lume_types::TypeRef,
        name: &'_ Identifier,
    ) -> impl Iterator<Item = &'_ Method> {
        self.methods_defined_on(ty)
            .filter(move |method| method.name.name() == name)
    }

    /// Looks up all [`Method`]s on the given [`TypeRef`] of name `name`, which
    /// are implemented through trait implementations.
    ///
    /// Methods returned by this method are not checked for validity within the
    /// current context, such as visibility, arguments or type arguments. To
    /// check whether any given [`Method`] is valid for a given context, see
    /// [`ThirBuildCtx::check_method()`].
    #[libftrace::traced(level = Trace)]
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
                // as any method defined on a trait definition should only function as a
                // fallback.
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
    /// Methods returned by this method are not checked for validity within the
    /// current context, such as visibility, arguments or type arguments. To
    /// check whether any given [`Method`] is valid for a given context, see
    /// [`ThirBuildCtx::check_method()`].
    #[libftrace::traced(level = Trace)]
    pub fn lookup_methods_on<'a>(
        &'a self,
        ty: &lume_types::TypeRef,
        name: &Identifier,
        blanket_lookup: BlanketLookup,
    ) -> Vec<&'a Method> {
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
    /// The method returned by this method is not checked for validity within
    /// the current context, such as visibility, arguments or type
    /// arguments. To check whether any given [`Method`] is valid for a
    /// given context, see [`ThirBuildCtx::check_method()`].
    #[libftrace::traced(level = Trace, fields(ty, name = name.as_str()))]
    pub fn lookup_method_on<'a>(&'a self, ty: &lume_types::TypeRef, name: &Identifier) -> Option<&'a Method> {
        // First check whether any method is defined directly on the type
        let methods = self.lookup_methods_on(ty, name, BlanketLookup::Exclude);
        if let Some(idx) = self.find_local_method(&ty.location, &methods) {
            return Some(&methods[idx]);
        }

        // If not, attempt to look for blanket implementations as well.
        let methods = self.lookup_methods_on(ty, name, BlanketLookup::Include);
        let idx = self.find_local_method(&ty.location, &methods)?;

        Some(&methods[idx])
    }

    /// Find the index into `methods` with the method which is "closest" to the
    /// given location.
    ///
    /// First, the method attempts to find a method which is defined in the same
    /// file as `local_loc`. If none is found, it looks for a method which
    /// exists within the same package as `local_loc`. If that also fails,
    /// it returns the index of the first method in the slice.
    #[libftrace::traced(level = Trace)]
    fn find_local_method(&self, local_loc: &Location, methods: &[&Method]) -> Option<usize> {
        // Methods defined within the same file as the given location.
        if let Some(idx) = methods
            .iter()
            .position(|m| self.hir_span_of_node(m.id).file.id == local_loc.file.id)
        {
            return Some(idx);
        }

        // Methods defined within the same package as the given location.
        if let Some(idx) = methods
            .iter()
            .position(|m| self.hir_span_of_node(m.id).file.package == local_loc.file.package)
        {
            return Some(idx);
        }

        // Methods which are visible outside of their owning package
        if let Some(idx) = methods
            .iter()
            .position(|m| m.visibility == Some(lume_hir::Visibility::Public))
        {
            return Some(idx);
        }

        if methods.is_empty() { None } else { Some(0) }
    }

    /// Looks up the [`Method`] which matches the given intrinsic.
    ///
    /// The method returned by this method is not checked for validity within
    /// the current context, such as visibility, arguments or type
    /// arguments.
    #[libftrace::traced(level = Trace, err)]
    pub fn lookup_intrinsic_method(&self, expr: &lume_hir::IntrinsicCall) -> Result<Option<&Method>> {
        let (_, method_name) = self.lang_item_of_intrinsic(&expr.kind);

        let callee_type = self.type_of(expr.kind.callee())?;
        let method_name = Identifier {
            name: method_name.to_string(),
            location: expr.location,
        };

        self.lookup_method_on(&callee_type, &method_name).map(Ok).transpose()
    }

    /// Looks up all [`Method`]s on the given [`TypeRef`], where the name isn't
    /// an exact match to `name`, but not too disimilar.
    ///
    /// Methods returned by this method are not checked for validity within the
    /// current context, such as visibility, arguments or type arguments. To
    /// check whether any given [`Method`] is otherwise valid for a given
    /// context, see [`ThirBuildCtx::check_method()`].
    #[libftrace::traced(level = Trace)]
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

    /// Folds all the functions which could be suggested from the given call
    /// expression into a single, emittable error message.
    #[libftrace::traced(level = Trace)]
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

    /// Folds all the methods which could be suggested from the given call
    /// expression into a single, emittable error message.
    #[libftrace::traced(level = Trace)]
    fn fold_method_suggestions<'a>(&self, expr: lume_hir::CallExpression) -> Result<diagnostics::MissingMethod> {
        // We don't add method suggestions to intrinsic calls.
        if let lume_hir::CallExpression::Intrinsic(expr) = &expr {
            let callee_id = *expr.kind.arguments().first().unwrap();
            let callee_type = self.type_of(callee_id)?;

            return Ok(diagnostics::MissingMethod {
                source: expr.location.clone(),
                type_name: self.new_named_type(&callee_type, false)?,
                method_name: expr.name(),
                suggestions: Vec::new(),
            });
        }

        let name = match expr {
            lume_hir::CallExpression::Static(call) => &call.name.name,
            lume_hir::CallExpression::Instanced(call) => &call.name,
            lume_hir::CallExpression::Intrinsic(_) => unreachable!(),
        };

        let callee_type = match expr {
            lume_hir::CallExpression::Static(call) => {
                self.find_type_ref(&call.name.clone().parent().unwrap())?.unwrap()
            }
            lume_hir::CallExpression::Instanced(call) => self.type_of(call.callee)?,
            lume_hir::CallExpression::Intrinsic(_) => unreachable!(),
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

    /// Looks up all [`Function`]s, where the name isn't an exact match to
    /// `name`, but not too disimilar.
    ///
    /// Functions returned by this method are not checked for validity within
    /// the current context, such as visibility, arguments or type
    /// arguments. To check whether any given [`Function`] is valid for a
    /// given context, see [`ThirBuildCtx::check_function()`].
    #[libftrace::traced(level = Trace, fields(name))]
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
    /// Functions returned by this method are not checked for validity within
    /// the current context, such as visibility, arguments or type
    /// arguments. To check whether any given [`Function`] is valid for a
    /// given context, see [`ThirBuildCtx::check_function()`].
    #[libftrace::traced(level = Trace)]
    pub fn probe_functions(&self, name: &Path) -> Vec<&'_ Function> {
        self.tdb().functions().filter(|func| &func.name == name).collect()
    }

    /// Looks up all registered [`Method`]s and [`Functions`]s of name `name`.
    ///
    /// Functions returned by this method are not checked for validity within
    /// the current context, such as visibility, arguments or type
    /// arguments.
    #[libftrace::traced(level = Trace, err)]
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
                let method = self.lookup_intrinsic_method(&call)?;

                let Some(method) = method else {
                    let trait_name = self
                        .type_name_of_intrinsic(&call.kind)
                        .unwrap_or_else(|| panic!("expected intrinsic type to exist: {}", call.name()));

                    return Err(crate::errors::IntrinsicNotImplemented {
                        source: expr.location(),
                        trait_name: format!("{trait_name:+}"),
                        operation: self.operation_name_of_intrinsic(&call.kind),
                    }
                    .into());
                };

                Ok(Callable::Method(method))
            }
            lume_hir::CallExpression::Static(call) => {
                if let Some(callee_ty_name) = call.name.clone().parent()
                    && callee_ty_name.is_type()
                {
                    let Some(callee_type) = self.find_type_ref_from(&callee_ty_name, call.id)? else {
                        return Err(self.missing_type_err(&lume_hir::Type {
                            id: lume_span::NodeId::empty(call.id.package),
                            name: callee_ty_name.clone(),
                            location: callee_ty_name.name().location,
                        }));
                    };

                    let method = self.lookup_method_on(&callee_type, call.name.name());

                    let Some(method) = method else {
                        let missing_method_err = self.fold_method_suggestions(expr)?;

                        return Err(missing_method_err.into());
                    };

                    Ok(Callable::Method(method))
                } else {
                    let functions = self.probe_functions(&call.name);

                    let Some(func_idx) = self.find_local_function(&call.location, &functions) else {
                        let missing_func_err = self.fold_function_suggestions(call)?;

                        return Err(missing_func_err.into());
                    };

                    Ok(Callable::Function(functions[func_idx]))
                }
            }
        }
    }

    /// Find the index into `funcs` with the function which is "closest" to the
    /// given location.
    ///
    /// First, the function attempts to find a function which is defined in the
    /// same file as `local_loc`. If none is found, it looks for a function
    /// which exists within the same package as `local_loc`. If that also
    /// fails, it returns the index of the first function in the slice.
    #[libftrace::traced(level = Trace)]
    fn find_local_function(&self, local_loc: &Location, funcs: &[&Function]) -> Option<usize> {
        // Functions defined within the same file as the given location.
        if let Some(idx) = funcs
            .iter()
            .position(|c| self.hir_span_of_node(c.id).file.id == local_loc.file.id)
        {
            return Some(idx);
        }

        // Functions defined within the same package as the given location.
        if let Some(idx) = funcs
            .iter()
            .position(|c| self.hir_span_of_node(c.id).file.package == local_loc.file.package)
        {
            return Some(idx);
        }

        // Functions which are visible outside of their owning package
        if let Some(idx) = funcs.iter().position(|c| c.visibility == lume_hir::Visibility::Public) {
            return Some(idx);
        }

        if funcs.is_empty() { None } else { Some(0) }
    }

    /// Looks up all [`Method`]s and attempts to find a single [`Method`], which
    /// matches the signature of the given instance call expression.
    ///
    /// Methods returned by this method are checked for validity within the
    /// current context, including visibility, arguments and type arguments.
    /// The look up methods which only match the callee type and method
    /// name, see [`TyInferCtx::lookup_methods_on()`].
    ///
    /// For a generic callable lookup, see [`TyInferCtx::lookup_callable()`].
    /// For a static callable lookup, see
    /// [`TyInferCtx::lookup_callable_static()`].
    #[libftrace::traced(level = Trace, err)]
    pub fn probe_callable_instance(&self, call: &lume_hir::InstanceCall) -> Result<Callable<'_>> {
        self.probe_callable(lume_hir::CallExpression::Instanced(call))
    }

    /// Looks up all [`Method`]s and attempts to find a single [`Method`], which
    /// matches the signature of the given intrinsic call expression.
    ///
    /// Methods returned by this method are checked for validity within the
    /// current context, including visibility, arguments and type arguments.
    /// The look up methods which only match the callee type and method
    /// name, see [`TyInferCtx::lookup_methods_on()`].
    ///
    /// For a generic callable lookup, see [`TyInferCtx::lookup_callable()`].
    /// For a static callable lookup, see
    /// [`TyInferCtx::lookup_callable_static()`].
    #[libftrace::traced(level = Trace, err)]
    pub fn probe_callable_intrinsic(&self, call: &lume_hir::IntrinsicCall) -> Result<Callable<'_>> {
        self.probe_callable(lume_hir::CallExpression::Intrinsic(call))
    }

    /// Looks up all [`Callable`]s and attempts to find one, which matches the
    /// signature of the given static call expression.
    ///
    /// Callables returned by this method are checked for validity within the
    /// current context, including visibility, arguments and type arguments.
    /// To look up methods which only match the callee type and method name,
    /// see [`TyInferCtx::lookup_methods_on()`].
    ///
    /// For a generic callable lookup, see [`TyInferCtx::lookup_callable()`].
    /// For an instance callable lookup, see
    /// [`TyInferCtx::lookup_callable_instance()`].
    #[libftrace::traced(level = Trace, err)]
    pub fn probe_callable_static(&self, call: &lume_hir::StaticCall) -> Result<Callable<'_>> {
        self.probe_callable(lume_hir::CallExpression::Static(call))
    }

    /// Instantiates a call expression against the given function signature,
    /// resolving any type arguments within and returning the instantiated
    /// signature.
    #[libftrace::traced(level = Trace, err)]
    pub fn instantiate_call_expression<'a>(
        &self,
        signature: lume_types::FunctionSig<'a>,
        expr: lume_hir::CallExpression<'a>,
    ) -> Result<lume_types::FunctionSigOwned> {
        let type_arguments = self.type_args_in_call(expr)?;

        Ok(self.instantiate_function(signature, &type_arguments))
    }

    /// Attempt to instantiate the given callable purely from the arguments
    /// passed to the callable.
    #[libftrace::traced(level = Trace)]
    pub fn instantiate_signature_from_args<'a>(
        &self,
        callable: Callable<'a>,
        expr: lume_hir::CallExpression<'a>,
    ) -> Result<lume_types::FunctionSigOwned> {
        let signature = self.signature_of(callable)?;

        self.instantiate_function_from_args(signature.as_ref(), expr)
    }

    /// Attempt to instantiate the given signature purely from the arguments
    /// passed to the callable.
    #[libftrace::traced(level = Trace)]
    pub fn instantiate_function_from_args<'a>(
        &self,
        signature: lume_types::FunctionSig<'a>,
        expr: lume_hir::CallExpression<'a>,
    ) -> Result<lume_types::FunctionSigOwned> {
        let mut inst = lume_types::FunctionSigOwned {
            params: signature.params.clone(),
            ret_ty: TypeRef::unknown(),
            type_params: Vec::new(),
        };

        let callable = self.probe_callable(expr)?;
        let params = &callable.signature().params;

        let args = match (expr, callable.is_instance()) {
            (lume_hir::CallExpression::Instanced(call), true) => vec![&[call.callee][..], &call.arguments[..]].concat(),
            (lume_hir::CallExpression::Intrinsic(call), true) => call.kind.arguments(),
            (lume_hir::CallExpression::Static(call), _) => call.arguments.clone(),
            (lume_hir::CallExpression::Instanced(_) | lume_hir::CallExpression::Intrinsic(_), false) => {
                return Err(crate::errors::InstanceCallOnStaticMethod {
                    source: expr.location(),
                    method_name: callable.name().clone(),
                }
                .into());
            }
        };

        debug_assert_eq!(params.len(), args.len());

        let mut type_args = Vec::new();

        for type_param in signature.type_params {
            for (param, arg) in params.inner().iter().zip(args.iter()) {
                let param_ty = &param.ty;
                let arg_ty = self.type_of(*arg)?;

                // We skip the `self` parameter since it would likely just resolve to the
                // same type parameter as the one we're trying to resolve.
                if callable.is_instance() && param.is_self() {
                    continue;
                }

                // If the parameter type doesn't have any generic components, we might
                // as well not check it.
                if !self.is_type_generic(param_ty)? {
                    continue;
                }

                if let Some(type_arg) = self.instantiate_argument_type(*type_param, param_ty, &arg_ty)? {
                    type_args.push(type_arg);
                    break;
                }
            }
        }

        debug_assert_eq!(signature.type_params.len(), type_args.len());

        for (idx, param) in signature.params.inner().iter().enumerate() {
            let param_ty = self.instantiate_type_from(&param.ty, signature.type_params, &type_args);

            param_ty.clone_into(&mut inst.params.params[idx].ty);
        }

        self.instantiate_type_from(signature.ret_ty, signature.type_params, &type_args)
            .clone_into(&mut inst.ret_ty);

        Ok(inst)
    }

    /// Instantiates a function signature against the given type arguments,
    /// resolving the parameters and return type within the signature.
    #[libftrace::traced(level = Trace)]
    pub fn instantiate_function<'a>(
        &self,
        sig: lume_types::FunctionSig<'a>,
        type_args: &[TypeRef],
    ) -> lume_types::FunctionSigOwned {
        self.instantiate_signature_isolate(sig, sig.type_params, type_args)
    }

    /// Instantiates a function signature against the given type arguments,
    /// resolving the parameters and return type within the signature.
    #[libftrace::traced(level = Trace)]
    pub fn instantiate_signature_isolate<'a>(
        &self,
        sig: lume_types::FunctionSig<'a>,
        type_params: &[NodeId],
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
    #[libftrace::traced(level = Trace)]
    pub fn instantiate_type_from<'a>(
        &self,
        ty: &'a lume_types::TypeRef,
        type_params: &[NodeId],
        type_args: &'a [TypeRef],
    ) -> lume_types::TypeRef {
        let mut inst_ty = self.instantiate_flat_type_from(ty, type_params, type_args).to_owned();

        inst_ty.type_arguments.clear();
        inst_ty.type_arguments.reserve_exact(ty.type_arguments.len());

        for type_arg in &ty.type_arguments {
            let inst_type_arg = self.instantiate_type_from(type_arg, type_params, type_args);

            inst_ty.type_arguments.push(inst_type_arg);
        }

        inst_ty
    }

    /// Instantiates a a single flat type reference against the given type
    /// arguments.
    #[libftrace::traced(level = Trace)]
    pub fn instantiate_flat_type_from<'a>(
        &self,
        ty: &'a lume_types::TypeRef,
        type_params: &[NodeId],
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
    /// Type arguments are fetched from the expression itself, as well as type
    /// arguments defined on the callee of the exprssion, if any.
    #[libftrace::traced(level = Trace)]
    pub fn type_args_in_call(&self, expr: lume_hir::CallExpression) -> Result<Vec<TypeRef>> {
        let type_parameters_hir = self.hir_avail_type_params(expr.id());
        let type_parameters = type_parameters_hir.iter().map(AsRef::as_ref).collect::<Vec<_>>();

        match &expr {
            lume_hir::CallExpression::Static(call) => {
                let hir_type_args = &call.all_type_arguments();

                self.mk_type_refs_generic(hir_type_args, &type_parameters)
            }
            lume_hir::CallExpression::Instanced(_) | lume_hir::CallExpression::Intrinsic(_) => {
                let callee = match expr {
                    lume_hir::CallExpression::Instanced(call) => call.callee,
                    lume_hir::CallExpression::Intrinsic(call) => call.kind.callee(),
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

    /// Attempts the infer the instantiated type of `type_param_id` from the
    /// given set of parameter type and matching argument type.
    #[libftrace::traced(level = Trace)]
    fn instantiate_argument_type<'a>(
        &self,
        type_param_id: NodeId,
        param_type: &TypeRef,
        arg_type: &TypeRef,
    ) -> Result<Option<TypeRef>> {
        if let Some(type_param_ref) = self.as_type_parameter(param_type)?
            && type_param_ref.id == type_param_id
        {
            return Ok(Some(arg_type.to_owned()));
        }

        for (param_type_arg, arg_type_arg) in param_type.type_arguments.iter().zip(arg_type.type_arguments.iter()) {
            if let Some(type_param_ref) = self.as_type_parameter(param_type_arg)?
                && type_param_ref.id == type_param_id
            {
                return Ok(Some(arg_type_arg.to_owned()));
            }
        }

        Ok(None)
    }

    /// Gets the expanded signature of the given [`Callable`].
    ///
    /// The expanded signature of a [`Callable`] will include all the type
    /// parameters defined on parent definitions, such as on implementation
    /// blocks.
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn signature_of(&self, callable: Callable) -> Result<FunctionSigOwned> {
        match callable {
            Callable::Method(method) => {
                let mut signature = method.sig().to_owned();

                // Append all the type parameters which are available on the method,
                // such as the type parameters on the implementation.
                signature.type_params.extend(
                    self.hir_avail_type_params(method.id)
                        .inner
                        .into_iter()
                        .map(|param| param.id),
                );

                Ok(signature)
            }
            Callable::Function(function) => Ok(function.sig().to_owned()),
        }
    }

    /// Gets the expanded signature of the [`Callable`] with the given ID.
    ///
    /// The expanded signature of a [`Callable`] will include all the type
    /// parameters defined on parent definitions, such as on implementation
    /// blocks.
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn signature_of_call_ref(&self, id: CallReference) -> Result<FunctionSigOwned> {
        let callable = self.callable_of(id)?;

        self.signature_of(callable)
    }

    /// Gets the instantiated signature of the given [`Callable`], w.r.t the
    /// given expression.
    ///
    /// The instantiated signature is a version of a signature where all
    /// resolvable type arguments within the expression have been resolved.
    /// For example, given an expression:
    ///
    /// ```lm
    /// fn foo<T>(val: T) -> T {
    ///     return val;
    /// }
    ///
    /// let a = foo<Boolean>(false);
    /// ```
    ///
    /// The resulting variable `a` will have the type of `Boolean`, since it was
    /// resolved from the type arguments on the callable expression.
    #[libftrace::traced(level = Trace, fields(callable = callable.name()), err, ret)]
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
    /// Methods returned by this method are not checked for validity within the
    /// current context, such as visibility, arguments or type arguments. To
    /// check whether any given [`Method`] is valid for a given context, see
    /// [`ThirBuildCtx::check_method()`].
    pub fn methods_defined_on(&self, self_ty: &lume_types::TypeRef) -> impl Iterator<Item = &'_ Method> {
        self.tdb().methods_on(self_ty.instance_of)
    }

    /// Determines whether the given [`TypeRef`] is a kind of
    /// [`TypeKindRef::TypeParameter`].
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn is_type_parameter(&self, ty: &TypeRef) -> Result<bool> {
        match self.tdb().ty_expect(ty.instance_of)?.kind {
            TypeKind::TypeParameter(_) => Ok(true),
            _ => Ok(false),
        }
    }

    /// Determines whether the given [`TypeRef`] is a kind of
    /// [`TypeKindRef::TypeParameter`].
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn as_type_parameter(&self, ty: &TypeRef) -> Result<Option<&lume_types::TypeParameter>> {
        match self.tdb().ty_expect(ty.instance_of)?.kind {
            TypeKind::TypeParameter(id) => Ok(self.tdb().type_parameter(id)),
            _ => Ok(None),
        }
    }

    /// Determines whether the given [`TypeRef`] has any generic components.
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn is_type_generic(&self, ty: &TypeRef) -> Result<bool> {
        if self.is_type_parameter(ty)? {
            return Ok(true);
        }

        for type_arg in &ty.type_arguments {
            if self.is_type_parameter(type_arg)? {
                return Ok(true);
            }
        }

        Ok(false)
    }

    /// Determines whether the given function is an entrypoint.
    #[cached_query]
    #[libftrace::traced(level = Trace)]
    pub fn is_entrypoint(&self, id: NodeId) -> bool {
        let Some(node) = self.hir.node(id) else { return false };

        let lume_hir::Node::Function(func) = node else {
            return false;
        };

        func.name.root.is_empty() && func.name.name.name().as_str() == "main"
    }

    /// Determines whether the given [`TypeRef`] is the `Never` type.
    #[cached_query]
    #[libftrace::traced(level = Trace)]
    pub fn is_type_never(&self, ty: &TypeRef) -> bool {
        let never_type = self.lang_item_type("never").expect("expected `Never` lang item");

        never_type.instance_of == ty.instance_of
    }

    /// Returns the [`DefId`] of the `std::ops::Dispose::dispose()` method from
    /// the standard library.
    #[cached_query]
    #[libftrace::traced(level = Trace)]
    pub fn drop_method_def(&self) -> NodeId {
        let drop_type_ref = self.lang_item_type("dispose_trait").unwrap();
        let drop_method_name = Identifier::from("dispose");

        let method = self
            .lookup_impl_methods_on(&drop_type_ref, &drop_method_name)
            .next()
            .unwrap();

        method.id
    }

    /// Determines whether the given method is a dropper.
    #[cached_query]
    #[libftrace::traced(level = Trace, fields(method), ret)]
    pub fn is_method_dropper(&self, method: NodeId) -> bool {
        let dropper_method_id = self.drop_method_def();

        if let Some(lume_hir::Node::TraitMethodImpl(method_impl)) = self.hir_node(method)
            && let Ok(method_def) = self.hir_trait_method_def_of_impl(method_impl)
            && method_def.id == dropper_method_id
        {
            return true;
        }

        false
    }

    /// Gets the trait implementation of `trait_type` on the type `source`.
    #[libftrace::traced(level = Trace)]
    pub fn get_trait_impl_of(&self, trait_type: &TypeRef, source: &TypeRef) -> Option<&lume_hir::TraitImplementation> {
        for trait_use in self.tdb().uses_on(source) {
            if &trait_use.trait_ == trait_type {
                let lume_hir::Node::TraitImpl(trait_impl) = self.hir_node(trait_use.id)? else {
                    panic!("bug!: expected trait impl with ID {}", trait_use.id);
                };

                return Some(trait_impl);
            }
        }

        None
    }

    /// Gets the trait implementation of `Cast<T>` on the type `source`, where
    /// `T` is the type `dest`.
    #[libftrace::traced(level = Trace)]
    pub fn cast_impl_of(&self, source: &TypeRef, dest: &TypeRef) -> Option<&lume_hir::TraitImplementation> {
        let mut cast_trait = self.lang_item_type("cast_trait")?;
        cast_trait.type_arguments.push(dest.clone());

        self.get_trait_impl_of(&cast_trait, source)
    }
}
