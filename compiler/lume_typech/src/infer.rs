use error_snippet::Result;
use lume_hir::{self, PathSegment, TypeId};
use lume_span::StatementId;

use crate::{query::CallReference, *};

mod define_functions;
mod define_impl;
mod define_impl_methods;
mod define_method_bodies;
mod define_properties;
mod define_property_types;
mod define_type_constraints;
mod define_type_params;
mod define_types;
mod define_use;

/// Defines a list of types which are often used in other languages,
/// but have a different name in Lume.
const NEWCOMER_TYPE_NAMES: &[(&str, &str)] = &[
    ("int", "Int32"),
    ("i8", "Int8"),
    ("u8", "UInt8"),
    ("i16", "Int16"),
    ("u16", "UInt16"),
    ("i32", "Int32"),
    ("u32", "UInt32"),
    ("i64", "Int64"),
    ("u64", "UInt64"),
    ("isize", "IntPtr"),
    ("usize", "UIntPtr"),
    ("f32", "Float"),
    ("f64", "Double"),
    ("str", "String"),
    ("string", "String"),
    ("bool", "Boolean"),
    ("boolean", "Boolean"),
];

impl ThirBuildCtx {
    /// Defines all the different types, type parameters and type constraints within
    /// the HIR maps into the type database.
    ///
    /// The defined types are stored within the `ThirBuildCtx` struct, which can be
    /// accessed through the `self.tcx` field, the `self.tcx()` method or the `self.tcx_mut()` method.
    ///
    /// # Errors
    ///
    /// Returns `Err` when either a language error occured, such as missing variables, missing methods,
    /// etc, or when expected items cannot be found within the context.
    pub fn define_types(&mut self) -> Result<()> {
        infer::define_types::DefineTypes::run_all(self);
        infer::define_functions::DefineFunctions::run_all(self);
        infer::define_impl::define_impl(self);
        infer::define_use::define_trait_impl(self)?;
        infer::define_properties::DefineProperties::run_all(self)?;
        infer::define_type_params::DefineTypeParameters::run_all(self)?;
        infer::define_impl_methods::DefineImplementationMethods::run_all(self)?;
        infer::define_type_constraints::DefineTypeConstraints::run_all(self)?;
        infer::define_property_types::DefinePropertyTypes::run_all(self)?;
        infer::define_method_bodies::DefineMethodBodies::run_all(self)?;

        self.infer_calls()?;

        Ok(())
    }

    /// Attempt to infer the referenced callable of all call expressions in the current module.
    ///
    /// The resolved references are stored in the `resolved_calls` field of the `ThirBuildCtx`, which can be
    /// accessed through the `self.tcx` field, the `self.tcx()` method or the `self.tcx_mut()` method.
    fn infer_calls(&mut self) -> Result<()> {
        for (id, expr) in self.hir.expressions() {
            let reference: CallReference = match &expr.kind {
                lume_hir::ExpressionKind::InstanceCall(call) => self.lookup_callable_instance(call)?.into(),
                lume_hir::ExpressionKind::StaticCall(call) => self.lookup_callable_static(call)?.into(),
                _ => continue,
            };

            self.resolved_calls.insert(*id, reference);
        }

        // After all calls have been resolved, we'll update the amount type parameters
        // in the call expressions, so
        for (id, reference) in &mut self.resolved_calls {
            let location = self.hir.expression(*id).unwrap().location.clone();

            let type_params = match *reference {
                CallReference::Method(id) => &self.tcx.method(id).unwrap().type_parameters,
                CallReference::Function(id) => &self.tcx.function(id).unwrap().type_parameters,
            };

            let type_args = match &mut self.hir.expressions_mut().get_mut(id).unwrap().kind {
                lume_hir::ExpressionKind::InstanceCall(call) => &mut call.type_arguments,
                lume_hir::ExpressionKind::StaticCall(call) => &mut call.type_arguments,
                kind => panic!("BUG: unexpected expression kind: {kind:?}"),
            };

            // If no type arguments are provided, we are expected to infer all the of them.
            //
            // However, if at least one is provided, but it doesn't match the number of type parameters,
            // we raise an error, so the user doesn't invoke the wrong function / method.
            if !type_args.is_empty() && type_args.len() != type_params.len() {
                return Err(crate::errors::TypeArgumentMismatch {
                    source: location.file.clone(),
                    range: location.index.clone(),
                    expected: type_params.len(),
                    found: type_args.len(),
                }
                .into());
            }

            // If the type arguments are meant to be inferred, match the number of type arguments
            // with the number of type parameters, using `Implicit` type arguments.
            if type_args.is_empty() {
                for _ in 0..type_params.len() {
                    type_args.push(lume_hir::TypeArgument::Implicit {
                        location: lume_span::Location::empty(),
                    });
                }
            }
        }

        Ok(())
    }

    /// Gets the HIR statement with the given ID and assert that it's a variable declaration statement.
    pub(crate) fn hir_expect_var_stmt(&self, id: StatementId) -> &lume_hir::VariableDeclaration {
        let stmt = self.hir_stmt(id);

        match &stmt.kind {
            lume_hir::StatementKind::Variable(decl) => decl,
            t => panic!("invalid variable reference type: {t:?}"),
        }
    }

    /// Lowers the given HIR type into a type reference.
    pub(crate) fn mk_type_ref(&self, ty: &lume_hir::Type) -> Result<TypeRef> {
        self.mk_type_ref_generic(ty, &[])
    }

    /// Lowers the given HIR type into a type reference, which also looks
    /// up the given type parameters.
    pub(crate) fn mk_type_ref_generic(&self, ty: &lume_hir::Type, type_params: &[TypeParameter]) -> Result<TypeRef> {
        let Some(found_type) = self.find_type_ref_ctx(&ty.name, type_params) else {
            return Err(self.missing_type_err(ty));
        };

        let mut type_ref = TypeRef::new(found_type);

        for type_param in &ty.type_params {
            let type_param_ref = self.mk_type_ref_generic(type_param, type_params)?;
            type_ref.type_arguments.push(type_param_ref);
        }

        Ok(type_ref)
    }

    fn find_type_ref_ctx(&self, name: &SymbolName, type_params: &[TypeParameter]) -> Option<TypeId> {
        // First, attempt to find the type name within the given type parameters.
        for type_param in type_params {
            let lume_hir::PathSegment::Named(type_name) = &name.name else {
                break;
            };

            if &type_param.name == type_name {
                return Some(type_param.type_id.unwrap());
            }
        }

        // Afterwards, attempt to find the type name within the type context.
        self.tcx().find_type(name).map(|ty| ty.id)
    }

    /// Attempts to find a [`TypeRef`] with the given name, if any.
    pub(crate) fn find_type_ref(&self, name: &SymbolName) -> Result<Option<TypeRef>> {
        let Some(ty) = self.tcx().find_type(name) else {
            return Ok(None);
        };

        if let PathSegment::Typed(_, args) = &name.name {
            let args = args
                .iter()
                .map(|arg| self.mk_type_ref(arg))
                .collect::<Result<Vec<_>>>()?;

            Ok(Some(TypeRef {
                instance_of: ty.id,
                type_arguments: args,
            }))
        } else {
            Ok(Some(TypeRef::new(ty.id)))
        }
    }

    /// Returns an error indicating that the given type was not found.
    #[allow(clippy::unused_self)]
    fn missing_type_err(&self, ty: &lume_hir::Type) -> error_snippet::Error {
        for (newcomer_name, lume_name) in NEWCOMER_TYPE_NAMES {
            let lume_hir::PathSegment::Named(ty_name) = &ty.name.name else {
                continue;
            };

            if newcomer_name == &ty_name.name {
                return check::errors::UnavailableScalarType {
                    source: ty.location.file.clone(),
                    range: ty.location.index.clone(),
                    found: ty.name.name.to_string(),
                    suggestion: lume_name,
                }
                .into();
            }
        }

        errors::MissingType {
            source: ty.location.file.clone(),
            range: ty.location.index.clone(),
            name: ty.name.clone(),
        }
        .into()
    }
}
