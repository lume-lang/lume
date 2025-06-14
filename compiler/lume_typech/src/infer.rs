use crate::*;
use error_snippet::Result;
use lume_hir::{self, PathSegment, TypeId};
use lume_span::StatementId;

mod define_func;
mod define_impl;
mod define_method_bodies;
mod define_properties;
mod define_property_types;
mod define_scopes;
mod define_trait;
mod define_trait_func;
mod define_type_constraints;
mod define_type_params;
mod define_types;

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
    /// accessed through the `self.tcx` field, the `self.tcx()` method or the `self.tdb_mut()` method.
    ///
    /// # Errors
    ///
    /// Returns `Err` when either a language error occured, such as missing variables, missing methods,
    /// etc, or when expected items cannot be found within the context.
    #[tracing::instrument(
        level = "INFO",
        name = "lume_typech::ThirBuildCtx::define_types",
        parent = None,
        skip(self),
        err
    )]
    pub fn define_types(&mut self) -> Result<()> {
        infer::define_types::DefineTypes::run_all(self);
        infer::define_func::DefineFunctions::run_all(self);
        infer::define_impl::define_impl(self);
        infer::define_trait::define_trait_impl(self)?;
        infer::define_properties::DefineProperties::run_all(self)?;
        infer::define_trait_func::DefineTraitMethods::run_all(self)?;
        infer::define_type_params::DefineTypeParameters::run_all(self)?;
        infer::define_type_constraints::DefineTypeConstraints::run_all(self)?;
        infer::define_property_types::DefinePropertyTypes::run_all(self)?;
        infer::define_method_bodies::DefineMethodBodies::run_all(self)?;
        infer::define_scopes::DefineScopes::run_all(self)?;

        Ok(())
    }

    /// Gets the HIR statement with the given ID and assert that it's a variable declaration statement.
    #[tracing::instrument(level = "DEBUG", skip(self))]
    pub(crate) fn hir_expect_var_stmt(&self, id: StatementId) -> &lume_hir::VariableDeclaration {
        let stmt = self.hir_expect_stmt(id);

        match &stmt.kind {
            lume_hir::StatementKind::Variable(decl) => decl,
            t => panic!("invalid variable reference type: {t:?}"),
        }
    }

    /// Lowers the given HIR type into a type reference.
    #[tracing::instrument(level = "DEBUG", skip_all, fields(ty = %ty.name, loc = %ty.location), err)]
    pub(crate) fn mk_type_ref(&self, ty: &lume_hir::Type) -> Result<TypeRef> {
        let params: &[&TypeParameter] = &[];

        self.mk_type_ref_generic(ty, params)
    }

    /// Lowers the given HIR type into a type reference, which also looks
    /// up the given type parameters.
    #[tracing::instrument(level = "DEBUG", skip_all, fields(ty = %ty.name, loc = %ty.location), err)]
    pub(crate) fn mk_type_ref_generic<T: AsRef<TypeParameter>>(
        &self,
        ty: &lume_hir::Type,
        type_params: &[T],
    ) -> Result<TypeRef> {
        let Some(found_type) = self.find_type_ref_ctx(&ty.name, type_params) else {
            return Err(self.missing_type_err(ty));
        };

        let mut type_ref = TypeRef::new(found_type, ty.location.clone());

        for type_param in &ty.type_params {
            let type_param_ref = self.mk_type_ref_generic(type_param, type_params)?;
            type_ref.type_arguments.push(type_param_ref);
        }

        Ok(type_ref)
    }

    #[tracing::instrument(level = "DEBUG", skip_all, fields(name = %name), ret)]
    fn find_type_ref_ctx<T: AsRef<TypeParameter>>(&self, name: &SymbolName, type_params: &[T]) -> Option<TypeId> {
        // First, attempt to find the type name within the given type parameters.
        for type_param in type_params {
            let lume_hir::PathSegment::Named(type_name) = &name.name else {
                break;
            };

            if &type_param.as_ref().name == type_name {
                return Some(type_param.as_ref().type_id.unwrap());
            }
        }

        // Afterwards, attempt to find the type name within the type context.
        self.tdb().find_type(name).map(|ty| ty.id)
    }

    /// Attempts to find a [`TypeRef`] with the given name, if any.
    pub(crate) fn find_type_ref(&self, name: &SymbolName) -> Result<Option<TypeRef>> {
        let Some(ty) = self.tdb().find_type(name) else {
            return Ok(None);
        };

        let location = name.location.clone();

        if let PathSegment::Typed(_, args) = &name.name {
            let args = args
                .iter()
                .map(|arg| self.mk_type_ref(arg))
                .collect::<Result<Vec<_>>>()?;

            Ok(Some(TypeRef {
                instance_of: ty.id,
                type_arguments: args,
                location,
            }))
        } else {
            Ok(Some(TypeRef::new(ty.id, location)))
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
