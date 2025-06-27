#![feature(map_try_insert)]

use std::collections::BTreeMap;

use error_snippet::Result;
use lume_errors::DiagCtx;
use lume_hir::{PathSegment, SymbolName, TypeId, TypeParameter};
use lume_span::{DefId, StatementId};
use lume_types::{NamedTypeRef, TyCtx, TypeDatabaseContext, TypeRef};

mod define;
mod errors;
pub mod query;
#[cfg(test)]
mod tests;

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

/// Data structure for defining the inferred types of expressions, statements, etc., such
/// that it can be used and/or consumed from the [`TyCheckCtx`].
pub struct TyInferCtx {
    /// Defines the type context from the build context.
    tcx: TyCtx,

    /// Defines the HIR map which contains the input expressions.
    hir: lume_hir::map::Map,

    /// Defines a mapping any single node and their parent node.
    ancestry: BTreeMap<DefId, DefId>,
}

impl TyInferCtx {
    /// Creates a new type inference context from the given HIR map.
    pub fn new(tcx: TyCtx, hir: lume_hir::map::Map) -> Self {
        Self {
            tcx,
            hir,
            ancestry: BTreeMap::new(),
        }
    }

    /// Retrieves the High-Level Intermediate Representation (HIR) map from the build context.
    pub fn hir(&self) -> &lume_hir::map::Map {
        &self.hir
    }

    /// Retrieves the High-Level Intermediate Representation (HIR) map from the build context.
    pub fn hir_mut(&mut self) -> &mut lume_hir::map::Map {
        &mut self.hir
    }

    /// Retrieves the type context from the build context.
    pub fn tdb(&self) -> &TypeDatabaseContext {
        self.tcx.db()
    }

    /// Retrieves the type context from the build context.
    pub fn tdb_mut(&mut self) -> &mut TypeDatabaseContext {
        self.tcx.db_mut()
    }

    /// Retrieves the diagnostics handler from the parent context.
    pub fn dcx(&self) -> DiagCtx {
        self.tcx.dcx()
    }

    /// Defines all the different types, type parameters and type constraints within
    /// the HIR maps into the type database.
    ///
    /// The defined types are stored within the `TyInferCtx` struct, which can be
    /// accessed through the `self.tcx` field, the `self.tcx()` method or the `self.tdb_mut()` method.
    ///
    /// # Errors
    ///
    /// Returns `Err` when either a language error occured, such as missing variables, missing methods,
    /// etc, or when expected items cannot be found within the context.
    #[tracing::instrument(
        level = "INFO",
        name = "lume_infer::TyInferCtx::infer",
        parent = None,
        skip(self),
        err
    )]
    pub fn infer(&mut self) -> Result<()> {
        self.define_types();
        self.define_functions();
        self.define_implementations();
        self.define_trait_implementations();
        self.define_properties()?;
        self.define_trait_methods()?;
        self.define_type_parameters()?;
        self.define_type_constraints()?;
        self.define_property_types()?;
        self.define_method_bodies()?;
        self.define_scopes()?;

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
    pub fn mk_type_ref(&self, ty: &lume_hir::Type) -> Result<TypeRef> {
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

        let mut type_ref = TypeRef::new(found_type, ty.location);

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
    ///
    /// # Errors
    ///
    /// Returns `Err` if one-or-more typed path segments include invalid references
    /// to type IDs.
    pub fn find_type_ref(&self, name: &SymbolName) -> Result<Option<TypeRef>> {
        let Some(ty) = self.tdb().find_type(name) else {
            return Ok(None);
        };

        let location = name.location;

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
                return errors::UnavailableScalarType {
                    source: ty.location.file.clone(),
                    range: ty.location.index.clone(),
                    found: ty.name.name.to_string(),
                    suggestion: lume_name,
                }
                .into();
            }
        }

        errors::MissingType {
            source: ty.location,
            name: ty.name.clone(),
        }
        .into()
    }

    /// Creates a new [`NamedTypeRef`] from the given [`TypeRef`].
    ///
    /// # Errors
    ///
    /// Returns `Err` if any types referenced by the given [`TypeRef`], or any child
    /// instances, are missing from the type context.
    pub fn new_named_type(&self, type_ref: &TypeRef) -> Result<NamedTypeRef> {
        let name = self.type_ref_name(type_ref)?.as_str().to_string();
        let type_arguments = type_ref
            .type_arguments
            .iter()
            .map(|arg| self.new_named_type(arg))
            .collect::<Result<Vec<_>>>()?;

        Ok(NamedTypeRef { name, type_arguments })
    }
}
