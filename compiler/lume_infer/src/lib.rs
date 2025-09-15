use std::{collections::BTreeMap, fmt::Debug, ops::Deref};

use error_snippet::Result;
use lume_errors::DiagCtx;
use lume_hir::{Path, TypeId, TypeParameter};
use lume_span::{DefId, StatementId};
use lume_types::{FunctionSig, NamedTypeRef, TyCtx, TypeDatabaseContext, TypeRef};

mod define;
mod errors;
pub mod query;
mod unification;

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
        self.define_fields()?;
        self.define_trait_methods()?;
        self.define_type_parameters()?;
        self.define_type_constraints()?;
        self.define_field_types()?;
        self.define_method_bodies()?;
        self.define_scopes()?;
        self.infer_type_arguments()?;

        tracing::debug!(target: "inference", "finished inference");

        tracing::debug_span!(target: "inference", "type unification").in_scope(|| {
            let pass = unification::UnificationPass::default();

            pass.invoke(self)?;

            // We need to invalidate the global cache for method calls, since the unification
            // pass has altered some items in the HIR, making those entries in the cache
            // incorrect and/or invalid.
            //
            // Very few method calls would've been cached at this point in the compile process,
            // so we can safetly clear the entire thing, without having to worry too much about
            // the potential performance loss.
            tracing::debug_span!("unification cache invalidation").in_scope(|| {
                let ctx: &lume_session::GlobalCtx = &*self;
                let store = lume_query::CacheContext::store(ctx);

                store.clear();
            });

            Result::Ok(())
        })?;

        self.dcx().ensure_untainted()?;

        Ok(())
    }

    /// Gets the HIR statement with the given ID and assert that it's a variable declaration statement.
    #[tracing::instrument(level = "DEBUG", skip(self))]
    #[allow(dead_code, reason = "expected used in future")]
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
    #[tracing::instrument(
        level = "DEBUG",
        skip_all, fields(ty = %ty.name, loc = %ty.location, ty_params = ?type_params),
        err
    )]
    pub fn mk_type_ref_generic(&self, ty: &lume_hir::Type, type_params: &[&TypeParameter]) -> Result<TypeRef> {
        let Some(found_type) = self.find_type_ref_ctx(&ty.name, type_params) else {
            return Err(self.missing_type_err(ty));
        };

        let mut type_ref = TypeRef::new(found_type, ty.location);

        for type_param in ty.type_arguments() {
            let type_param_ref = self.mk_type_ref_generic(type_param, type_params)?;
            type_ref.type_arguments.push(type_param_ref);
        }

        Ok(type_ref)
    }

    /// Lowers the given HIR types into type references.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub fn mk_type_refs(&self, ty: &[lume_hir::Type]) -> Result<Vec<TypeRef>> {
        ty.iter().map(|t| self.mk_type_ref(t)).collect()
    }

    /// Lowers the given HIR types into type references.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub fn mk_type_refs_generic(&self, ty: &[lume_hir::Type], type_params: &[&TypeParameter]) -> Result<Vec<TypeRef>> {
        ty.iter().map(|t| self.mk_type_ref_generic(t, type_params)).collect()
    }

    /// Lowers the given HIR type, with respect to the type parameters available from
    /// the given definition.
    #[tracing::instrument(
        level = "DEBUG",
        skip_all, fields(ty = %ty.name, loc = %ty.location, def = ?def),
        err
    )]
    pub fn mk_type_ref_from(&self, ty: &lume_hir::Type, def: lume_span::DefId) -> Result<TypeRef> {
        let type_parameters_hir = self.hir_avail_type_params(def);
        let type_parameters = type_parameters_hir.iter().map(AsRef::as_ref).collect::<Vec<_>>();

        self.mk_type_ref_generic(ty, &type_parameters)
    }

    /// Lowers the given HIR type, with respect to the type parameters available from
    /// the given expression.
    #[tracing::instrument(
        level = "DEBUG",
        skip_all, fields(ty = %ty.name, loc = %ty.location, expr = ?expr),
        err
    )]
    pub fn mk_type_ref_from_expr(&self, ty: &lume_hir::Type, expr: lume_span::ExpressionId) -> Result<TypeRef> {
        self.mk_type_ref_from(ty, DefId::Expression(expr))
    }

    /// Lowers the given HIR types, with respect to the type parameters available from
    /// the given definition.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub fn mk_type_refs_from(&self, ty: &[lume_hir::Type], def: lume_span::DefId) -> Result<Vec<TypeRef>> {
        let type_parameters_hir = self.hir_avail_type_params(def);
        let type_parameters = type_parameters_hir.iter().map(AsRef::as_ref).collect::<Vec<_>>();

        self.mk_type_refs_generic(ty, &type_parameters)
    }

    #[tracing::instrument(level = "DEBUG", skip_all, fields(name = %name), ret)]
    fn find_type_ref_ctx<T: AsRef<TypeParameter>>(&self, name: &Path, type_params: &[T]) -> Option<TypeId> {
        // First, attempt to find the type name within the given type parameters.
        for type_param in type_params {
            if &type_param.as_ref().name == name.name.name() {
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
    #[tracing::instrument(level = "TRACE", skip_all, fields(name = %name), err)]
    pub fn find_type_ref(&self, name: &Path) -> Result<Option<TypeRef>> {
        let Some(ty) = self.tdb().find_type(name) else {
            return Ok(None);
        };

        let location = name.location;

        let args = name
            .type_arguments()
            .iter()
            .map(|arg| self.mk_type_ref(arg))
            .collect::<Result<Vec<_>>>()?;

        Ok(Some(TypeRef {
            instance_of: ty.id,
            type_arguments: args,
            location,
        }))
    }

    /// Attempts to find a [`TypeRef`] with the given name, if any.
    ///
    /// # Errors
    ///
    /// Returns `Err` if one-or-more typed path segments include invalid references
    /// to type IDs.
    #[tracing::instrument(level = "TRACE", skip_all, fields(name = %name), err)]
    pub fn find_type_ref_generic(&self, name: &Path, ty_params: &[&TypeParameter]) -> Result<Option<TypeRef>> {
        let found_ty = 'find: {
            if name.root.is_empty() {
                let param_type_id = ty_params
                    .iter()
                    .find_map(|ty| if &ty.name == name.name() { ty.type_id } else { None });

                if let Some(param_type_id) = param_type_id {
                    break 'find self.tdb().type_(param_type_id);
                }
            }

            self.tdb().find_type(name)
        };

        let Some(ty) = found_ty else {
            return Ok(None);
        };

        let location = name.location;

        let args = name
            .type_arguments()
            .iter()
            .map(|arg| self.mk_type_ref_generic(arg, ty_params))
            .collect::<Result<Vec<_>>>()?;

        Ok(Some(TypeRef {
            instance_of: ty.id,
            type_arguments: args,
            location,
        }))
    }

    /// Attempts to find a [`TypeRef`] with the given name, if any.
    ///
    /// # Errors
    ///
    /// Returns `Err` if one-or-more typed path segments include invalid references
    /// to type IDs.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub fn find_type_ref_from(&self, name: &Path, def: lume_span::DefId) -> Result<Option<TypeRef>> {
        let type_parameters_hir = self.hir_avail_type_params(def);
        let type_parameters = type_parameters_hir.iter().map(AsRef::as_ref).collect::<Vec<_>>();

        self.find_type_ref_generic(name, &type_parameters)
    }

    /// Returns an error indicating that the given type was not found.
    fn missing_type_err(&self, ty: &lume_hir::Type) -> error_snippet::Error {
        for (newcomer_name, lume_name) in NEWCOMER_TYPE_NAMES {
            if newcomer_name == &ty.name.name().as_str() {
                return errors::UnavailableScalarType {
                    source: ty.location.file.clone(),
                    range: ty.location.index.clone(),
                    found: ty.name.name.to_string(),
                    suggestion: lume_name,
                }
                .into();
            }
        }

        let mut type_path = ty.name.root.clone();

        while !type_path.is_empty() {
            if !self.tdb().namespace_exists(&type_path) {
                let subpath = Path::from(type_path);

                let source = if let Some(import) = self.hir.get_imported(&subpath) {
                    import.location
                } else {
                    subpath.location
                };

                if let Some(parent) = subpath.clone().parent() {
                    return errors::InvalidNamespace {
                        source,
                        parent: format!("{parent:+}"),
                        name: subpath.name.to_string(),
                    }
                    .into();
                } else {
                    return errors::InvalidNamespaceRoot {
                        source,
                        name: format!("{:+}", subpath.name.to_string()),
                    }
                    .into();
                }
            }

            type_path.pop();
        }

        if let Some(import) = self.hir.get_imported(&ty.name) {
            return errors::InvalidTypeInNamespace {
                source: import.name.name().location,
                name: ty.name.clone(),
                namespace: format!("{:+}", ty.name.clone().parent().unwrap()),
            }
            .into();
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
    pub fn new_named_type(&self, type_ref: &TypeRef, expand: bool) -> Result<NamedTypeRef> {
        let path = self.type_ref_name(type_ref)?;
        let name = if expand { format!("{path:+}") } else { format!("{path}") };

        let type_arguments = type_ref
            .type_arguments
            .iter()
            .map(|arg| self.new_named_type(arg, expand))
            .collect::<Result<Vec<_>>>()?;

        Ok(NamedTypeRef { name, type_arguments })
    }

    /// Creates a human-readable version of the given signature.
    ///
    /// # Errors
    ///
    /// Returns `Err` if any types referenced by the given [`FunctionSig`], or any child
    /// instances are missing from the type context.
    pub fn sig_to_string(&self, name: &lume_hir::Identifier, sig: FunctionSig<'_>, expand: bool) -> Result<String> {
        let name = if expand { format!("{name:+}") } else { format!("{name}") };

        let type_parameters = if sig.type_params.is_empty() {
            String::new()
        } else {
            format!(
                "<{}>",
                sig.type_params
                    .iter()
                    .map(|id| {
                        let param = self.tdb().type_parameter(*id).unwrap();

                        self.type_param_to_string(param, expand)
                    })
                    .collect::<Result<Vec<_>>>()?
                    .join(", ")
            )
        };

        let parameters = format!(
            "({})",
            sig.params
                .inner()
                .iter()
                .map(|param| {
                    if param.name == "self" {
                        Ok(String::from("self"))
                    } else {
                        let type_name = self.new_named_type(&param.ty, expand)?;

                        Ok(format!(
                            "{}{}: {type_name}",
                            if param.vararg { "..." } else { "" },
                            param.name
                        ))
                    }
                })
                .collect::<Result<Vec<_>>>()?
                .join(", ")
        );

        let ret_ty = self.new_named_type(sig.ret_ty, expand)?;

        Ok(format!("fn {name}{type_parameters}{parameters} -> {ret_ty}"))
    }

    /// Creates a human-readable version of the given type parameter.
    ///
    /// # Errors
    ///
    /// Returns `Err` if any types referenced by the given [`TypeParameter`], or any child
    /// instances are missing from the type context.
    pub fn type_param_to_string(&self, type_param: &lume_types::TypeParameter, expand: bool) -> Result<String> {
        let constraints = if type_param.constraints.is_empty() {
            String::new()
        } else {
            format!(
                ": {}",
                type_param
                    .constraints
                    .iter()
                    .map(|constraint| { Ok(self.new_named_type(constraint, expand)?.to_string()) })
                    .collect::<Result<Vec<_>>>()?
                    .join(", ")
            )
        };

        Ok(format!("{}{constraints}", type_param.name))
    }

    /// Lifts the given [`TypeRef`] into a HIR [`lume_hir::Type`] instance.
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub fn hir_lift_type(&self, ty: &TypeRef) -> Result<lume_hir::Type> {
        let id = lume_span::ItemId::from_name(ty.instance_of.package, ty);
        let name = self.type_ref_name(ty)?.to_owned();
        let location = ty.location;

        Ok(lume_hir::Type { id, name, location })
    }

    /// Creates a new [`TypeRef`] which refers to the `std::Type` type.
    ///
    /// # Panics
    ///
    /// Panics if the type is not found within the database.
    pub fn std_type(&self) -> TypeRef {
        let name = lume_hir::Path::from_parts(
            Some([lume_hir::PathSegment::namespace("std")]),
            lume_hir::PathSegment::ty("Type"),
        );

        self.find_type_ref(&name).unwrap().unwrap()
    }

    /// Creates a new [`TypeRef`] which refers to the type of the given name.
    ///
    /// # Panics
    ///
    /// Panics if the type is not found within the database.
    pub fn std_type_ref(&self, name: &str) -> TypeRef {
        let name = lume_hir::Path::from_parts(
            Some([lume_hir::PathSegment::namespace("std")]),
            lume_hir::PathSegment::ty(name),
        );

        self.find_type_ref(&name).unwrap().unwrap()
    }

    /// Creates a new [`TypeRef`] which refers to the `std::String`.
    ///
    /// # Panics
    ///
    /// Panics if the type is not found within the database.
    pub fn std_ref_string(&self) -> TypeRef {
        self.std_type_ref("String")
    }

    /// Creates a new [`TypeRef`] which refers to the `std::Array`.
    ///
    /// # Panics
    ///
    /// Panics if the type is not found within the database.
    pub fn std_ref_array(&self, elemental: TypeRef) -> TypeRef {
        let mut ty = self.std_type_ref("Array");
        ty.type_arguments.push(elemental);

        ty
    }

    /// Creates a new [`TypeRef`] which refers to the `std::Pointer`.
    ///
    /// # Panics
    ///
    /// Panics if the type is not found within the database.
    pub fn std_ref_pointer(&self, elemental: TypeRef) -> TypeRef {
        let mut ty = self.std_type_ref("Pointer");
        ty.type_arguments.push(elemental);

        ty
    }

    /// Determines whether the given type is of type `std::Array`.
    pub fn is_std_array(&self, ty: &TypeRef) -> bool {
        ty.instance_of == self.std_type_ref("Array").instance_of
    }
}

impl Deref for TyInferCtx {
    type Target = TyCtx;

    fn deref(&self) -> &Self::Target {
        &self.tcx
    }
}
