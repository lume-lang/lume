use std::collections::BTreeMap;
use std::fmt::Debug;
use std::ops::Deref;
use std::sync::RwLock;

use error_snippet::Result;
use lume_architect::DatabaseContext;
use lume_errors::{DiagCtx, Error};
use lume_hir::{Path, TypeParameter};
use lume_span::*;
use lume_types::{FunctionSig, NamedTypeRef, TyCtx, TypeDatabaseContext, TypeRef};

mod define;
pub mod errors;
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

/// Data structure for defining the inferred types of expressions, statements,
/// etc., such that it can be used and/or consumed in the `lume_typech` crate.
pub struct TyInferCtx {
    /// Defines the type context from the build context.
    tcx: TyCtx,

    /// Defines the HIR map which contains the input expressions.
    hir: lume_hir::map::Map,

    /// Defines a mapping any single node and their parent node.
    ancestry: BTreeMap<NodeId, NodeId>,

    nested_inference_lock: RwLock<()>,
}

impl TyInferCtx {
    /// Creates a new type inference context from the given HIR map.
    pub fn new(tcx: TyCtx, hir: lume_hir::map::Map) -> Self {
        Self {
            tcx,
            hir,
            ancestry: BTreeMap::new(),
            nested_inference_lock: RwLock::new(()),
        }
    }

    /// Retrieves the High-Level Intermediate Representation (HIR) map from the
    /// build context.
    pub fn hir(&self) -> &lume_hir::map::Map {
        &self.hir
    }

    /// Retrieves the High-Level Intermediate Representation (HIR) map from the
    /// build context.
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

    /// Defines all the different types, type parameters and type constraints
    /// within the HIR maps into the type database.
    ///
    /// The defined types are stored within the `TyInferCtx` struct, which can
    /// be accessed through the `self.tcx` field, the `self.tcx()` method or
    /// the `self.tdb_mut()` method.
    ///
    /// # Errors
    ///
    /// Returns `Err` when either a language error occured, such as missing
    /// variables, missing methods, etc, or when expected items cannot be
    /// found within the context.
    #[libftrace::traced(level = Info, err)]
    pub fn infer(&mut self) -> Result<()> {
        self.define_items()?;
        self.define_methods()?;
        self.define_type_parameters()?;
        self.define_scopes()?;
        self.infer_type_arguments()?;

        libftrace::debug!("finished inference");
        self.dcx().ensure_untainted()?;

        Ok(())
    }

    /// Lowers the given HIR type into a type reference.
    #[libftrace::traced(level = Debug, fields(ty = ty.name, loc = ty.location))]
    pub fn mk_type_ref(&self, ty: &lume_hir::Type) -> Result<TypeRef> {
        let params: &[&TypeParameter] = &[];

        self.mk_type_ref_generic(ty, params)
    }

    /// Lowers the given HIR type into a type reference, which also looks
    /// up the given type parameters.
    #[libftrace::traced(level = Debug, fields(ty = ty.name, loc = ty.location, ty_params = type_params))]
    pub fn mk_type_ref_generic(&self, ty: &lume_hir::Type, type_params: &[&TypeParameter]) -> Result<TypeRef> {
        let Some(found_type) = self.find_type_ref_ctx(&ty.name, type_params) else {
            return Err(self.missing_type_err(&ty.name, ty.location));
        };

        let mut type_ref = TypeRef::new(found_type, ty.location);
        type_ref.hir = ty.self_type.then_some(ty.id);

        for type_param in ty.bound_types() {
            let type_param_ref = self.mk_type_ref_generic(type_param, type_params)?;
            type_ref.bound_types.push(type_param_ref);
        }

        Ok(type_ref)
    }

    /// Lowers the given HIR types into type references.
    #[libftrace::traced(level = Debug, err)]
    pub fn mk_type_refs(&self, ty: &[lume_hir::Type]) -> Result<Vec<TypeRef>> {
        ty.iter().map(|t| self.mk_type_ref(t)).collect()
    }

    /// Lowers the given HIR types into type references.
    #[libftrace::traced(level = Debug, err)]
    pub fn mk_type_refs_generic(&self, ty: &[lume_hir::Type], type_params: &[&TypeParameter]) -> Result<Vec<TypeRef>> {
        ty.iter().map(|t| self.mk_type_ref_generic(t, type_params)).collect()
    }

    /// Lowers the given HIR type, with respect to the type parameters available
    /// from the given definition.
    #[libftrace::traced(level = Debug, fields(ty = ty.name, loc = ty.location, def = def), err)]
    pub fn mk_type_ref_from(&self, ty: &lume_hir::Type, def: NodeId) -> Result<TypeRef> {
        let type_parameters_id = self.available_type_params_at(def);
        let type_parameters = self.as_type_params(&type_parameters_id)?;

        self.mk_type_ref_generic(ty, &type_parameters)
    }

    /// Lowers the given HIR type, with respect to the type parameters available
    /// from the given expression.
    #[libftrace::traced(level = Debug, fields(ty = ty.name, expr), err)]
    pub fn mk_type_ref_from_expr(&self, ty: &lume_hir::Type, expr: NodeId) -> Result<TypeRef> {
        self.mk_type_ref_from(ty, expr)
    }

    /// Lowers the given HIR types, with respect to the type parameters
    /// available from the given definition.
    #[libftrace::traced(level = Debug, err)]
    pub fn mk_type_refs_from(&self, ty: &[lume_hir::Type], def: NodeId) -> Result<Vec<TypeRef>> {
        let type_parameters_id = self.available_type_params_at(def);
        let type_parameters = self.as_type_params(&type_parameters_id)?;

        self.mk_type_refs_generic(ty, &type_parameters)
    }

    #[libftrace::traced(level = Debug, fields(name))]
    fn find_type_ref_ctx<T: AsRef<TypeParameter>>(&self, name: &Path, type_params: &[T]) -> Option<NodeId> {
        // First, attempt to find the type name within the given type parameters.
        for type_param in type_params {
            if &type_param.as_ref().name == name.name.name() {
                return Some(type_param.as_ref().id);
            }
        }

        // Afterwards, attempt to find the type name within the type context.
        self.tdb().find_type(name).map(|ty| ty.id)
    }

    /// Attempts to find a [`TypeRef`] with the given name, if any.
    ///
    /// # Errors
    ///
    /// Returns `Err` if one-or-more typed path segments include invalid
    /// references to type IDs.
    #[libftrace::traced(level = Trace, fields(name))]
    pub fn find_type_ref(&self, name: &Path) -> Result<Option<TypeRef>> {
        let Some(ty) = self.tdb().find_type(name) else {
            return Ok(None);
        };

        let location = name.location;

        let bound_types = name
            .bound_types()
            .iter()
            .map(|arg| self.mk_type_ref(arg))
            .collect::<Result<Vec<_>>>()?;

        Ok(Some(TypeRef {
            instance_of: ty.id,
            bound_types,
            hir: None,
            location,
        }))
    }

    /// Attempts to find a [`TypeRef`] with the given name, if any.
    ///
    /// # Errors
    ///
    /// Returns `Err` if one-or-more typed path segments include invalid
    /// references to type IDs.
    #[libftrace::traced(level = Trace, fields(name), err)]
    pub fn find_type_ref_generic(&self, name: &Path, ty_params: &[&TypeParameter]) -> Result<Option<TypeRef>> {
        let found_ty = 'find: {
            if name.root.is_empty() {
                let param_type_id = ty_params
                    .iter()
                    .find_map(|ty| if &ty.name == name.name() { Some(ty.id) } else { None });

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

        let bound_types = name
            .bound_types()
            .iter()
            .map(|arg| self.mk_type_ref_generic(arg, ty_params))
            .collect::<Result<Vec<_>>>()?;

        Ok(Some(TypeRef {
            instance_of: ty.id,
            bound_types,
            hir: None,
            location,
        }))
    }

    /// Attempts to find a [`TypeRef`] with the given name, if any.
    ///
    /// # Errors
    ///
    /// Returns `Err` if one-or-more typed path segments include invalid
    /// references to type IDs.
    #[libftrace::traced(level = Debug, err)]
    pub fn find_type_ref_from(&self, name: &Path, def: NodeId) -> Result<Option<TypeRef>> {
        let type_parameters_id = self.available_type_params_at(def);
        let type_parameters = self.as_type_params(&type_parameters_id)?;

        self.find_type_ref_generic(name, &type_parameters)
    }

    /// Ensures that the given type references are compatible.
    ///
    /// # Errors
    ///
    /// Returns `Err` when the given types are incompatible or
    /// if expected items cannot be found within the context.
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn ensure_type_compatibility(&self, from: &TypeRef, to: &TypeRef) -> Result<()> {
        // If the two given types are exactly the same, both underlying instance and
        // type arguments, we can be sure they're compatible.
        if from == to {
            return Ok(());
        }

        // The `Never` type is inherently compatible with everything.
        if self.is_type_never(from) || self.is_type_never(to) {
            return Ok(());
        }

        // Special case for `void` types, since they are always identical, no matter
        // whether they have different underlying IDs.
        match (from.is_void(), to.is_void()) {
            // void => value OR value => void
            (false, true) | (true, false) => {
                return Err(errors::MismatchedTypes {
                    reason_loc: to.location,
                    found_loc: from.location,
                    expected: self.new_named_type(to, true)?,
                    found: self.new_named_type(from, true)?,
                }
                .into());
            }

            // void == void
            (true, true) => return Ok(()),

            // value => value
            (false, false) => (),
        }

        // If `to` refers to a trait where `from` implements `to`, they can
        // be downcast correctly.
        if self.is_trait(to)? {
            if self.trait_impl_by(to, from)? {
                return Ok(());
            }

            libftrace::debug!("trait not implemented: {:?} => {:?}", from, to);

            return Err(errors::TraitNotImplemented {
                location: from.location,
                trait_name: self.new_named_type(to, false)?,
                type_name: self.new_named_type(from, false)?,
            }
            .into());
        }

        // If `to` refers to a type parameter, check if `from` satisfies the
        // constraints.
        if let Some(to_arg) = self.as_type_parameter(to)? {
            libftrace::debug!("checking type parameter constraints: {from:?} => {to:?}");

            for constraint in &to_arg.constraints {
                let constraint_type = self.mk_type_ref_from(constraint, to.instance_of)?;

                if !self.check_type_compatibility(from, &constraint_type)? {
                    return Err(errors::TypeParameterConstraintUnsatisfied {
                        source: from.location,
                        constraint_loc: constraint.location,
                        param_name: to_arg.name.to_string(),
                        type_name: self.new_named_type(from, false)?,
                        constraint_name: self.new_named_type(&constraint_type, false)?,
                    }
                    .into());
                }
            }

            libftrace::debug!("type parameter constraints valid");
            return Ok(());
        }

        // If the two types share the same elemental type, the type arguments
        // may be compatible.
        if from.instance_of == to.instance_of && from.bound_types.len() == to.bound_types.len() {
            libftrace::debug!("checking type argument downcast: {from:?} => {to:?}");

            for (from_arg, to_arg) in from.bound_types.iter().zip(to.bound_types.iter()) {
                self.ensure_type_compatibility(from_arg, to_arg)?;
            }

            libftrace::debug!("type downcast to type parameter");
            return Ok(());
        }

        #[cfg(debug_assertions)]
        {
            libftrace::debug!(
                "type-checking failed, {} => {}",
                self.new_named_type(from, false).unwrap(),
                self.new_named_type(to, false).unwrap()
            );
        }

        Err(errors::MismatchedTypes {
            reason_loc: to.location,
            found_loc: from.location,
            expected: self.new_named_type(to, false)?,
            found: self.new_named_type(from, false)?,
        }
        .into())
    }

    /// Checks whether the given type references are compatible.
    ///
    /// # Errors
    ///
    /// Returns `Err` when expected items cannot be found within the context.
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn check_type_compatibility(&self, from: &TypeRef, to: &TypeRef) -> Result<bool> {
        if let Err(err) = self.ensure_type_compatibility(from, to) {
            // Type errors will have a code attached - compiler errors will not.
            if err.code().is_none() {
                return Err(err);
            }

            Ok(false)
        } else {
            Ok(true)
        }
    }

    /// Returns an error indicating that the given type was not found.
    fn missing_type_err(&self, name: &lume_hir::Path, location: Location) -> error_snippet::Error {
        for (newcomer_name, lume_name) in NEWCOMER_TYPE_NAMES {
            if newcomer_name == &name.name().as_str() {
                return errors::UnavailableScalarType {
                    source: location.file.clone(),
                    range: location.index.clone(),
                    found: name.name.to_string(),
                    suggestion: lume_name,
                }
                .into();
            }
        }

        if let Some(import) = self.hir.get_imported(name) {
            return errors::InvalidTypeInNamespace {
                source: import.name.name().location,
                name: name.clone(),
                namespace: format!("{:+}", name.clone().parent().unwrap()),
            }
            .into();
        }

        errors::MissingType {
            source: location,
            name: name.clone(),
        }
        .into()
    }

    /// Returns an error indicating that the given types do not match.
    pub fn mismatched_types(&self, expected: &TypeRef, found: &TypeRef) -> Error {
        errors::MismatchedTypes {
            reason_loc: expected.location,
            found_loc: found.location,
            expected: self
                .new_named_type(expected, true)
                .expect("could not expand type reference"),
            found: self
                .new_named_type(found, true)
                .expect("could not expand type reference"),
        }
        .into()
    }

    /// Raises an error indicating that the given types do not match.
    pub fn raise_mismatched_types(&self, expected: &TypeRef, found: &TypeRef) {
        self.dcx().emit(self.mismatched_types(expected, found));
    }

    /// Creates a new [`NamedTypeRef`] from the given [`TypeRef`].
    ///
    /// # Errors
    ///
    /// Returns `Err` if any types referenced by the given [`TypeRef`], or any
    /// child instances, are missing from the type context.
    pub fn new_named_type(&self, type_ref: &TypeRef, expand: bool) -> Result<NamedTypeRef> {
        let path = self.type_ref_name(type_ref)?;
        let name = if expand { format!("{path:+}") } else { format!("{path}") };

        let bound_types = type_ref
            .bound_types
            .iter()
            .map(|arg| self.new_named_type(arg, expand))
            .collect::<Result<Vec<_>>>()?;

        Ok(NamedTypeRef { name, bound_types })
    }

    /// Creates a human-readable version of the given signature.
    ///
    /// # Errors
    ///
    /// Returns `Err` if any types referenced by the given [`FunctionSig`], or
    /// any child instances are missing from the type context.
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
                        let param = self.hir_expect_type_parameter(*id);

                        self.type_param_to_string(param, expand)
                    })
                    .collect::<Result<Vec<_>>>()?
                    .join(", ")
            )
        };

        let parameters = format!(
            "({})",
            sig.params
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
    /// Returns `Err` if any types referenced by the given
    /// [`lume_hir::TypeParameter`], or any child instances are missing from
    /// the type context.
    pub fn type_param_to_string(&self, type_param: &lume_hir::TypeParameter, expand: bool) -> Result<String> {
        let constraints = if type_param.constraints.is_empty() {
            String::new()
        } else {
            format!(
                ": {}",
                type_param
                    .constraints
                    .iter()
                    .map(|constraint| {
                        let constraint_type = self.mk_type_ref_from(constraint, type_param.id)?;

                        Ok(self.new_named_type(&constraint_type, expand)?.to_string())
                    })
                    .collect::<Result<Vec<_>>>()?
                    .join(", ")
            )
        };

        Ok(format!("{}{constraints}", type_param.name))
    }

    /// Lifts the given [`TypeRef`] into a HIR [`lume_hir::Type`] instance.
    #[libftrace::traced(level = Trace, err)]
    pub fn hir_lift_type(&self, ty: &TypeRef) -> Result<lume_hir::Type> {
        let name = self.type_ref_name(ty)?.to_owned();
        let location = ty.location;

        Ok(lume_hir::Type {
            id: lume_hir::TypeId::from(ty.instance_of),
            name,
            self_type: false,
            location,
        })
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
        ty.bound_types.push(elemental);

        ty
    }

    /// Creates a new [`TypeRef`] which refers to the `std::Pointer`.
    ///
    /// # Panics
    ///
    /// Panics if the type is not found within the database.
    pub fn std_ref_pointer(&self, elemental: TypeRef) -> TypeRef {
        let mut ty = self.std_type_ref("Pointer");
        ty.bound_types.push(elemental);

        ty
    }

    /// Determines whether the given type is of type `std::Array`.
    pub fn is_std_array(&self, ty: &TypeRef) -> bool {
        ty.instance_of == self.std_type_ref("Array").instance_of
    }

    /// Determines whether the given type is of type `std::Pointer`.
    pub fn is_std_pointer(&self, ty: &TypeRef) -> bool {
        ty.instance_of == self.std_type_ref("Pointer").instance_of
    }
}

impl Deref for TyInferCtx {
    type Target = TyCtx;

    fn deref(&self) -> &Self::Target {
        &self.tcx
    }
}

impl DatabaseContext for TyInferCtx {
    fn db(&self) -> &lume_architect::Database {
        DatabaseContext::db(self.gcx())
    }
}
