use std::fmt::Write;
use std::ops::Deref;
use std::sync::Arc;

use error_snippet::Result;
use indexmap::IndexMap;
use lume_errors::DiagCtx;
use lume_hir::Path;
use lume_session::GlobalCtx;
use lume_span::*;
use serde::{Deserialize, Serialize};

use crate::errors::*;

pub(crate) mod errors;
pub mod trait_impl;

pub const TYPEREF_VOID_ID: NodeId = NodeId::from_usize(PackageId::empty(), 0x0000_0000);
pub const TYPEREF_BOOL_ID: NodeId = NodeId::from_usize(PackageId::empty(), 0x0000_0001);
pub const TYPEREF_INT8_ID: NodeId = NodeId::from_usize(PackageId::empty(), 0x0000_0002);
pub const TYPEREF_INT16_ID: NodeId = NodeId::from_usize(PackageId::empty(), 0x0000_0003);
pub const TYPEREF_INT32_ID: NodeId = NodeId::from_usize(PackageId::empty(), 0x0000_0004);
pub const TYPEREF_INT64_ID: NodeId = NodeId::from_usize(PackageId::empty(), 0x0000_0005);
pub const TYPEREF_UINT8_ID: NodeId = NodeId::from_usize(PackageId::empty(), 0x0000_0006);
pub const TYPEREF_UINT16_ID: NodeId = NodeId::from_usize(PackageId::empty(), 0x0000_0007);
pub const TYPEREF_UINT32_ID: NodeId = NodeId::from_usize(PackageId::empty(), 0x0000_0008);
pub const TYPEREF_UINT64_ID: NodeId = NodeId::from_usize(PackageId::empty(), 0x0000_0009);
pub const TYPEREF_FLOAT32_ID: NodeId = NodeId::from_usize(PackageId::empty(), 0x0000_000A);
pub const TYPEREF_FLOAT64_ID: NodeId = NodeId::from_usize(PackageId::empty(), 0x0000_000B);
pub const TYPEREF_UNKNOWN_ID: NodeId = NodeId::from_usize(PackageId::empty(), 0xFFFF_FFFF);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub idx: usize,
    pub name: String,
    pub ty: TypeRef,
    pub vararg: bool,
    pub location: Location,
}

impl Parameter {
    pub fn is_self(&self) -> bool {
        self.idx == 0 && self.name == "self"
    }
}

/// Defines the signature of a function or method, with parameters and return
/// type.
///
/// While the type infers that it's only applicable for functions, this
/// structure is also used for methods.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionSig<'a> {
    pub params: &'a [Parameter],
    pub type_params: &'a [NodeId],
    pub ret_ty: &'a TypeRef,
}

impl FunctionSig<'_> {
    pub fn is_instanced(&self) -> bool {
        self.params.iter().any(|param| param.is_self())
    }

    pub fn is_vararg(&self) -> bool {
        self.params.iter().rev().any(|param| param.vararg)
    }

    pub fn to_owned(&self) -> FunctionSigOwned {
        FunctionSigOwned {
            params: self.params.to_owned(),
            type_params: self.type_params.to_owned(),
            ret_ty: self.ret_ty.to_owned(),
        }
    }
}

/// Defines the signature of a function or method, with parameters and return
/// type.
///
/// While the type infers that it's only applicable for functions, this
/// structure is also used for methods.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSigOwned {
    pub params: Vec<Parameter>,
    pub type_params: Vec<NodeId>,
    pub ret_ty: TypeRef,
}

impl FunctionSigOwned {
    pub fn is_instanced(&self) -> bool {
        self.params.iter().any(|param| param.is_self())
    }

    pub fn is_vararg(&self) -> bool {
        self.params.iter().rev().any(|param| param.vararg)
    }

    pub fn as_ref(&'_ self) -> FunctionSig<'_> {
        FunctionSig {
            params: &self.params,
            type_params: &self.type_params,
            ret_ty: &self.ret_ty,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub id: NodeId,
    pub name: Path,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MethodKind {
    Implementation,
    Intrinsic,
    TraitImplementation,
    TraitDefinition,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    pub id: NodeId,
    pub kind: MethodKind,
    pub callee: TypeRef,
    pub name: Path,
}

impl Method {
    /// Determines whether the method is intrinsic.
    #[inline]
    pub fn is_intrinsic(&self) -> bool {
        self.kind == MethodKind::Intrinsic
    }

    /// Determines whether the method is a trait method definition.
    #[inline]
    pub fn is_trait_definition(&self) -> bool {
        self.kind == MethodKind::TraitDefinition
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeKind {
    /// Defines a non-value type.
    Void,

    /// The type is a regular user-defined struct.
    Struct,

    /// The type is a regular user-defined trait.
    Trait,

    /// The type is a regular user-defined enumeration.
    Enum,

    /// Defines a 1-bit boolean type.
    Bool,

    /// Defines an N-bit signed integer type.
    Int(u8),

    /// Defines an N-bit unsigned integer type.
    UInt(u8),

    /// Defines an N-bit floating point type.
    Float(u8),

    /// Defines a string type.
    String,

    /// The type is a reference to a type parameter in the current scope.
    TypeParameter,
}

impl TypeKind {
    #[inline]
    pub fn transport(&self) -> TypeTransport {
        match self {
            TypeKind::Void | TypeKind::Bool | TypeKind::Int(_) | TypeKind::UInt(_) | TypeKind::Float(_) => {
                TypeTransport::Copy
            }
            TypeKind::Struct | TypeKind::Trait | TypeKind::Enum | TypeKind::String | TypeKind::TypeParameter => {
                TypeTransport::Reference
            }
        }
    }

    #[inline]
    pub fn is_ref_type(&self) -> bool {
        self.transport() == TypeTransport::Reference
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeTransport {
    /// The type is fully copied when passed as an argument or returned from a
    /// function.
    Copy,

    /// The type uses the same memory location and is passed by reference.
    Reference,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub id: NodeId,
    pub kind: TypeKind,
    pub name: Path,
}

impl Type {
    /// Creates a new [`Type`] with an inner type of [`TypeKind::Void`].
    pub fn void() -> Self {
        Self {
            id: TYPEREF_VOID_ID,
            kind: TypeKind::Void,
            name: Path::void(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Bool`].
    pub fn bool() -> Self {
        Self {
            id: TYPEREF_BOOL_ID,
            kind: TypeKind::Bool,
            name: Path::boolean(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Int`] with 8
    /// bits.
    pub fn i8() -> Self {
        Self {
            id: TYPEREF_INT8_ID,
            kind: TypeKind::Int(8),
            name: Path::i8(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Int`] with 16
    /// bits.
    pub fn i16() -> Self {
        Self {
            id: TYPEREF_INT16_ID,
            kind: TypeKind::Int(16),
            name: Path::i16(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Int`] with 32
    /// bits.
    pub fn i32() -> Self {
        Self {
            id: TYPEREF_INT32_ID,
            kind: TypeKind::Int(32),
            name: Path::i32(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Int`] with 64
    /// bits.
    pub fn i64() -> Self {
        Self {
            id: TYPEREF_INT64_ID,
            kind: TypeKind::Int(64),
            name: Path::i64(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::UInt`] with 8
    /// bits.
    pub fn u8() -> Self {
        Self {
            id: TYPEREF_UINT8_ID,
            kind: TypeKind::UInt(8),
            name: Path::u8(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::UInt`] with 16
    /// bits.
    pub fn u16() -> Self {
        Self {
            id: TYPEREF_UINT16_ID,
            kind: TypeKind::UInt(16),
            name: Path::u16(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::UInt`] with 32
    /// bits.
    pub fn u32() -> Self {
        Self {
            id: TYPEREF_UINT32_ID,
            kind: TypeKind::UInt(32),
            name: Path::u32(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::UInt`] with 64
    /// bits.
    pub fn u64() -> Self {
        Self {
            id: TYPEREF_UINT64_ID,
            kind: TypeKind::UInt(64),
            name: Path::u64(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Float`] with 32
    /// bits.
    pub fn f32() -> Self {
        Self {
            id: TYPEREF_FLOAT32_ID,
            kind: TypeKind::Float(32),
            name: Path::f32(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Float`] with 64
    /// bits.
    pub fn f64() -> Self {
        Self {
            id: TYPEREF_FLOAT64_ID,
            kind: TypeKind::Float(64),
            name: Path::f64(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct TypeRef {
    pub instance_of: NodeId,
    pub bound_types: Vec<TypeRef>,

    #[serde(skip)]
    pub location: Location,
}

impl TypeRef {
    /// Creates a new [`TypeRef`] with the given instance.
    pub fn new(instance: NodeId, location: Location) -> Self {
        Self {
            instance_of: instance,
            bound_types: vec![],
            location,
        }
    }

    /// Assigns a new location to the [`TypeRef`].
    #[inline]
    #[must_use]
    pub fn with_location(mut self, location: Location) -> Self {
        self.location = location;
        self
    }

    /// Creates a new [`TypeRef`] with an inner type of [`TypeKind::Void`].
    pub fn void() -> Self {
        Self {
            instance_of: TYPEREF_VOID_ID,
            bound_types: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Bool`].
    pub fn bool() -> Self {
        Self {
            instance_of: TYPEREF_BOOL_ID,
            bound_types: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Int`] with 8
    /// bits.
    pub fn i8() -> Self {
        Self {
            instance_of: TYPEREF_INT8_ID,
            bound_types: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Int`] with 16
    /// bits.
    pub fn i16() -> Self {
        Self {
            instance_of: TYPEREF_INT16_ID,
            bound_types: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Int`] with 32
    /// bits.
    pub fn i32() -> Self {
        Self {
            instance_of: TYPEREF_INT32_ID,
            bound_types: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Int`] with 64
    /// bits.
    pub fn i64() -> Self {
        Self {
            instance_of: TYPEREF_INT64_ID,
            bound_types: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::UInt`] with 8
    /// bits.
    pub fn u8() -> Self {
        Self {
            instance_of: TYPEREF_UINT8_ID,
            bound_types: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::UInt`] with 16
    /// bits.
    pub fn u16() -> Self {
        Self {
            instance_of: TYPEREF_UINT16_ID,
            bound_types: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::UInt`] with 32
    /// bits.
    pub fn u32() -> Self {
        Self {
            instance_of: TYPEREF_UINT32_ID,
            bound_types: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::UInt`] with 64
    /// bits.
    pub fn u64() -> Self {
        Self {
            instance_of: TYPEREF_UINT64_ID,
            bound_types: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Float`] with 32
    /// bits.
    pub fn f32() -> Self {
        Self {
            instance_of: TYPEREF_FLOAT32_ID,
            bound_types: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Float`] with 64
    /// bits.
    pub fn f64() -> Self {
        Self {
            instance_of: TYPEREF_FLOAT64_ID,
            bound_types: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`TypeRef`] with an invalid inner type, meant to
    /// be used before any types are actually resolved.
    pub fn unknown() -> Self {
        Self {
            instance_of: TYPEREF_UNKNOWN_ID,
            bound_types: vec![],
            location: Location::empty(),
        }
    }

    /// Determines if the type is `void`.
    pub fn is_void(&self) -> bool {
        self.instance_of == TYPEREF_VOID_ID
    }

    /// Determines if the type is a `bool`.
    pub fn is_bool(&self) -> bool {
        self.instance_of == TYPEREF_BOOL_ID
    }

    /// Determines if the type is an integer.
    pub fn is_integer(&self) -> bool {
        matches!(
            self.instance_of,
            TYPEREF_INT8_ID
                | TYPEREF_INT16_ID
                | TYPEREF_INT32_ID
                | TYPEREF_INT64_ID
                | TYPEREF_UINT8_ID
                | TYPEREF_UINT16_ID
                | TYPEREF_UINT32_ID
                | TYPEREF_UINT64_ID
        )
    }

    /// Determines if the type is a floating-point number.
    pub fn is_float(&self) -> bool {
        matches!(self.instance_of, TYPEREF_FLOAT32_ID | TYPEREF_FLOAT64_ID)
    }

    /// Determines the bitwidth of the type.
    ///
    /// # Panics
    ///
    /// Panics if the type is neither an integer nor a floating-point number.
    pub fn bitwidth(&self) -> u8 {
        match self.instance_of {
            TYPEREF_INT8_ID | TYPEREF_UINT8_ID => 8,
            TYPEREF_INT16_ID | TYPEREF_UINT16_ID => 16,
            TYPEREF_INT32_ID | TYPEREF_UINT32_ID | TYPEREF_FLOAT32_ID => 32,
            TYPEREF_INT64_ID | TYPEREF_UINT64_ID | TYPEREF_FLOAT64_ID => 64,
            _ => panic!("bitwidth of non-int, non-float type is invalid"),
        }
    }

    /// Determines the signedness of the type.
    ///
    /// # Panics
    ///
    /// Panics if the type is not an integer.
    pub fn signed(&self) -> bool {
        match self.instance_of {
            TYPEREF_INT8_ID | TYPEREF_INT16_ID | TYPEREF_INT32_ID | TYPEREF_INT64_ID => true,
            TYPEREF_UINT8_ID | TYPEREF_UINT16_ID | TYPEREF_UINT32_ID | TYPEREF_UINT64_ID => false,
            _ => panic!("signedness of non-int is invalid"),
        }
    }

    /// Determines if the type is an `i8`.
    pub fn is_i8(&self) -> bool {
        self.instance_of == TYPEREF_INT8_ID
    }

    /// Determines if the type is an `i16`.
    pub fn is_i16(&self) -> bool {
        self.instance_of == TYPEREF_INT16_ID
    }

    /// Determines if the type is an `i32`.
    pub fn is_i32(&self) -> bool {
        self.instance_of == TYPEREF_INT32_ID
    }

    /// Determines if the type is an `i64`.
    pub fn is_i64(&self) -> bool {
        self.instance_of == TYPEREF_INT64_ID
    }

    /// Determines if the type is an `u8`.
    pub fn is_u8(&self) -> bool {
        self.instance_of == TYPEREF_UINT8_ID
    }

    /// Determines if the type is an `u16`.
    pub fn is_u16(&self) -> bool {
        self.instance_of == TYPEREF_UINT16_ID
    }

    /// Determines if the type is an `u32`.
    pub fn is_u32(&self) -> bool {
        self.instance_of == TYPEREF_UINT32_ID
    }

    /// Determines if the type is an `u64`.
    pub fn is_u64(&self) -> bool {
        self.instance_of == TYPEREF_UINT64_ID
    }

    /// Determines if the type is an `f32`.
    pub fn is_f32(&self) -> bool {
        self.instance_of == TYPEREF_FLOAT32_ID
    }

    /// Determines if the type is an `f64`.
    pub fn is_f64(&self) -> bool {
        self.instance_of == TYPEREF_FLOAT64_ID
    }

    /// Determines if the type is unknown.
    pub fn is_unknown(&self) -> bool {
        self.instance_of == TYPEREF_UNKNOWN_ID
    }

    /// Determines if the type is a scalar type.
    pub fn is_scalar_type(&self) -> bool {
        self.is_bool() || self.is_integer() || self.is_float()
    }
}

impl PartialEq for TypeRef {
    fn eq(&self, other: &Self) -> bool {
        self.instance_of == other.instance_of && self.bound_types == other.bound_types
    }
}

impl Eq for TypeRef {}

impl std::hash::Hash for TypeRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.instance_of.hash(state);
        self.bound_types.hash(state);
    }
}

impl std::fmt::Display for TypeRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.instance_of)?;

        if !self.bound_types.is_empty() {
            write!(
                f,
                "<{}>",
                self.bound_types
                    .iter()
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedTypeRef {
    pub name: String,
    pub bound_types: Vec<NamedTypeRef>,
}

impl NamedTypeRef {
    /// Creates a new [`NamedTypeRef`] with the given name.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            bound_types: vec![],
        }
    }
}

impl std::fmt::Display for NamedTypeRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)?;

        if !self.bound_types.is_empty() {
            f.write_char('<')?;

            for (index, type_arg) in self.bound_types.iter().enumerate() {
                type_arg.fmt(f)?;

                if index < self.bound_types.len() - 1 {
                    f.write_str(", ")?;
                }
            }

            f.write_char('>')?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParameter {
    pub id: NodeId,
    pub name: String,
    pub constraints: Vec<TypeRef>,
    pub location: Location,
}

#[derive(Debug)]
pub struct TypeDatabaseContext {
    pub types: IndexMap<NodeId, Type>,
    pub methods: IndexMap<NodeId, Method>,
    pub functions: IndexMap<NodeId, Function>,
    pub traits: trait_impl::TraitLookup,
}

#[allow(clippy::cast_possible_truncation)]
impl TypeDatabaseContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Gets an iterator which iterates all [`Type`]-instances within
    /// the database context.
    pub fn types(&self) -> impl Iterator<Item = &Type> {
        self.types.values()
    }

    /// Gets the [`Type`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Type`] is not found.
    pub fn type_(&self, id: NodeId) -> Option<&Type> {
        self.types.get(&id)
    }

    /// Gets the [`TypeTransport`] of the [`Type`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Type`] is not found.
    pub fn type_transport(&self, id: NodeId) -> Option<TypeTransport> {
        self.type_(id).map(|ty| ty.kind.transport())
    }

    /// Gets the whether the given [`Type`] is a reference type.
    ///
    /// Returns `None` if the [`Type`] is not found.
    pub fn is_reference_type(&self, id: NodeId) -> Option<bool> {
        self.type_transport(id).map(|ty| ty == TypeTransport::Reference)
    }

    /// Expects the [`Type`] with the given ID, if it exists.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the type wasn't found.
    #[libftrace::traced(level = Trace, fields(id), err)]
    pub fn expect_type(&self, id: NodeId) -> Result<&Type> {
        match self.type_(id) {
            Some(ty) => Ok(ty),
            None => Err(NodeNotFound { id }.into()),
        }
    }

    /// Gets an iterator which iterates all [`Method`]-instances within
    /// the database context.
    pub fn methods(&self) -> impl Iterator<Item = &Method> {
        self.methods.values()
    }

    /// Gets the [`Method`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Method`] is not found.
    pub fn method(&self, id: NodeId) -> Option<&Method> {
        self.methods.get(&id)
    }

    /// Gets an iterator which iterates all [`Function`]-instances within
    /// the database context.
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.functions.values()
    }

    /// Gets the [`Function`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Function`] is not found.
    pub fn function(&self, id: NodeId) -> Option<&Function> {
        self.functions.get(&id)
    }

    /// Attempts to find a [`Type`] with the given name, if any.
    pub fn find_type(&self, name: &Path) -> Option<&Type> {
        self.types()
            .find(|ty| ty.name.is_name_match(name) && !matches!(ty.kind, TypeKind::TypeParameter))
    }

    /// Attempts to find a [`Function`] with the given name, if any.
    pub fn find_function(&self, name: &Path) -> Option<&Function> {
        self.functions().find(|func| func.name == *name)
    }

    /// Attempts to find a [`Method`] with the given name, if any.
    pub fn find_method(&self, name: &Path) -> Option<&Method> {
        self.methods().find(|met| met.name == *name)
    }

    /// Allocates a new [`Function`] with the given name and kind.
    #[inline]
    #[libftrace::traced(level = Trace, fields(id, name = format!("{name:+}")))]
    pub fn func_alloc(&mut self, id: NodeId, name: Path) {
        self.functions.insert(id, Function { id, name });
    }

    /// Allocates a new [`Type`] with the given name and kind.
    #[inline]
    #[libftrace::traced(level = Trace, fields(id, name = format!("{name:+}")))]
    pub fn type_alloc(&mut self, id: NodeId, name: &Path, kind: TypeKind) {
        let existing = self.types.insert(id, Type {
            id,
            kind,
            name: name.clone(),
        });

        assert!(
            existing.is_none(),
            "overwrote type {id} ({:+}, {kind:?} => {name:+}, {:?})",
            existing.as_ref().unwrap().name,
            existing.as_ref().unwrap().kind,
        );
    }

    /// Allocates a new [`Method`] on the given owner [`NodeId`].
    #[inline]
    #[libftrace::traced(level = Trace, fields(id, name = format!("{name:+}")))]
    pub fn method_alloc(&mut self, id: NodeId, owner: TypeRef, name: Path, kind: MethodKind) {
        self.methods.insert(id, Method {
            id,
            kind,
            callee: owner,
            name,
        });
    }
}

impl Default for TypeDatabaseContext {
    fn default() -> Self {
        Self {
            types: indexmap::indexmap! {
                TYPEREF_VOID_ID => Type::void(),
                TYPEREF_BOOL_ID => Type::bool(),
                TYPEREF_INT8_ID => Type::i8(),
                TYPEREF_INT16_ID => Type::i16(),
                TYPEREF_INT32_ID => Type::i32(),
                TYPEREF_INT64_ID => Type::i64(),
                TYPEREF_UINT8_ID => Type::u8(),
                TYPEREF_UINT16_ID => Type::u16(),
                TYPEREF_UINT32_ID => Type::u32(),
                TYPEREF_UINT64_ID => Type::u64(),
                TYPEREF_FLOAT32_ID => Type::f32(),
                TYPEREF_FLOAT64_ID => Type::f64(),
            },
            methods: IndexMap::new(),
            functions: IndexMap::new(),
            traits: trait_impl::TraitLookup::default(),
        }
    }
}

/// Central data structure for performing analysis and checking on types within
/// a compilation job. This structure contains references to all types defined
/// within a source package, as well as all resulting types from expressions,
/// statements, etc.
pub struct TyCtx {
    /// Defines the global context
    gcx: Arc<GlobalCtx>,

    /// Defines the database with all defined types
    db: TypeDatabaseContext,
}

impl TyCtx {
    /// Creates a new instance of [`TyCtx`] with the given global context.
    pub fn new(gcx: Arc<GlobalCtx>) -> Self {
        Self {
            gcx,
            db: TypeDatabaseContext::default(),
        }
    }

    /// Gets the inner global context.
    pub fn gcx(&self) -> &GlobalCtx {
        &self.gcx
    }

    /// Gets the diagnostics context.
    pub fn dcx(&self) -> DiagCtx {
        self.gcx.dcx.clone()
    }

    /// Gets the inner type database context.
    pub fn db(&self) -> &TypeDatabaseContext {
        &self.db
    }

    /// Gets the inner type database context.
    pub fn db_mut(&mut self) -> &mut TypeDatabaseContext {
        &mut self.db
    }
}

impl Deref for TyCtx {
    type Target = GlobalCtx;

    fn deref(&self) -> &Self::Target {
        &self.gcx
    }
}
