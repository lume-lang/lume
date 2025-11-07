use std::fmt::Write;
use std::ops::Deref;
use std::sync::Arc;

use error_snippet::Result;
use indexmap::IndexMap;
use lume_errors::DiagCtx;
use lume_hir::{Path, PathSegment, Visibility};
use lume_session::GlobalCtx;
use lume_span::*;
use serde::{Deserialize, Serialize};

use crate::errors::*;

pub mod errors;

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

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Parameters {
    pub params: Vec<Parameter>,
}

impl Parameters {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, name: String, ty: TypeRef, vararg: bool, location: Location) {
        self.params.push(Parameter {
            idx: self.params.len(),
            name,
            ty,
            vararg,
            location,
        });
    }

    pub fn inner(&self) -> &[Parameter] {
        &self.params
    }

    pub fn len(&self) -> usize {
        self.params.len()
    }

    pub fn is_empty(&self) -> bool {
        self.params.is_empty()
    }

    /// Determines whether the method is instanced, as opposed to static.
    pub fn is_instanced(&self) -> bool {
        if let Some(param) = self.params.first() {
            param.is_self()
        } else {
            false
        }
    }

    pub fn is_vararg(&self) -> bool {
        self.params.iter().any(|param| param.vararg)
    }
}

/// Defines the signature of a function or method, with parameters and return
/// type.
///
/// While the type infers that it's only applicable for functions, this
/// structure is also used for methods.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionSig<'a> {
    pub params: &'a Parameters,
    pub type_params: &'a [NodeId],
    pub ret_ty: &'a TypeRef,
}

impl FunctionSig<'_> {
    pub fn is_instanced(&self) -> bool {
        self.params.is_instanced()
    }

    pub fn is_vararg(&self) -> bool {
        self.params.is_vararg()
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
    pub params: Parameters,
    pub type_params: Vec<NodeId>,
    pub ret_ty: TypeRef,
}

impl FunctionSigOwned {
    pub fn is_instanced(&self) -> bool {
        self.params.is_instanced()
    }

    pub fn is_vararg(&self) -> bool {
        self.params.is_vararg()
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
    pub visibility: Visibility,
    pub type_parameters: Vec<NodeId>,
    pub parameters: Parameters,
    pub return_type: TypeRef,
}

impl Function {
    /// Gets the signature of the function.
    pub fn sig(&'_ self) -> FunctionSig<'_> {
        FunctionSig {
            params: &self.parameters,
            type_params: &self.type_parameters,
            ret_ty: &self.return_type,
        }
    }
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Field {
    pub id: NodeId,
    pub index: usize,
    pub visibility: Visibility,
    pub owner: NodeId,
    pub name: String,
    pub field_type: TypeRef,
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
    pub visibility: Option<Visibility>,
    pub callee: TypeRef,
    pub name: Path,
    pub type_parameters: Vec<NodeId>,
    pub parameters: Parameters,
    pub return_type: TypeRef,
}

impl Method {
    /// Gets the signature of the method.
    pub fn sig(&'_ self) -> FunctionSig<'_> {
        FunctionSig {
            params: &self.parameters,
            type_params: &self.type_parameters,
            ret_ty: &self.return_type,
        }
    }

    /// Determines whether the method is static, as opposed to instanced.
    #[inline]
    pub fn is_static(&self) -> bool {
        !self.parameters.is_instanced()
    }

    /// Determines whether the method is instanced, as opposed to static.
    #[inline]
    pub fn is_instanced(&self) -> bool {
        self.parameters.is_instanced()
    }

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

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub id: NodeId,
    pub name: Path,
    pub type_parameters: Vec<NodeId>,
}

impl Struct {
    pub fn new(reference: &lume_hir::StructDefinition) -> Self {
        Self {
            id: reference.id,
            name: reference.name.clone(),
            type_parameters: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Trait {
    pub id: NodeId,
    pub name: Path,
    pub type_parameters: Vec<NodeId>,
}

impl Trait {
    pub fn new(reference: &lume_hir::TraitDefinition) -> Self {
        Self {
            id: reference.id,
            name: reference.name.clone(),
            type_parameters: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub id: NodeId,
    pub name: Path,
    pub type_parameters: Vec<NodeId>,
}

impl Enum {
    pub fn new(reference: &lume_hir::EnumDefinition) -> Self {
        Self {
            id: reference.id,
            name: reference.name.clone(),
            type_parameters: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumCase {
    pub parent: NodeId,
    pub name: Path,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Implementation {
    pub id: NodeId,
    pub target: Path,
    pub type_parameters: Vec<NodeId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Use {
    pub id: NodeId,
    pub trait_: TypeRef,
    pub target: TypeRef,
    pub type_parameters: Vec<NodeId>,
    pub methods: Vec<NodeId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKindRef {
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

#[derive(Debug, Clone, PartialEq)]
pub enum UserType {
    /// The type is a regular user-defined struct.
    Struct(Box<Struct>),

    /// The type is a regular user-defined trait.
    Trait(Box<Trait>),

    /// The type is a regular user-defined enumeration.
    Enum(Box<Enum>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    /// Defines a non-value type.
    Void,

    /// Defines a 1-bit boolean type.
    Bool,

    /// Defines an N-bit signed integer type.
    Int(u8),

    /// Defines an N-bit unsigned integer type.
    UInt(u8),

    /// Defines an N-bit floating point type.
    Float(u8),

    /// Defines a user-defined type.
    User(UserType),

    /// Defines a string type.
    String,

    /// The type is a reference to a type parameter in the current scope.
    TypeParameter(NodeId),
}

impl TypeKind {
    #[inline]
    pub fn transport(&self) -> TypeTransport {
        match self {
            TypeKind::Void | TypeKind::Bool | TypeKind::Int(_) | TypeKind::UInt(_) | TypeKind::Float(_) => {
                TypeTransport::Copy
            }
            TypeKind::User(_) | TypeKind::String | TypeKind::TypeParameter(_) => TypeTransport::Reference,
        }
    }

    #[inline]
    pub fn is_ref_type(&self) -> bool {
        self.transport() == TypeTransport::Reference
    }

    pub fn as_kind_ref(&self) -> TypeKindRef {
        match self {
            TypeKind::Void => TypeKindRef::Void,
            TypeKind::Bool => TypeKindRef::Bool,
            TypeKind::Int(n) => TypeKindRef::Int(*n),
            TypeKind::UInt(n) => TypeKindRef::UInt(*n),
            TypeKind::Float(n) => TypeKindRef::Float(*n),
            TypeKind::String => TypeKindRef::String,
            TypeKind::User(UserType::Struct(_)) => TypeKindRef::Struct,
            TypeKind::User(UserType::Trait(_)) => TypeKindRef::Trait,
            TypeKind::User(UserType::Enum(_)) => TypeKindRef::Enum,
            TypeKind::TypeParameter(_) => TypeKindRef::TypeParameter,
        }
    }

    pub fn type_parameters(&self) -> &[NodeId] {
        match self {
            TypeKind::Void
            | TypeKind::Bool
            | TypeKind::Int(_)
            | TypeKind::UInt(_)
            | TypeKind::Float(_)
            | TypeKind::String
            | TypeKind::TypeParameter(_) => &[],
            TypeKind::User(UserType::Struct(def)) => &def.type_parameters,
            TypeKind::User(UserType::Trait(def)) => &def.type_parameters,
            TypeKind::User(UserType::Enum(def)) => &def.type_parameters,
        }
    }

    pub fn is_generic(&self) -> bool {
        match self {
            TypeKind::Void
            | TypeKind::Bool
            | TypeKind::Int(_)
            | TypeKind::UInt(_)
            | TypeKind::Float(_)
            | TypeKind::String
            | TypeKind::TypeParameter(_) => false,
            TypeKind::User(UserType::Struct(def)) => !def.type_parameters.is_empty(),
            TypeKind::User(UserType::Trait(def)) => !def.type_parameters.is_empty(),
            TypeKind::User(UserType::Enum(def)) => !def.type_parameters.is_empty(),
        }
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

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Int(8)`].
    pub fn i8() -> Self {
        Self {
            id: TYPEREF_INT8_ID,
            kind: TypeKind::Int(8),
            name: Path::i8(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Int(16)`].
    pub fn i16() -> Self {
        Self {
            id: TYPEREF_INT16_ID,
            kind: TypeKind::Int(16),
            name: Path::i16(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Int(32)`].
    pub fn i32() -> Self {
        Self {
            id: TYPEREF_INT32_ID,
            kind: TypeKind::Int(32),
            name: Path::i32(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Int(64)`].
    pub fn i64() -> Self {
        Self {
            id: TYPEREF_INT64_ID,
            kind: TypeKind::Int(64),
            name: Path::i64(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::UInt(8)`].
    pub fn u8() -> Self {
        Self {
            id: TYPEREF_UINT8_ID,
            kind: TypeKind::UInt(8),
            name: Path::u8(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::UInt(16)`].
    pub fn u16() -> Self {
        Self {
            id: TYPEREF_UINT16_ID,
            kind: TypeKind::UInt(16),
            name: Path::u16(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::UInt(32)`].
    pub fn u32() -> Self {
        Self {
            id: TYPEREF_UINT32_ID,
            kind: TypeKind::UInt(32),
            name: Path::u32(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::UInt(64)`].
    pub fn u64() -> Self {
        Self {
            id: TYPEREF_UINT64_ID,
            kind: TypeKind::UInt(64),
            name: Path::u64(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Float(32)`].
    pub fn f32() -> Self {
        Self {
            id: TYPEREF_FLOAT32_ID,
            kind: TypeKind::Float(32),
            name: Path::f32(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Float(64)`].
    pub fn f64() -> Self {
        Self {
            id: TYPEREF_FLOAT64_ID,
            kind: TypeKind::Float(64),
            name: Path::f64(),
        }
    }

    pub fn is_type_parameter(&self) -> bool {
        matches!(self.kind, TypeKind::TypeParameter(_))
    }

    pub fn is_generic(&self) -> bool {
        self.kind.is_generic()
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct TypeRef {
    pub instance_of: NodeId,
    pub type_arguments: Vec<TypeRef>,

    #[serde(skip)]
    pub location: Location,
}

impl TypeRef {
    /// Creates a new [`TypeRef`] with the given instance.
    pub fn new(instance: NodeId, location: Location) -> Self {
        Self {
            instance_of: instance,
            type_arguments: vec![],
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

    /// Creates a new [`TypeRef`] with an inner type of [`TypeKindRef::Void`].
    pub fn void() -> Self {
        Self {
            instance_of: TYPEREF_VOID_ID,
            type_arguments: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Bool`].
    pub fn bool() -> Self {
        Self {
            instance_of: TYPEREF_BOOL_ID,
            type_arguments: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Int(8)`].
    pub fn i8() -> Self {
        Self {
            instance_of: TYPEREF_INT8_ID,
            type_arguments: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Int(16)`].
    pub fn i16() -> Self {
        Self {
            instance_of: TYPEREF_INT16_ID,
            type_arguments: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Int(32)`].
    pub fn i32() -> Self {
        Self {
            instance_of: TYPEREF_INT32_ID,
            type_arguments: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Int(64)`].
    pub fn i64() -> Self {
        Self {
            instance_of: TYPEREF_INT64_ID,
            type_arguments: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::UInt(8)`].
    pub fn u8() -> Self {
        Self {
            instance_of: TYPEREF_UINT8_ID,
            type_arguments: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::UInt(16)`].
    pub fn u16() -> Self {
        Self {
            instance_of: TYPEREF_UINT16_ID,
            type_arguments: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::UInt(32)`].
    pub fn u32() -> Self {
        Self {
            instance_of: TYPEREF_UINT32_ID,
            type_arguments: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::UInt(64)`].
    pub fn u64() -> Self {
        Self {
            instance_of: TYPEREF_UINT64_ID,
            type_arguments: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Float(32)`].
    pub fn f32() -> Self {
        Self {
            instance_of: TYPEREF_FLOAT32_ID,
            type_arguments: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`Type`] with an inner type of [`TypeKind::Float(64)`].
    pub fn f64() -> Self {
        Self {
            instance_of: TYPEREF_FLOAT64_ID,
            type_arguments: vec![],
            location: Location::empty(),
        }
    }

    /// Creates a new [`TypeRef`] with an invalid inner type, meant to
    /// be used before any types are actually resolved.
    pub fn unknown() -> Self {
        Self {
            instance_of: TYPEREF_UNKNOWN_ID,
            type_arguments: vec![],
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
        self.instance_of == other.instance_of && self.type_arguments == other.type_arguments
    }
}

impl Eq for TypeRef {}

impl std::hash::Hash for TypeRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.instance_of.hash(state);
        self.type_arguments.hash(state);
    }
}

impl std::fmt::Display for TypeRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.instance_of)?;

        if !self.type_arguments.is_empty() {
            write!(
                f,
                "<{}>",
                self.type_arguments
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
    pub type_arguments: Vec<NamedTypeRef>,
}

impl NamedTypeRef {
    /// Creates a new [`NamedTypeRef`] with the given name.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            type_arguments: vec![],
        }
    }
}

impl std::fmt::Display for NamedTypeRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)?;

        if !self.type_arguments.is_empty() {
            f.write_char('<')?;

            for (index, type_arg) in self.type_arguments.iter().enumerate() {
                type_arg.fmt(f)?;

                if index < self.type_arguments.len() - 1 {
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
    pub fields: IndexMap<NodeId, Field>,
    pub methods: IndexMap<NodeId, Method>,
    pub functions: IndexMap<NodeId, Function>,
    pub type_parameters: IndexMap<NodeId, TypeParameter>,
    pub implementations: IndexMap<NodeId, Implementation>,
    pub uses: IndexMap<NodeId, Use>,
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

    /// Gets an iterator which iterates all [`Type`]-instances within
    /// the database context.
    pub fn types_mut(&mut self) -> impl Iterator<Item = &mut Type> {
        self.types.values_mut()
    }

    /// Gets the [`Type`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Type`] is not found.
    pub fn type_(&self, id: NodeId) -> Option<&Type> {
        self.types.get(&id)
    }

    /// Gets the [`Type`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Type`] is not found.
    pub fn type_mut(&mut self, id: NodeId) -> Option<&mut Type> {
        self.types.get_mut(&id)
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

    /// Gets the whether the given [`Type`] is a value type.
    ///
    /// Returns `None` if the [`Type`] is not found.
    pub fn is_value_type(&self, id: NodeId) -> Option<bool> {
        self.type_transport(id).map(|ty| ty == TypeTransport::Copy)
    }

    /// Gets the [`Type`] with the given ID, if any, as a type parameter. If the
    /// type is not a type parameter, returns `None`.
    ///
    /// Returns `None` if the [`Type`] is not found.
    pub fn type_as_param(&self, id: NodeId) -> Option<&TypeParameter> {
        if let Some(TypeKind::TypeParameter(param_id)) = self.type_(id).map(|ty| &ty.kind) {
            self.type_parameter(*param_id)
        } else {
            None
        }
    }

    /// Expects the [`Type`] with the given ID, if it exists.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the type wasn't found.
    pub fn ty_expect(&self, id: NodeId) -> Result<&Type> {
        match self.type_(id) {
            Some(ty) => Ok(ty),
            None => Err(NodeNotFound { id }.into()),
        }
    }

    /// Expects the [`Type`] with the given ID to be a [`Struct`] kind.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the type wasn't found or if the found type did not
    /// have a [`TypeKindRef`] of [`TypeKindRef::Struct`].
    pub fn ty_expect_struct(&self, id: NodeId) -> Result<&Struct> {
        let ty = self.ty_expect(id)?;

        if let TypeKind::User(UserType::Struct(tr)) = &ty.kind {
            Ok(tr.as_ref())
        } else {
            Err(UnexpectedTypeKind {
                expected: TypeKindRef::Struct,
                found: ty.kind.as_kind_ref(),
            }
            .into())
        }
    }

    /// Expects the [`Type`] with the given ID to be a [`Trait`] kind.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the type wasn't found or if the found type did not
    /// have a [`TypeKindRef`] of [`TypeKindRef::Trait`].
    pub fn ty_expect_trait(&self, id: NodeId) -> Result<&Trait> {
        let ty = self.ty_expect(id)?;

        if let TypeKind::User(UserType::Trait(tr)) = &ty.kind {
            Ok(tr.as_ref())
        } else {
            Err(UnexpectedTypeKind {
                expected: TypeKindRef::Trait,
                found: ty.kind.as_kind_ref(),
            }
            .into())
        }
    }

    /// Gets an iterator which iterates all [`Field`]-instances within
    /// the database context.
    pub fn fields(&self) -> impl Iterator<Item = &Field> {
        self.fields.values()
    }

    /// Gets an iterator which iterates all [`Field`]-instances within
    /// the database context.
    pub fn fields_mut(&mut self) -> impl Iterator<Item = &mut Field> {
        self.fields.values_mut()
    }

    /// Gets the [`Field`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Field`] is not found.
    pub fn field(&self, id: NodeId) -> Option<&Field> {
        self.fields.get(&id)
    }

    /// Gets the [`Field`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Field`] is not found.
    pub fn field_mut(&mut self, id: NodeId) -> Option<&mut Field> {
        self.fields.get_mut(&id)
    }

    /// Gets an iterator which iterates all [`Method`]-instances within
    /// the database context.
    pub fn methods(&self) -> impl Iterator<Item = &Method> {
        self.methods.values()
    }

    /// Gets an iterator which iterates all [`Method`]-instances within
    /// the database context.
    pub fn methods_mut(&mut self) -> impl Iterator<Item = &mut Method> {
        self.methods.values_mut()
    }

    /// Gets an iterator which iterates all [`Use`]-instances within
    /// the database context.
    pub fn uses(&self) -> impl Iterator<Item = &Use> {
        self.uses.values()
    }

    /// Gets an iterator which iterates all [`Use`]-instances within
    /// the database context.
    pub fn uses_mut(&mut self) -> impl Iterator<Item = &mut Use> {
        self.uses.values_mut()
    }

    /// Gets the [`Use`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Use`] is not found.
    pub fn use_(&self, id: NodeId) -> Option<&Use> {
        self.uses.get(&id)
    }

    /// Gets the [`Use`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Use`] is not found.
    pub fn use_mut(&mut self, id: NodeId) -> Option<&mut Use> {
        self.uses.get_mut(&id)
    }

    /// Gets the [`Method`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Method`] is not found.
    pub fn method(&self, id: NodeId) -> Option<&Method> {
        self.methods.get(&id)
    }

    /// Gets the [`Method`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Method`] is not found.
    pub fn method_mut(&mut self, id: NodeId) -> Option<&mut Method> {
        self.methods.get_mut(&id)
    }

    /// Gets an iterator which iterates all [`Function`]-instances within
    /// the database context.
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.functions.values()
    }

    /// Gets an iterator which iterates all [`Function`]-instances within
    /// the database context.
    pub fn functions_mut(&mut self) -> impl Iterator<Item = &mut Function> {
        self.functions.values_mut()
    }

    /// Gets the [`Function`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Function`] is not found.
    pub fn function(&self, id: NodeId) -> Option<&Function> {
        self.functions.get(&id)
    }

    /// Gets the [`Function`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Function`] is not found.
    pub fn function_mut(&mut self, id: NodeId) -> Option<&mut Function> {
        self.functions.get_mut(&id)
    }

    /// Gets the [`TypeParameter`] with the given ID, if any.
    ///
    /// Returns `None` if the [`TypeParameter`] is not found.
    pub fn type_parameter(&self, id: NodeId) -> Option<&TypeParameter> {
        self.type_parameters.get(&id)
    }

    /// Gets the [`TypeParameter`] with the given ID, if any.
    ///
    /// Returns `None` if the [`TypeParameter`] is not found.
    pub fn type_parameter_mut(&mut self, id: NodeId) -> Option<&mut TypeParameter> {
        self.type_parameters.get_mut(&id)
    }

    /// Gets the [`Implementation`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Implementation`] is not found.
    pub fn implementation(&self, id: NodeId) -> Option<&Implementation> {
        self.implementations.get(&id)
    }

    /// Gets the [`Implementation`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Implementation`] is not found.
    pub fn implementation_mut(&mut self, id: NodeId) -> Option<&mut Implementation> {
        self.implementations.get_mut(&id)
    }

    /// Gets an iterator which iterates all [`Item`]-instances where
    /// the item refers to a [`Method`], which are defined on the given
    /// [`Item`].
    pub fn methods_on(&self, id: NodeId) -> impl Iterator<Item = &Method> {
        let mut methods: Vec<Box<dyn Iterator<Item = &Method>>> =
            vec![Box::new(self.methods().filter(move |m| m.callee.instance_of == id))];

        if let Some(type_param) = self.type_as_param(id) {
            for constraint in &type_param.constraints {
                methods.push(Box::new(self.methods_on(constraint.instance_of)));
            }
        }

        methods.into_iter().flatten()
    }

    /// Gets an iterator which iterates all [`Use`]-instances where
    /// the item refers to a [`Use`], which are implementation on the given
    /// [`Item`].
    pub fn uses_on(&self, on: &TypeRef) -> impl Iterator<Item = &Use> {
        self.uses().filter(move |u| &u.target == on)
    }

    /// Gets an iterator which iterates all [`Use`]-instances which implement
    /// the given trait item.
    pub fn uses_of(&self, on: &TypeRef) -> impl Iterator<Item = &Use> {
        self.uses().filter(move |u| &u.trait_ == on)
    }

    /// Attempts to find a [`Type`] with the given name, if any.
    pub fn find_type(&self, name: &Path) -> Option<&Type> {
        self.types()
            .find(|ty| ty.name.is_name_match(name) && !matches!(ty.kind, TypeKind::TypeParameter(_)))
    }

    /// Attempts to find a [`Function`] with the given name, if any.
    pub fn find_function(&self, name: &Path) -> Option<&Function> {
        self.functions().find(|func| func.name == *name)
    }

    /// Attempts to find all [`Field`]s on the given parent type.
    pub fn find_fields(&self, owner: NodeId) -> impl Iterator<Item = &Field> {
        self.fields().filter(move |prop| prop.owner == owner)
    }

    /// Attempts to find a [`Field`] with the given name on the given parent
    /// type, if any.
    pub fn find_field(&self, owner: NodeId, name: &String) -> Option<&Field> {
        self.fields().find(|prop| prop.owner == owner && prop.name == *name)
    }

    /// Attempts to find a [`Method`] with the given name, if any.
    pub fn find_method(&self, name: &Path) -> Option<&Method> {
        self.methods().find(|met| met.name == *name)
    }

    /// Allocates a new [`Function`] with the given name and kind.
    #[inline]
    pub fn func_alloc(&mut self, id: NodeId, name: Path, visibility: Visibility) -> NodeId {
        self.functions.insert(id, Function {
            id,
            name,
            visibility,
            type_parameters: Vec::new(),
            parameters: Parameters::new(),
            return_type: TypeRef::unknown(),
        });

        id
    }

    /// Allocates a new [`Type`] with the given name and kind.
    #[inline]
    pub fn type_alloc(&mut self, id: NodeId, name: &Path, kind: TypeKind) -> NodeId {
        let existing = self.types.insert(id, Type {
            id,
            kind,
            name: name.clone(),
        });

        assert!(
            existing.is_none(),
            "overwrote type {id} ({:+} => {name:+})",
            existing.unwrap().name
        );

        id
    }

    /// Allocates a new [`Implementation`] with the target.
    #[inline]
    pub fn impl_alloc(&mut self, id: NodeId, target: Path) -> NodeId {
        let existing = self.implementations.insert(id, Implementation {
            id,
            target,
            type_parameters: Vec::new(),
        });

        assert!(existing.is_none());

        id
    }

    /// Allocates a new [`Use`] with the target.
    #[inline]
    pub fn use_alloc(&mut self, id: NodeId) -> NodeId {
        self.uses.insert(id, Use {
            id,
            trait_: TypeRef::unknown(),
            target: TypeRef::unknown(),
            type_parameters: Vec::new(),
            methods: Vec::new(),
        });

        id
    }

    /// Allocates a new [`Field`] on the given [`Item`].
    ///
    /// # Errors
    ///
    /// Returns `Err` if `owner` refers to an [`Item`] which could not be found,
    /// or is not a type.
    #[inline]
    pub fn field_alloc(
        &mut self,
        id: NodeId,
        index: usize,
        owner: NodeId,
        name: String,
        visibility: Visibility,
    ) -> NodeId {
        self.fields.insert(id, Field {
            id,
            index,
            owner,
            name,
            visibility,
            field_type: TypeRef::unknown(),
        });

        id
    }

    /// Allocates a new [`Method`] on the given [`Item`].
    ///
    /// # Errors
    ///
    /// Returns `Err` if `owner` refers to an [`Item`] which could not be found,
    /// or is not a type.
    #[inline]
    pub fn method_alloc(
        &mut self,
        id: NodeId,
        owner: TypeRef,
        name: Path,
        visibility: Option<Visibility>,
        kind: MethodKind,
    ) -> NodeId {
        self.methods.insert(id, Method {
            id,
            kind,
            callee: owner,
            name,
            visibility,
            parameters: Parameters::new(),
            type_parameters: Vec::new(),
            return_type: TypeRef::unknown(),
        });

        id
    }

    /// Allocates a new [`TypeParameter`] with the given name and kind.
    #[inline]
    pub fn type_param_alloc(&mut self, id: NodeId, name: String, loc: Location) -> NodeId {
        let existing = self.type_parameters.insert(id, TypeParameter {
            id,
            name,
            constraints: Vec::new(),
            location: loc,
        });

        assert!(existing.is_none());

        id
    }

    /// Gets the type parameters defined on the [`lume_hir::Node`] with the
    /// given ID.
    ///
    /// # Errors
    ///
    /// Returns `Err` if no type with the given ID was found in the context,
    /// or if the found type is non-generic (such as [`TypeKind::Void`] or
    /// [`TypeKind::TypeParameter`]).
    #[tracing::instrument(level = "TRACE", skip(self), err, ret)]
    pub fn type_params_of(&self, id: NodeId) -> Result<&[NodeId]> {
        if let Some(ty) = self.type_(id) {
            return Ok(ty.kind.type_parameters());
        }

        if let Some(method) = self.method(id) {
            return Ok(&method.type_parameters);
        }

        if let Some(func) = self.function(id) {
            return Ok(&func.type_parameters);
        }

        if let Some(implementation) = self.implementation(id) {
            return Ok(&implementation.type_parameters);
        }

        if let Some(trait_impl) = self.use_(id) {
            return Ok(&trait_impl.type_parameters);
        }

        Err(TypeParametersOnNonGenericItem { id }.into())
    }

    /// Pushes a new type parameter to the [`lume_hir::Node`] with the given ID.
    ///
    /// # Errors
    ///
    /// Returns `Err` if no type with the given ID was found in the context,
    /// or if the found type is non-generic (such as [`TypeKind::Void`] or
    /// [`TypeKind::TypeParameter`]).
    pub fn push_type_param(&mut self, id: NodeId, type_id: NodeId) -> Result<()> {
        if !self.type_params_of(id)?.contains(&type_id) {
            if let Some(ty) = self.type_mut(id) {
                match &mut ty.kind {
                    TypeKind::User(UserType::Struct(ty)) => {
                        ty.type_parameters.push(type_id);
                    }
                    TypeKind::User(UserType::Trait(ty)) => {
                        ty.type_parameters.push(type_id);
                    }
                    TypeKind::User(UserType::Enum(ty)) => {
                        ty.type_parameters.push(type_id);
                    }
                    _ => {}
                }
            }

            if let Some(method) = self.method_mut(id) {
                method.type_parameters.push(type_id);
                return Ok(());
            }

            if let Some(func) = self.function_mut(id) {
                func.type_parameters.push(type_id);
                return Ok(());
            }

            if let Some(implementation) = self.implementation_mut(id) {
                implementation.type_parameters.push(type_id);
                return Ok(());
            }

            if let Some(trait_impl) = self.use_mut(id) {
                trait_impl.type_parameters.push(type_id);
                return Ok(());
            }
        }

        Ok(())
    }

    /// Checks whether the given namespace exists for any item within the
    /// database.
    pub fn namespace_exists(&self, root: &[PathSegment]) -> bool {
        for ty in self.types.values() {
            if ty.name.root.starts_with(root) {
                return true;
            }
        }

        false
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
            fields: IndexMap::new(),
            methods: IndexMap::new(),
            functions: IndexMap::new(),
            type_parameters: IndexMap::new(),
            implementations: IndexMap::new(),
            uses: IndexMap::new(),
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
