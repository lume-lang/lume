use std::{fmt::Write, ops::Deref, sync::Arc};

use error_snippet::Result;
use lume_errors::DiagCtx;
use lume_session::GlobalCtx;

use crate::errors::*;
use lume_hir::{FunctionId, ImplId, MethodId, Path, PropertyId, TypeId, TypeParameterId, UseId, Visibility};
use lume_span::{ItemId, Location};

pub mod errors;

pub trait WithTypeParameters {
    /// Gets the type parameters of the current instance.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the instance was not found within the type context.
    fn type_params(self, tcx: &TypeDatabaseContext) -> Result<&Vec<TypeParameterId>>;

    /// Gets the type parameters of the current instance.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the instance was not found within the type context.
    fn type_params_mut(self, tcx: &mut TypeDatabaseContext) -> Result<&mut Vec<TypeParameterId>>;
}

impl WithTypeParameters for FunctionId {
    fn type_params(self, tcx: &TypeDatabaseContext) -> Result<&Vec<TypeParameterId>> {
        match tcx.function(self) {
            Some(m) => Ok(&m.type_parameters),
            None => Err(FunctionNotFound { id: self }.into()),
        }
    }

    fn type_params_mut(self, tcx: &mut TypeDatabaseContext) -> Result<&mut Vec<TypeParameterId>> {
        match tcx.function_mut(self) {
            Some(m) => Ok(&mut m.type_parameters),
            None => Err(FunctionNotFound { id: self }.into()),
        }
    }
}

impl WithTypeParameters for ImplId {
    fn type_params(self, tcx: &TypeDatabaseContext) -> Result<&Vec<TypeParameterId>> {
        match tcx.implementation(self) {
            Some(m) => Ok(&m.type_parameters),
            None => Err(ImplNotFound { id: self }.into()),
        }
    }

    fn type_params_mut(self, tcx: &mut TypeDatabaseContext) -> Result<&mut Vec<TypeParameterId>> {
        match tcx.implementation_mut(self) {
            Some(m) => Ok(&mut m.type_parameters),
            None => Err(ImplNotFound { id: self }.into()),
        }
    }
}

impl WithTypeParameters for MethodId {
    fn type_params(self, tcx: &TypeDatabaseContext) -> Result<&Vec<TypeParameterId>> {
        match tcx.method(self) {
            Some(m) => Ok(&m.type_parameters),
            None => Err(MethodNotFound { id: self }.into()),
        }
    }

    fn type_params_mut(self, tcx: &mut TypeDatabaseContext) -> Result<&mut Vec<TypeParameterId>> {
        match tcx.method_mut(self) {
            Some(m) => Ok(&mut m.type_parameters),
            None => Err(MethodNotFound { id: self }.into()),
        }
    }
}

impl WithTypeParameters for UseId {
    fn type_params(self, tcx: &TypeDatabaseContext) -> Result<&Vec<TypeParameterId>> {
        match tcx.use_(self) {
            Some(m) => Ok(&m.type_parameters),
            None => Err(UseNotFound { id: self }.into()),
        }
    }

    fn type_params_mut(self, tcx: &mut TypeDatabaseContext) -> Result<&mut Vec<TypeParameterId>> {
        match tcx.use_mut(self) {
            Some(m) => Ok(&mut m.type_parameters),
            None => Err(UseNotFound { id: self }.into()),
        }
    }
}

impl WithTypeParameters for TypeId {
    fn type_params(self, tcx: &TypeDatabaseContext) -> Result<&Vec<TypeParameterId>> {
        let Some(ty) = tcx.type_(self) else {
            return Err(TypeNotFound { id: self }.into());
        };

        match &ty.kind {
            TypeKind::User(UserType::Struct(k)) => Ok(&k.type_parameters),
            TypeKind::User(UserType::Trait(k)) => Ok(&k.type_parameters),
            kind => Err(TypeParametersOnNonGenericType { ty: kind.as_kind_ref() }.into()),
        }
    }

    fn type_params_mut(self, tcx: &mut TypeDatabaseContext) -> Result<&mut Vec<TypeParameterId>> {
        let Some(ty) = tcx.type_mut(self) else {
            return Err(TypeNotFound { id: self }.into());
        };

        match &mut ty.kind {
            TypeKind::User(UserType::Struct(k)) => Ok(&mut k.type_parameters),
            TypeKind::User(UserType::Trait(k)) => Ok(&mut k.type_parameters),
            kind => Err(TypeParametersOnNonGenericType { ty: kind.as_kind_ref() }.into()),
        }
    }
}

const TYPEREF_VOID_ID: TypeId = TypeId(0x0000_00000);
const TYPEREF_BOOL_ID: TypeId = TypeId(0x0000_00001);
const TYPEREF_INT8_ID: TypeId = TypeId(0x0000_00002);
const TYPEREF_INT16_ID: TypeId = TypeId(0x0000_00003);
const TYPEREF_INT32_ID: TypeId = TypeId(0x0000_00004);
const TYPEREF_INT64_ID: TypeId = TypeId(0x0000_00005);
const TYPEREF_UINT8_ID: TypeId = TypeId(0x0000_00006);
const TYPEREF_UINT16_ID: TypeId = TypeId(0x0000_00007);
const TYPEREF_UINT32_ID: TypeId = TypeId(0x0000_00008);
const TYPEREF_UINT64_ID: TypeId = TypeId(0x0000_00009);
const TYPEREF_FLOAT32_ID: TypeId = TypeId(0x0000_0000A);
const TYPEREF_FLOAT64_ID: TypeId = TypeId(0x0000_0000B);
const TYPEREF_STRING_ID: TypeId = TypeId(0x0000_0000C);
const TYPEREF_UNKNOWN_ID: TypeId = TypeId(0xFFFF_FFFF);

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Type(Box<Type>),
    Function(Box<Function>),
    Property(Box<Property>),
    Method(Box<Method>),
    Implementation(Box<Implementation>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub idx: usize,
    pub name: String,
    pub ty: TypeRef,
    pub vararg: bool,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Parameters {
    pub params: Vec<Parameter>,
}

impl Parameters {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, name: String, ty: TypeRef, vararg: bool) {
        self.params.push(Parameter {
            idx: self.params.len(),
            name,
            ty,
            vararg,
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

    pub fn is_vararg(&self) -> bool {
        self.params.iter().any(|param| param.vararg)
    }
}

/// Defines the signature of a function or method, with parameters and return type.
///
/// While the type infers that it's only applicable for functions, this structure
/// is also used for methods.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSig<'a> {
    pub params: &'a Parameters,
    pub type_params: &'a [TypeParameterId],
    pub ret_ty: &'a TypeRef,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub id: FunctionId,
    pub hir: ItemId,
    pub name: Path,
    pub visibility: Visibility,
    pub type_parameters: Vec<TypeParameterId>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    pub id: PropertyId,
    pub visibility: Visibility,
    pub owner: TypeId,
    pub name: String,
    pub property_type: TypeRef,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    pub id: MethodId,
    pub hir: ItemId,
    pub visibility: Visibility,
    pub callee: TypeRef,
    pub name: Path,
    pub type_parameters: Vec<TypeParameterId>,
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

    // /// Determines whether the method is instanced, as opposed to static.
    pub fn is_instanced(&self) -> bool {
        if let Some(param) = self.parameters.params.first() {
            &param.name == "self" && param.ty == self.callee
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub id: ItemId,
    pub name: Path,
    pub type_parameters: Vec<TypeParameterId>,
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
    pub id: ItemId,
    pub name: Path,
    pub type_parameters: Vec<TypeParameterId>,
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
    pub name: Path,
}

impl Enum {
    pub fn new(reference: &lume_hir::EnumDefinition) -> Self {
        Self {
            name: reference.name.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumCase {
    pub parent: ItemId,
    pub name: Path,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Implementation {
    pub id: ImplId,
    pub target: Path,
    pub type_parameters: Vec<TypeParameterId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Use {
    pub id: UseId,
    pub trait_: TypeRef,
    pub target: TypeRef,
    pub type_parameters: Vec<TypeParameterId>,
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
    TypeParameter(TypeParameterId),
}

impl TypeKind {
    pub fn transport(&self) -> TypeTransport {
        match self {
            TypeKind::Void | TypeKind::Bool | TypeKind::Int(_) | TypeKind::UInt(_) | TypeKind::Float(_) => {
                TypeTransport::Copy
            }
            TypeKind::User(_) | TypeKind::String | TypeKind::TypeParameter(_) => TypeTransport::Reference,
        }
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeTransport {
    /// The type is fully copied when passed as an argument or returned from a function.
    Copy,

    /// The type uses the same memory location and is passed by reference.
    Reference,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub id: TypeId,
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

    /// Creates a new [`Type`] with an inner type of [`TypeKind::String`].
    pub fn string() -> Self {
        Self {
            id: TYPEREF_STRING_ID,
            kind: TypeKind::String,
            name: Path::string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeRef {
    pub instance_of: TypeId,
    pub type_arguments: Vec<TypeRef>,
    pub location: Location,
}

impl TypeRef {
    /// Creates a new [`TypeRef`] with the given instance.
    pub fn new(instance: TypeId, location: Location) -> Self {
        Self {
            instance_of: instance,
            type_arguments: vec![],
            location,
        }
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

    /// Creates a new [`Type`] with an inner type of [`TypeKind::String`].
    pub fn string() -> Self {
        Self {
            instance_of: TYPEREF_STRING_ID,
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

    /// Determines if the type is an `string`.
    pub fn is_string(&self) -> bool {
        self.instance_of == TYPEREF_STRING_ID
    }

    /// Determines if the type is unknown.
    pub fn is_unknown(&self) -> bool {
        self.instance_of == TYPEREF_UNKNOWN_ID
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

#[derive(Debug, Clone)]
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
    pub id: TypeParameterId,
    pub name: String,
    pub constraints: Vec<TypeRef>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeArgument {
    Named { name: String },
    Resolved { param: TypeParameterId },
}

#[derive(Debug)]
pub struct TypeDatabaseContext {
    pub types: Vec<Type>,
    pub properties: Vec<Property>,
    pub methods: Vec<Method>,
    pub functions: Vec<Function>,
    pub type_parameters: Vec<TypeParameter>,
    pub implementations: Vec<Implementation>,
    pub uses: Vec<Use>,
}

#[allow(clippy::cast_possible_truncation)]
impl TypeDatabaseContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Gets an iterator which iterates all [`Type`]-instances within
    /// the database context.
    pub fn types(&self) -> impl Iterator<Item = &Type> {
        self.types.iter()
    }

    /// Gets an iterator which iterates all [`Type`]-instances within
    /// the database context.
    pub fn types_mut(&mut self) -> impl Iterator<Item = &mut Type> {
        self.types.iter_mut()
    }

    /// Gets the [`Type`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Type`] is not found.
    pub fn type_(&self, id: TypeId) -> Option<&Type> {
        self.types.get(id.0 as usize)
    }

    /// Gets the [`Type`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Type`] is not found.
    pub fn type_mut(&mut self, id: TypeId) -> Option<&mut Type> {
        self.types.get_mut(id.0 as usize)
    }

    /// Expects the [`Type`] with the given ID, if it exists.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the type wasn't found.
    pub fn ty_expect(&self, id: TypeId) -> Result<&Type> {
        match self.type_(id) {
            Some(ty) => Ok(ty),
            None => Err(TypeNotFound { id }.into()),
        }
    }

    /// Expects the [`Type`] with the given ID to be a [`Trait`] kind.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the type wasn't found or if the found type did not
    /// have a [`TypeKindRef`] of [`TypeKindRef::Trait`].
    pub fn ty_expect_trait(&self, id: TypeId) -> Result<&Trait> {
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

    /// Gets an iterator which iterates all [`Property`]-instances within
    /// the database context.
    pub fn properties(&self) -> impl Iterator<Item = &Property> {
        self.properties.iter()
    }

    /// Gets an iterator which iterates all [`Property`]-instances within
    /// the database context.
    pub fn properties_mut(&mut self) -> impl Iterator<Item = &mut Property> {
        self.properties.iter_mut()
    }

    /// Gets the [`Property`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Property`] is not found.
    pub fn property(&self, id: PropertyId) -> Option<&Property> {
        self.properties.get(id.0 as usize)
    }

    /// Gets the [`Property`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Property`] is not found.
    pub fn property_mut(&mut self, id: PropertyId) -> Option<&mut Property> {
        self.properties.get_mut(id.0 as usize)
    }

    /// Gets an iterator which iterates all [`Method`]-instances within
    /// the database context.
    pub fn methods(&self) -> impl Iterator<Item = &Method> {
        self.methods.iter()
    }

    /// Gets an iterator which iterates all [`Method`]-instances within
    /// the database context.
    pub fn methods_mut(&mut self) -> impl Iterator<Item = &mut Method> {
        self.methods.iter_mut()
    }

    /// Gets an iterator which iterates all [`Use`]-instances within
    /// the database context.
    pub fn uses(&self) -> impl Iterator<Item = &Use> {
        self.uses.iter()
    }

    /// Gets an iterator which iterates all [`Use`]-instances within
    /// the database context.
    pub fn uses_mut(&mut self) -> impl Iterator<Item = &mut Use> {
        self.uses.iter_mut()
    }

    /// Gets the [`Use`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Use`] is not found.
    pub fn use_(&self, id: UseId) -> Option<&Use> {
        self.uses.get(id.0 as usize)
    }

    /// Gets the [`Use`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Use`] is not found.
    pub fn use_mut(&mut self, id: UseId) -> Option<&mut Use> {
        self.uses.get_mut(id.0 as usize)
    }

    /// Gets the [`Method`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Method`] is not found.
    pub fn method(&self, id: MethodId) -> Option<&Method> {
        self.methods.get(id.0 as usize)
    }

    /// Gets the [`Method`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Method`] is not found.
    pub fn method_mut(&mut self, id: MethodId) -> Option<&mut Method> {
        self.methods.get_mut(id.0 as usize)
    }

    /// Gets an iterator which iterates all [`Function`]-instances within
    /// the database context.
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.functions.iter()
    }

    /// Gets an iterator which iterates all [`Function`]-instances within
    /// the database context.
    pub fn functions_mut(&mut self) -> impl Iterator<Item = &mut Function> {
        self.functions.iter_mut()
    }

    /// Gets the [`Function`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Function`] is not found.
    pub fn function(&self, id: FunctionId) -> Option<&Function> {
        self.functions.get(id.0 as usize)
    }

    /// Gets the [`Function`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Function`] is not found.
    pub fn function_mut(&mut self, id: FunctionId) -> Option<&mut Function> {
        self.functions.get_mut(id.0 as usize)
    }

    /// Gets the [`TypeParameter`] with the given ID, if any.
    ///
    /// Returns `None` if the [`TypeParameter`] is not found.
    pub fn type_parameter(&self, id: TypeParameterId) -> Option<&TypeParameter> {
        self.type_parameters.get(id.0 as usize)
    }

    /// Gets the [`TypeParameter`] with the given ID, if any.
    ///
    /// Returns `None` if the [`TypeParameter`] is not found.
    pub fn type_parameter_mut(&mut self, id: TypeParameterId) -> Option<&mut TypeParameter> {
        self.type_parameters.get_mut(id.0 as usize)
    }

    /// Gets the [`Implementation`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Implementation`] is not found.
    pub fn implementation(&self, id: ImplId) -> Option<&Implementation> {
        self.implementations.get(id.0 as usize)
    }

    /// Gets the [`Implementation`] with the given ID, if any.
    ///
    /// Returns `None` if the [`Implementation`] is not found.
    pub fn implementation_mut(&mut self, id: ImplId) -> Option<&mut Implementation> {
        self.implementations.get_mut(id.0 as usize)
    }

    /// Gets an iterator which iterates all [`Item`]-instances where
    /// the item refers to a [`Method`], which are defined on the given [`Item`].
    pub fn methods_on(&self, id: TypeId) -> impl Iterator<Item = &Method> {
        self.methods().filter(move |m| m.callee.instance_of == id)
    }

    /// Gets an iterator which iterates all [`Item`]-instances where
    /// the item refers to a [`Use`], which are implementation on the given [`Item`].
    pub fn uses_on(&self, on: &TypeRef) -> impl Iterator<Item = &Use> {
        self.uses().filter(move |u| &u.target == on)
    }

    /// Attempts to find a [`Type`] with the given name, if any.
    pub fn find_type(&self, name: &Path) -> Option<&Type> {
        self.types().find(|ty| ty.name == *name)
    }

    /// Attempts to find a [`Function`] with the given name, if any.
    pub fn find_function(&self, name: &Path) -> Option<&Function> {
        self.functions().find(|func| func.name == *name)
    }

    /// Attempts to find all [`Property`]s on the given parent type.
    pub fn find_properties(&self, owner: TypeId) -> impl Iterator<Item = &Property> {
        self.properties().filter(move |prop| prop.owner == owner)
    }

    /// Attempts to find a [`Property`] with the given name on the given parent type, if any.
    pub fn find_property(&self, owner: TypeId, name: &String) -> Option<&Property> {
        self.properties().find(|prop| prop.owner == owner && prop.name == *name)
    }

    /// Attempts to find a [`Method`] with the given name, if any.
    pub fn find_method(&self, name: &Path) -> Option<&Method> {
        self.methods().find(|met| met.name == *name)
    }

    /// Allocates a new [`Function`] with the given name and kind.
    #[inline]
    pub fn func_alloc(&mut self, hir: ItemId, name: Path, visibility: Visibility) -> FunctionId {
        let id = FunctionId(self.functions.len() as u64);

        let func = Function {
            id,
            hir,
            name,
            visibility,
            type_parameters: Vec::new(),
            parameters: Parameters::new(),
            return_type: TypeRef::unknown(),
        };

        self.functions.push(func);
        id
    }

    /// Allocates a new [`Type`] with the given name and kind.
    #[inline]
    pub fn type_alloc(&mut self, name: Path, kind: TypeKind) -> TypeId {
        let id = TypeId(self.types.len() as u64);
        let ty = Type { id, kind, name };

        self.types.push(ty);
        id
    }

    /// Allocates a new [`Implementation`] with the target.
    #[inline]
    pub fn impl_alloc(&mut self, target: Path) -> ImplId {
        let id = ImplId(self.implementations.len() as u64);

        let implementation = Implementation {
            id,
            target,
            type_parameters: Vec::new(),
        };

        self.implementations.push(implementation);
        id
    }

    /// Allocates a new [`Use`] with the target.
    #[inline]
    pub fn use_alloc(&mut self) -> UseId {
        let id = UseId(self.uses.len() as u64);

        self.uses.push(Use {
            id,
            trait_: TypeRef::unknown(),
            target: TypeRef::unknown(),
            type_parameters: Vec::new(),
        });

        id
    }

    /// Allocates a new [`Property`] on the given [`Item`].
    ///
    /// # Errors
    ///
    /// Returns `Err` if `owner` refers to an [`Item`] which could not be found, or
    /// is not a type.
    #[inline]
    pub fn property_alloc(&mut self, owner: TypeId, name: String, visibility: Visibility) -> Result<PropertyId> {
        let id = PropertyId(self.properties.len() as u64);
        let prop = Property {
            id,
            owner,
            name,
            visibility,
            property_type: TypeRef::unknown(),
        };

        self.properties.push(prop);

        Ok(id)
    }

    /// Allocates a new [`Method`] on the given [`Item`].
    ///
    /// # Errors
    ///
    /// Returns `Err` if `owner` refers to an [`Item`] which could not be found, or
    /// is not a type.
    #[inline]
    pub fn method_alloc(
        &mut self,
        hir: ItemId,
        owner: TypeRef,
        name: Path,
        visibility: Visibility,
    ) -> Result<MethodId> {
        let id = MethodId(self.methods.len() as u64);

        let method = Method {
            id,
            hir,
            callee: owner,
            name,
            visibility,
            parameters: Parameters::new(),
            type_parameters: Vec::new(),
            return_type: TypeRef::unknown(),
        };

        self.methods.push(method);

        Ok(id)
    }

    /// Allocates a new [`TypeParameter`] with the given name and kind.
    #[inline]
    pub fn type_param_alloc(&mut self, name: String) -> TypeParameterId {
        let id = TypeParameterId(self.type_parameters.len() as u64);
        let param = TypeParameter {
            id,
            name,
            constraints: Vec::new(),
        };

        self.type_parameters.push(param);
        id
    }

    /// Gets the type parameters defined on the [`Type`] with the given ID.
    ///
    /// # Errors
    ///
    /// Returns `Err` if no type with the given ID was found in the context,
    /// or if the found type is non-generic (such as [`TypeKind::Void`] or [`TypeKind::TypeParameter`]).
    pub fn type_params_of(&self, id: impl WithTypeParameters) -> Result<&Vec<TypeParameterId>> {
        id.type_params(self)
    }

    /// Pushes a new type parameter to the [`Type`] with the given ID.
    ///
    /// # Errors
    ///
    /// Returns `Err` if no type with the given ID was found in the context,
    /// or if the found type is non-generic (such as [`TypeKind::Void`] or [`TypeKind::TypeParameter`]).
    pub fn push_type_param(&mut self, id: impl WithTypeParameters, type_id: TypeParameterId) -> Result<()> {
        id.type_params_mut(self)?.push(type_id);

        Ok(())
    }
}

impl Default for TypeDatabaseContext {
    fn default() -> Self {
        Self {
            types: vec![
                Type::void(),
                Type::bool(),
                Type::i8(),
                Type::i16(),
                Type::i32(),
                Type::i64(),
                Type::u8(),
                Type::u16(),
                Type::u32(),
                Type::u64(),
                Type::f32(),
                Type::f64(),
                Type::string(),
            ],
            properties: Vec::new(),
            methods: Vec::new(),
            functions: Vec::new(),
            type_parameters: Vec::new(),
            implementations: Vec::new(),
            uses: Vec::new(),
        }
    }
}

/// Central data structure for performing analysis and checking on types within a compilation
/// job. This structure contains references to all types defined within a source package,
/// as well as all resulting types from expressions, statements, etc.
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
