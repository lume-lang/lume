use error_snippet::Result;
use indexmap::IndexMap;

use crate::errors::*;
use lume_hir::{FunctionId, ImplId, MethodId, PropertyId, SymbolName, TypeId, TypeParameterId, Visibility};
use lume_span::ItemId;

mod errors;

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

impl WithTypeParameters for TypeId {
    fn type_params(self, tcx: &TypeDatabaseContext) -> Result<&Vec<TypeParameterId>> {
        let Some(ty) = tcx.type_(self) else {
            return Err(TypeNotFound { id: self }.into());
        };

        match &ty.kind {
            TypeKind::Struct(k) => Ok(&k.type_parameters),
            TypeKind::Trait(k) => Ok(&k.type_parameters),
            kind => Err(TypeParametersOnNonGenericType { ty: kind.clone() }.into()),
        }
    }

    fn type_params_mut(self, tcx: &mut TypeDatabaseContext) -> Result<&mut Vec<TypeParameterId>> {
        let Some(ty) = tcx.type_mut(self) else {
            return Err(TypeNotFound { id: self }.into());
        };

        match &mut ty.kind {
            TypeKind::Struct(k) => Ok(&mut k.type_parameters),
            TypeKind::Trait(k) => Ok(&mut k.type_parameters),
            kind => Err(TypeParametersOnNonGenericType { ty: kind.clone() }.into()),
        }
    }
}

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
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Parameters {
    pub params: Vec<Parameter>,
}

impl Parameters {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, name: String, ty: TypeRef) {
        self.params.push(Parameter {
            idx: self.params.len(),
            name,
            ty,
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
    pub name: SymbolName,
    pub visibility: Visibility,
    pub type_parameters: Vec<TypeParameterId>,
    pub parameters: Parameters,
    pub return_type: TypeRef,
}

impl Function {
    /// Gets the signature of the function.
    pub fn sig(&self) -> FunctionSig {
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
    pub visibility: Visibility,
    pub callee: TypeRef,
    pub name: SymbolName,
    pub type_parameters: Vec<TypeParameterId>,
    pub parameters: Parameters,
    pub return_type: TypeRef,
}

impl Method {
    /// Gets the signature of the method.
    pub fn sig(&self) -> FunctionSig {
        FunctionSig {
            params: &self.parameters,
            type_params: &self.type_parameters,
            ret_ty: &self.return_type,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub id: ItemId,
    pub name: SymbolName,
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
    pub name: SymbolName,
    pub type_parameters: Vec<TypeParameterId>,
}

impl Trait {
    pub fn new(reference: &lume_hir::TraitDefinition) -> Self {
        Self {
            name: reference.name.clone(),
            type_parameters: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub name: SymbolName,
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
    pub name: SymbolName,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Alias {
    pub name: SymbolName,
}

impl Alias {
    pub fn new(reference: &lume_hir::AliasDefinition) -> Self {
        Self {
            name: reference.name.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Implementation {
    pub id: ImplId,
    pub target: SymbolName,
    pub type_parameters: Vec<TypeParameterId>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    /// The type is a regular user-defined struct.
    Struct(Box<Struct>),

    /// The type is a regular user-defined trait.
    Trait(Box<Trait>),

    /// The type is a regular user-defined enumeration.
    Enum(Box<Enum>),

    /// The type is an alias to some other type.
    Alias(Box<Alias>),

    /// The type is a reference to a type parameter in the current scope.
    TypeParameter(TypeParameterId),

    /// Represents a non-value.
    Void,
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
    pub transport: TypeTransport,
    pub name: SymbolName,

    pub properties: IndexMap<String, PropertyId>,
    pub methods: IndexMap<String, MethodId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeRef {
    pub instance_of: TypeId,
    pub type_arguments: Vec<TypeRef>,
}

impl TypeRef {
    /// Creates a new [`TypeRef`] with the given instance.
    pub fn new(instance: TypeId) -> Self {
        Self {
            instance_of: instance,
            type_arguments: vec![],
        }
    }

    /// Creates a new [`TypeRef`] with an invalid inner type, meant to
    /// be used before any types are actually resolved.
    pub const fn unknown() -> Self {
        Self {
            instance_of: TYPEREF_UNKNOWN_ID,
            type_arguments: vec![],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParameter {
    pub id: TypeParameterId,
    pub name: String,
    pub constraints: Vec<TypeRef>,
}

#[derive(Default, Debug)]
pub struct TypeDatabaseContext {
    pub types: Vec<Type>,
    pub properties: Vec<Property>,
    pub methods: Vec<Method>,
    pub functions: Vec<Function>,
    pub type_parameters: Vec<TypeParameter>,
    pub implementations: Vec<Implementation>,
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
    /// the item refers to a [`Method`], which are defined on the givem [`Item`].
    pub fn methods_on(&self, id: TypeId) -> impl Iterator<Item = &Method> {
        self.methods().filter(move |m| m.callee.instance_of == id)
    }

    /// Attempts to find a [`Type`] with the given name, if any.
    pub fn find_type(&self, name: &SymbolName) -> Option<&Type> {
        self.types().find(|ty| ty.name == *name)
    }

    /// Attempts to find a [`Function`] with the given name, if any.
    pub fn find_function(&self, name: &SymbolName) -> Option<&Function> {
        self.functions().find(|func| func.name == *name)
    }

    /// Attempts to find a [`Property`] with the given name on the given parent type, if any.
    pub fn find_property(&self, owner: TypeId, name: &String) -> Option<&Property> {
        self.properties().find(|prop| prop.owner == owner && prop.name == *name)
    }

    /// Attempts to find a [`Method`] with the given name, if any.
    pub fn find_method(&self, name: &SymbolName) -> Option<&Method> {
        self.methods().find(|met| met.name == *name)
    }

    /// Allocates a new [`Function`] with the given name and kind.
    #[inline]
    pub fn func_alloc(&mut self, name: SymbolName, visibility: Visibility) -> FunctionId {
        let id = FunctionId(self.functions.len() as u64);

        let func = Function {
            id,
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
    pub fn type_alloc(&mut self, name: SymbolName, kind: TypeKind) -> TypeId {
        let id = TypeId(self.types.len() as u64);

        let ty = Type {
            id,
            kind,
            name,
            transport: TypeTransport::Reference,
            properties: IndexMap::new(),
            methods: IndexMap::new(),
        };

        self.types.push(ty);
        id
    }

    /// Allocates a new [`Implementation`] with the target.
    #[inline]
    pub fn impl_alloc(&mut self, target: SymbolName) -> ImplId {
        let id = ImplId(self.implementations.len() as u64);

        let implementation = Implementation {
            id,
            target,
            type_parameters: Vec::new(),
        };

        self.implementations.push(implementation);
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
            name: name.clone(),
            visibility,
            property_type: TypeRef::unknown(),
        };

        self.properties.push(prop);

        match self.type_mut(owner) {
            Some(ty) => ty.properties.insert(name, id),
            None => return Err(TypeNotFound { id: owner }.into()),
        };

        Ok(id)
    }

    /// Allocates a new [`Method`] on the given [`Item`].
    ///
    /// # Errors
    ///
    /// Returns `Err` if `owner` refers to an [`Item`] which could not be found, or
    /// is not a type.
    #[inline]
    pub fn method_alloc(&mut self, owner: TypeRef, name: SymbolName, visibility: Visibility) -> Result<MethodId> {
        let id = MethodId(self.methods.len() as u64);
        let instance = owner.instance_of;

        let method = Method {
            id,
            callee: owner,
            name: name.clone(),
            visibility,
            parameters: Parameters::new(),
            type_parameters: Vec::new(),
            return_type: TypeRef::unknown(),
        };

        self.methods.push(method);

        match self.type_mut(instance) {
            Some(ty) => ty.methods.insert(name.name.name, id),
            None => return Err(TypeNotFound { id: instance }.into()),
        };

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
