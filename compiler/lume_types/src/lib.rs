use error_snippet::Result;
use indexmap::IndexMap;
use lume_span::Location;

const UNKNOWN_TYPE_ID: TypeId = TypeId(u32::MAX);

#[derive(Debug, Clone, Eq)]
pub struct Identifier {
    pub name: String,
    pub location: Location,
}

impl std::hash::Hash for Identifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

impl<T: Into<String>> From<T> for Identifier {
    fn from(name: T) -> Self {
        Self {
            name: name.into(),
            location: Location::empty(),
        }
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Identifier) -> bool {
        self.name == other.name
    }
}

#[derive(Debug, Clone, Eq)]
pub struct NamespacePath {
    pub path: Vec<Identifier>,
    pub location: Location,
}

impl NamespacePath {
    pub fn empty() -> Self {
        Self {
            path: Vec::new(),
            location: Location::empty(),
        }
    }
}

impl std::hash::Hash for NamespacePath {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.path.hash(state);
    }
}

impl<T: IntoIterator<Item = impl Into<String>>> From<T> for NamespacePath {
    fn from(path: T) -> Self {
        Self {
            path: path
                .into_iter()
                .map(|name| Identifier::from(Into::<String>::into(name)))
                .collect(),
            location: Location::empty(),
        }
    }
}

impl PartialEq for NamespacePath {
    fn eq(&self, other: &NamespacePath) -> bool {
        self.path == other.path
    }
}

#[derive(Clone, Eq)]
pub struct SymbolName {
    /// Defines the namespace which the symbol was defined in.
    pub namespace: NamespacePath,

    /// Defines the relative name of the symbol within it's namespace.
    pub name: Identifier,

    pub location: Location,
}

impl PartialEq for SymbolName {
    fn eq(&self, other: &SymbolName) -> bool {
        self.namespace == other.namespace && self.name == other.name
    }
}

impl std::hash::Hash for SymbolName {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.namespace.hash(state);
        self.name.hash(state);
    }
}

impl SymbolName {
    pub fn from_parts(namespace: impl IntoIterator<Item = impl Into<String>>, name: impl Into<String>) -> Self {
        let namespace = NamespacePath::from(namespace);
        let name = Identifier::from(name);

        Self {
            namespace,
            name,
            location: Location::empty(),
        }
    }

    pub fn with_root(base: SymbolName, name: Identifier) -> Self {
        let mut namespace = base.namespace.clone();
        namespace.path.push(base.name);

        Self {
            namespace,
            name,
            location: base.location.clone(),
        }
    }

    pub fn i8() -> Self {
        Self::from_parts(["std"], "Int8")
    }

    pub fn u8() -> Self {
        Self::from_parts(["std"], "UInt8")
    }

    pub fn i16() -> Self {
        Self::from_parts(["std"], "Int8")
    }

    pub fn u16() -> Self {
        Self::from_parts(["std"], "UInt16")
    }

    pub fn i32() -> Self {
        Self::from_parts(["std"], "Int32")
    }

    pub fn u32() -> Self {
        Self::from_parts(["std"], "UInt32")
    }

    pub fn i64() -> Self {
        Self::from_parts(["std"], "Int64")
    }

    pub fn u64() -> Self {
        Self::from_parts(["std"], "UInt64")
    }

    pub fn iptr() -> Self {
        Self::from_parts(["std"], "IPtr")
    }

    pub fn uptr() -> Self {
        Self::from_parts(["std"], "UPtr")
    }

    pub fn float() -> Self {
        Self::from_parts(["std"], "Float")
    }

    pub fn double() -> Self {
        Self::from_parts(["std"], "Double")
    }

    pub fn string() -> Self {
        Self::from_parts(["std"], "String")
    }

    pub fn boolean() -> Self {
        Self::from_parts(["std"], "Boolean")
    }
}

impl std::fmt::Display for SymbolName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.name)
    }
}

impl std::fmt::Debug for SymbolName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for m in &self.namespace.path {
            write!(f, "{}::", m)?;
        }

        write!(f, "{}", self.name.name)
    }
}

#[derive(serde::Serialize, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Visibility {
    // Order matters here, since `Ord` and `PartialOrd` determines
    // the order of enums by the order of their variants!
    Private,
    Public,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub idx: usize,
    pub name: String,
    pub ty: TypeRef,
}

#[derive(serde::Serialize, Default, Debug, Clone, PartialEq, Eq)]
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
#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
pub struct FunctionSig<'a> {
    pub params: &'a Parameters,
    pub type_params: &'a [TypeParameterId],
    pub ret_ty: &'a TypeRef,
}

#[derive(serde::Serialize, Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunctionId(pub u32);

impl FunctionId {
    pub fn get<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a Function {
        &ctx.functions[self.0 as usize]
    }

    pub fn get_mut<'a>(&'a self, ctx: &'a mut TypeDatabaseContext) -> &'a mut Function {
        &mut ctx.functions[self.0 as usize]
    }

    pub fn find(ctx: &TypeDatabaseContext, name: &SymbolName) -> Option<FunctionId> {
        ctx.functions
            .iter()
            .position(|f| f.name == *name)
            .map(|idx| FunctionId(idx as u32))
    }

    pub fn is_private(self, ctx: &TypeDatabaseContext) -> bool {
        self.get(ctx).visibility == Visibility::Private
    }

    pub fn is_public(self, ctx: &TypeDatabaseContext) -> bool {
        self.get(ctx).visibility == Visibility::Public
    }

    pub fn return_type<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a TypeRef {
        &self.get(ctx).return_type
    }

    pub fn set_return_type(&self, ctx: &mut TypeDatabaseContext, ty: TypeRef) {
        self.get_mut(ctx).return_type = ty;
    }

    pub fn add_parameter(&self, ctx: &mut TypeDatabaseContext, name: String, ty: TypeRef) {
        self.get_mut(ctx).parameters.push(name, ty)
    }

    pub fn type_params<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a Vec<TypeParameterId> {
        &self.get(ctx).type_parameters
    }

    pub fn add_type_param(&self, ctx: &mut TypeDatabaseContext, name: String) -> TypeParameterId {
        let id = TypeParameter::alloc(ctx, name);
        self.get_mut(ctx).type_parameters.push(id);

        id
    }
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
    pub fn alloc(ctx: &mut TypeDatabaseContext, name: SymbolName, visibility: Visibility) -> FunctionId {
        let id = FunctionId(ctx.functions.len() as u32);
        let function = Function {
            id,
            name,
            visibility,
            type_parameters: Vec::new(),
            parameters: Parameters::new(),
            return_type: TypeRef::unknown(),
        };

        ctx.functions.push(function);
        id
    }

    pub fn find<'a>(ctx: &'a TypeDatabaseContext, name: &SymbolName) -> Option<&'a Function> {
        ctx.functions.iter().find(|f| &f.name == name)
    }

    /// Gets the signature of the function.
    pub fn sig<'a>(&'a self) -> FunctionSig<'a> {
        FunctionSig {
            params: &self.parameters,
            type_params: &self.type_parameters,
            ret_ty: &self.return_type,
        }
    }
}

#[derive(serde::Serialize, Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct PropertyId(pub u32);

impl PropertyId {
    pub fn get<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a Property {
        &ctx.properties[self.0 as usize]
    }

    pub fn get_mut<'a>(&'a self, ctx: &'a mut TypeDatabaseContext) -> &'a mut Property {
        &mut ctx.properties[self.0 as usize]
    }

    pub fn is_private(self, ctx: &TypeDatabaseContext) -> bool {
        self.get(ctx).visibility == Visibility::Private
    }

    pub fn is_public(self, ctx: &TypeDatabaseContext) -> bool {
        self.get(ctx).visibility == Visibility::Public
    }

    pub fn property_type<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a TypeRef {
        &self.get(ctx).property_type
    }

    pub fn set_property_type(&self, ctx: &mut TypeDatabaseContext, ty: TypeRef) {
        self.get_mut(ctx).property_type = ty;
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Property {
    pub id: PropertyId,
    pub visibility: Visibility,
    pub owner: TypeId,
    pub name: String,
    pub property_type: TypeRef,
}

impl Property {
    pub fn alloc(ctx: &mut TypeDatabaseContext, owner: TypeId, name: String, visibility: Visibility) -> PropertyId {
        let id = PropertyId(ctx.properties.len() as u32);
        let property = Property {
            id,
            visibility,
            owner,
            name,
            property_type: TypeRef::unknown(),
        };

        ctx.properties.push(property);
        id
    }
}

#[derive(serde::Serialize, Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct MethodId(pub u32);

impl MethodId {
    pub fn get(self, ctx: &TypeDatabaseContext) -> &Method {
        &ctx.methods[self.0 as usize]
    }

    pub fn get_mut(self, ctx: &mut TypeDatabaseContext) -> &mut Method {
        &mut ctx.methods[self.0 as usize]
    }

    pub fn find(ctx: &TypeDatabaseContext, name: &SymbolName) -> Option<MethodId> {
        ctx.methods
            .iter()
            .position(|f| f.name == *name)
            .map(|idx| MethodId(idx as u32))
    }

    pub fn is_private(&self, ctx: &TypeDatabaseContext) -> bool {
        self.get(ctx).visibility == Visibility::Private
    }

    pub fn is_public(&self, ctx: &TypeDatabaseContext) -> bool {
        self.get(ctx).visibility == Visibility::Public
    }

    pub fn return_type<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a TypeRef {
        &self.get(ctx).return_type
    }

    pub fn set_return_type(&self, ctx: &mut TypeDatabaseContext, ty: TypeRef) {
        self.get_mut(ctx).return_type = ty;
    }

    pub fn add_parameter(&self, ctx: &mut TypeDatabaseContext, name: String, ty: TypeRef) {
        self.get_mut(ctx).parameters.push(name, ty)
    }

    pub fn type_params<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a Vec<TypeParameterId> {
        &self.get(ctx).type_parameters
    }

    pub fn add_type_param(&self, ctx: &mut TypeDatabaseContext, name: String) -> TypeParameterId {
        let id = TypeParameter::alloc(ctx, name);
        self.get_mut(ctx).type_parameters.push(id);

        id
    }
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
    pub fn alloc(ctx: &mut TypeDatabaseContext, class: TypeId, name: Identifier, visibility: Visibility) -> MethodId {
        let id = MethodId(ctx.methods.len() as u32);
        let qualified_name = SymbolName::with_root(class.name(ctx), name);

        let method = Method {
            id,
            visibility,
            callee: TypeRef::new(class),
            name: qualified_name,
            type_parameters: Vec::new(),
            parameters: Parameters::new(),
            return_type: TypeRef::unknown(),
        };

        ctx.methods.push(method);
        id
    }

    /// Gets the signature of the method.
    pub fn sig<'a>(&'a self) -> FunctionSig<'a> {
        FunctionSig {
            params: &self.parameters,
            type_params: &self.type_parameters,
            ret_ty: &self.return_type,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: SymbolName,
    pub type_parameters: Vec<TypeParameterId>,
}

impl Class {
    pub fn new(name: SymbolName) -> Self {
        Self {
            name,
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
    pub fn new(name: SymbolName) -> Self {
        Self {
            name,
            type_parameters: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub name: SymbolName,
}

impl Enum {
    pub fn new(name: SymbolName) -> Self {
        Self { name }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Alias {
    pub name: SymbolName,
}

impl Alias {
    pub fn new(name: SymbolName) -> Self {
        Self { name }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    /// The type is a regular user-defined class.
    Class(Box<Class>),

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

#[derive(serde::Serialize, Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeTransport {
    /// The type is fully copied when passed as an argument or returned from a function.
    Copy,

    /// The type uses the same memory location and is passed by reference.
    Reference,
}

#[derive(serde::Serialize, Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeId(pub u32);

impl TypeId {
    pub fn get<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a Type {
        &ctx.types[self.0 as usize]
    }

    pub fn get_mut<'a>(&'a self, ctx: &'a mut TypeDatabaseContext) -> &'a mut Type {
        &mut ctx.types[self.0 as usize]
    }

    pub fn find(ctx: &TypeDatabaseContext, name: &SymbolName) -> Option<TypeId> {
        // We intentionally ignore type parameters when finding types by name,
        // since they might not refer to a type parameter which is valid under the
        // current scope. Type parameters are only added as a type so they can be
        // referenced by other types.
        ctx.types
            .iter()
            .position(|t| t.name == *name && !matches!(t.kind, TypeKind::TypeParameter(_)))
            .map(|idx| TypeId(idx as u32))
    }

    pub fn find_or_err(ctx: &TypeDatabaseContext, name: &SymbolName) -> TypeId {
        match Self::find(ctx, name) {
            Some(id) => id,
            None => panic!("no type of name {:?} was found", name),
        }
    }

    pub fn name(&self, ctx: &TypeDatabaseContext) -> SymbolName {
        self.get(ctx).name.clone()
    }

    pub fn transport(&self, ctx: &TypeDatabaseContext) -> TypeTransport {
        self.get(ctx).transport
    }

    pub fn is_copied(&self, ctx: &TypeDatabaseContext) -> bool {
        self.get(ctx).transport == TypeTransport::Copy
    }

    pub fn is_referenced(&self, ctx: &TypeDatabaseContext) -> bool {
        self.get(ctx).transport == TypeTransport::Reference
    }

    pub fn set_transport(&self, ctx: &mut TypeDatabaseContext, transport: TypeTransport) {
        self.get_mut(ctx).transport = transport;
    }

    pub fn set_copied(&self, ctx: &mut TypeDatabaseContext) {
        self.set_transport(ctx, TypeTransport::Copy);
    }

    pub fn set_referenced(&self, ctx: &mut TypeDatabaseContext) {
        self.set_transport(ctx, TypeTransport::Reference);
    }

    pub fn kind<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a TypeKind {
        &self.get(ctx).kind
    }

    pub fn is_class(&self, ctx: &TypeDatabaseContext) -> bool {
        matches!(self.kind(ctx), TypeKind::Class(_))
    }

    pub fn is_trait(&self, ctx: &TypeDatabaseContext) -> bool {
        matches!(self.kind(ctx), TypeKind::Trait(_))
    }

    pub fn is_enum(&self, ctx: &TypeDatabaseContext) -> bool {
        matches!(self.kind(ctx), TypeKind::Enum(_))
    }

    pub fn is_void(&self, ctx: &TypeDatabaseContext) -> bool {
        matches!(self.kind(ctx), TypeKind::Void)
    }

    pub fn type_params<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a Vec<TypeParameterId> {
        match self.get(ctx) {
            Type {
                kind: TypeKind::Class(class),
                ..
            } => &class.type_parameters,
            Type {
                kind: TypeKind::Trait(trait_def),
                ..
            } => &trait_def.type_parameters,
            _ => {
                panic!("Cannot add type parameter to non-generic type");
            }
        }
    }

    pub fn add_type_param(&self, ctx: &mut TypeDatabaseContext, name: String) -> TypeParameterId {
        let id = TypeParameter::alloc(ctx, name);

        match self.get_mut(ctx) {
            Type {
                kind: TypeKind::Class(class),
                ..
            } => {
                class.type_parameters.push(id);
            }
            Type {
                kind: TypeKind::Trait(trait_def),
                ..
            } => {
                trait_def.type_parameters.push(id);
            }
            _ => {
                panic!("Cannot add type parameter to non-generic type");
            }
        };

        id
    }
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

impl Type {
    pub fn alloc(ctx: &mut TypeDatabaseContext, name: SymbolName, kind: TypeKind) -> TypeId {
        let id = TypeId(ctx.types.len() as u32);
        let method = Type {
            id,
            kind,
            transport: TypeTransport::Reference,
            name,
            properties: IndexMap::new(),
            methods: IndexMap::new(),
        };

        ctx.types.push(method);
        id
    }

    pub fn type_parameter(ctx: &mut TypeDatabaseContext, id: TypeParameterId, name: Identifier) -> TypeId {
        Type::alloc(
            ctx,
            SymbolName {
                namespace: NamespacePath::empty(),
                location: name.location.clone(),
                name,
            },
            TypeKind::TypeParameter(id),
        )
    }

    pub fn find(ctx: &TypeDatabaseContext, predicate: impl FnMut(&&Type) -> bool) -> Option<&Type> {
        ctx.types.iter().find(predicate)
    }

    pub fn find_type_param(ctx: &TypeDatabaseContext, type_param: TypeParameterId) -> Option<&Type> {
        Self::find(ctx, |ty| ty.kind == TypeKind::TypeParameter(type_param))
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TypeRef {
    instance_of: TypeId,
    type_arguments: Vec<TypeRef>,
}

impl TypeRef {
    pub fn new(instance_of: TypeId) -> Self {
        Self {
            instance_of,
            type_arguments: Vec::new(),
        }
    }

    pub fn instance_of(&self) -> TypeId {
        self.instance_of
    }

    pub fn get<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a Type {
        self.instance_of.get(ctx)
    }

    pub fn push_type_argument(&mut self, type_argument: TypeRef) {
        self.type_arguments.push(type_argument);
    }

    pub fn name(&self, ctx: &TypeDatabaseContext) -> SymbolName {
        self.get(ctx).name.clone()
    }

    pub fn property(&self, ctx: &TypeDatabaseContext, name: &String) -> Option<PropertyId> {
        self.get(ctx).properties.get(name).cloned()
    }

    pub fn properties(&self, ctx: &TypeDatabaseContext) -> Vec<PropertyId> {
        self.get(ctx).properties.values().cloned().collect()
    }

    pub fn methods(&self, ctx: &TypeDatabaseContext) -> Vec<MethodId> {
        self.get(ctx).methods.values().cloned().collect()
    }

    pub fn unknown() -> Self {
        Self::new(UNKNOWN_TYPE_ID)
    }
}

#[derive(serde::Serialize, Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct TypeParameterId(pub u32);

impl TypeParameterId {
    pub fn get<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a TypeParameter {
        &ctx.type_parameters[self.0 as usize]
    }

    pub fn get_mut<'a>(&'a self, ctx: &'a mut TypeDatabaseContext) -> &'a mut TypeParameter {
        &mut ctx.type_parameters[self.0 as usize]
    }

    pub fn add_constraint(&self, ctx: &mut TypeDatabaseContext, constraint: TypeRef) {
        self.get_mut(ctx).constraints.push(constraint);
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TypeParameter {
    pub id: TypeParameterId,
    pub name: String,
    pub constraints: Vec<TypeRef>,
}

impl TypeParameter {
    pub fn alloc(ctx: &mut TypeDatabaseContext, name: String) -> TypeParameterId {
        let id = TypeParameterId(ctx.type_parameters.len() as u32);
        let param = TypeParameter {
            id,
            name,
            constraints: Vec::new(),
        };

        ctx.type_parameters.push(param);
        id
    }
}

#[derive(Default, Debug)]
pub struct TypeDatabaseContext {
    pub types: Vec<Type>,
    pub properties: Vec<Property>,
    pub methods: Vec<Method>,
    pub functions: Vec<Function>,
    pub type_parameters: Vec<TypeParameter>,
}

impl TypeDatabaseContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn check_type_compatibility(&self, _from: &TypeRef, _to: &TypeRef) -> Result<()> {
        Ok(())
    }
}
