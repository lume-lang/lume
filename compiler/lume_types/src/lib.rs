use lume_diag::Result;

const UNKNOWN_TYPE_ID: TypeId = TypeId(u32::MAX);

#[derive(serde::Serialize, Debug, Clone, Eq)]
pub struct Identifier {
    pub name: String,
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

impl From<&'static str> for Identifier {
    fn from(name: &'static str) -> Self {
        Self { name: name.to_string() }
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Identifier) -> bool {
        self.name == other.name
    }
}

#[derive(serde::Serialize, Debug, Clone, Eq)]
pub struct IdentifierPath {
    pub path: Vec<Identifier>,
}

impl IdentifierPath {
    pub fn empty() -> Self {
        Self { path: Vec::new() }
    }
}

impl std::hash::Hash for IdentifierPath {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.path.hash(state);
    }
}

impl From<&[&'static str]> for IdentifierPath {
    fn from(path: &[&'static str]) -> Self {
        Self {
            path: path.iter().map(|name| Identifier::from(*name)).collect(),
        }
    }
}

impl PartialEq for IdentifierPath {
    fn eq(&self, other: &IdentifierPath) -> bool {
        self.path == other.path
    }
}

#[derive(serde::Serialize, Hash, Clone, PartialEq, Eq)]
pub struct SymbolName {
    /// Defines the namespace which the symbol was defined in.
    pub namespace: IdentifierPath,

    /// Defines the relative name of the symbol within it's namespace.
    pub name: Identifier,
}

impl SymbolName {
    pub fn from_parts(namespace: &[&'static str], name: &'static str) -> Self {
        let namespace = IdentifierPath::from(namespace);
        let name = Identifier::from(name);

        Self { namespace, name }
    }

    pub fn i8() -> Self {
        Self::from_parts(&["std"], "Int8")
    }

    pub fn u8() -> Self {
        Self::from_parts(&["std"], "UInt8")
    }

    pub fn i16() -> Self {
        Self::from_parts(&["std"], "Int8")
    }

    pub fn u16() -> Self {
        Self::from_parts(&["std"], "UInt16")
    }

    pub fn i32() -> Self {
        Self::from_parts(&["std"], "Int32")
    }

    pub fn u32() -> Self {
        Self::from_parts(&["std"], "UInt32")
    }

    pub fn i64() -> Self {
        Self::from_parts(&["std"], "Int64")
    }

    pub fn u64() -> Self {
        Self::from_parts(&["std"], "UInt64")
    }

    pub fn iptr() -> Self {
        Self::from_parts(&["std"], "IPtr")
    }

    pub fn uptr() -> Self {
        Self::from_parts(&["std"], "UPtr")
    }

    pub fn float() -> Self {
        Self::from_parts(&["std"], "Float")
    }

    pub fn double() -> Self {
        Self::from_parts(&["std"], "Double")
    }

    pub fn string() -> Self {
        Self::from_parts(&["std"], "String")
    }

    pub fn boolean() -> Self {
        Self::from_parts(&["std"], "Boolean")
    }
}

impl std::fmt::Debug for SymbolName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'")?;

        for m in &self.namespace.path {
            write!(f, "{}.", m)?;
        }

        write!(f, "{}'", self.name.name)
    }
}

#[derive(serde::Serialize, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Visibility {
    // Order matters here, since `Ord` and `PartialOrd` determines
    // the order of enums by the order of their variants!
    Private,
    Public,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Parameter {
    pub idx: usize,
    pub name: String,
    pub ty: TypeRef,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Parameters {
    pub params: Vec<Parameter>,
}

impl Parameters {
    pub fn new() -> Self {
        Self { params: Vec::new() }
    }

    pub fn push(&mut self, name: String, ty: TypeRef) {
        self.params.push(Parameter {
            idx: self.params.len(),
            name,
            ty,
        });
    }
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

    pub fn add_type_param(&self, ctx: &mut TypeDatabaseContext, name: String) -> TypeParameterId {
        let id = TypeParameter::alloc(ctx, name);
        self.get_mut(ctx).type_parameters.push(id);

        id
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Function {
    pub name: SymbolName,
    pub visibility: Visibility,
    pub type_parameters: Vec<TypeParameterId>,
    pub parameters: Parameters,
    pub return_type: TypeRef,
}

impl Function {
    pub fn alloc(ctx: &mut TypeDatabaseContext, name: SymbolName, visibility: Visibility) -> FunctionId {
        let id = ctx.functions.len();
        let function = Function {
            name,
            visibility,
            type_parameters: Vec::new(),
            parameters: Parameters::new(),
            return_type: TypeRef::unknown(),
        };

        ctx.functions.push(function);
        FunctionId(id as u32)
    }

    pub fn find(ctx: &TypeDatabaseContext, name: SymbolName) -> Option<&Function> {
        ctx.functions.iter().find(|f| f.name == name)
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
    pub visibility: Visibility,
    pub owner: TypeId,
    pub name: String,
    pub property_type: TypeRef,
}

impl Property {
    pub fn alloc(ctx: &mut TypeDatabaseContext, owner: TypeId, name: String, visibility: Visibility) -> PropertyId {
        let id = ctx.properties.len();
        let property = Property {
            visibility,
            owner,
            name,
            property_type: TypeRef::unknown(),
        };

        ctx.properties.push(property);
        PropertyId(id as u32)
    }
}

#[derive(serde::Serialize, Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct MethodId(pub u32);

impl MethodId {
    pub fn get<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a Method {
        &ctx.methods[self.0 as usize]
    }

    pub fn get_mut<'a>(&'a self, ctx: &'a mut TypeDatabaseContext) -> &'a mut Method {
        &mut ctx.methods[self.0 as usize]
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

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Method {
    pub visibility: Visibility,
    pub callee: TypeRef,
    pub name: String,
    pub type_parameters: Vec<TypeParameterId>,
    pub parameters: Parameters,
    pub return_type: TypeRef,
}

impl Method {
    pub fn alloc(ctx: &mut TypeDatabaseContext, class: TypeId, name: String, visibility: Visibility) -> MethodId {
        let id = ctx.methods.len();
        let method = Method {
            visibility,
            callee: TypeRef::new(class),
            name,
            type_parameters: Vec::new(),
            parameters: Parameters::new(),
            return_type: TypeRef::unknown(),
        };

        ctx.methods.push(method);
        MethodId(id as u32)
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
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

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
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

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Enum {
    pub name: SymbolName,
}

impl Enum {
    pub fn new(name: SymbolName) -> Self {
        Self { name }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Alias {
    pub name: SymbolName,
}

impl Alias {
    pub fn new(name: SymbolName) -> Self {
        Self { name }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
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
        match ctx.types.iter().position(|t| t.name == *name) {
            Some(index) => Some(TypeId(index as u32)),
            None => None,
        }
    }

    pub fn find_or_err(ctx: &TypeDatabaseContext, name: &SymbolName) -> TypeId {
        match Self::find(ctx, name) {
            Some(id) => id,
            None => panic!("no type of name {:?} was found", name),
        }
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

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Type {
    pub kind: TypeKind,
    pub transport: TypeTransport,
    pub name: SymbolName,
}

impl Type {
    pub fn alloc(ctx: &mut TypeDatabaseContext, name: SymbolName, kind: TypeKind) -> TypeId {
        let id = TypeId(ctx.types.len() as u32);
        let method = Type {
            kind,
            transport: TypeTransport::Reference,
            name,
        };

        ctx.types.push(method);
        id
    }

    pub fn reference_type(name: SymbolName) -> Self {
        Type {
            kind: TypeKind::Class(Box::new(Class::new(name.clone()))),
            transport: TypeTransport::Reference,
            name,
        }
    }

    pub fn value_type(name: SymbolName) -> Self {
        Type {
            kind: TypeKind::Class(Box::new(Class::new(name.clone()))),
            transport: TypeTransport::Copy,
            name,
        }
    }

    pub fn type_parameter(ctx: &mut TypeDatabaseContext, id: TypeParameterId, name: Identifier) -> TypeId {
        Type::alloc(
            ctx,
            SymbolName {
                namespace: IdentifierPath::empty(),
                name,
            },
            TypeKind::TypeParameter(id),
        )
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
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

    pub fn push_type_argument(&mut self, type_argument: TypeRef) {
        self.type_arguments.push(type_argument);
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

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct TypeParameter {
    pub name: String,
    pub constraints: Vec<TypeRef>,
}

impl TypeParameter {
    pub fn alloc(ctx: &mut TypeDatabaseContext, name: String) -> TypeParameterId {
        let id = ctx.type_parameters.len();
        let param = TypeParameter {
            name,
            constraints: Vec::new(),
        };

        ctx.type_parameters.push(param);
        TypeParameterId(id as u32)
    }
}

#[derive(serde::Serialize, Debug)]
pub struct TypeDatabaseContext {
    pub types: Vec<Type>,
    pub properties: Vec<Property>,
    pub methods: Vec<Method>,
    pub functions: Vec<Function>,
    pub type_parameters: Vec<TypeParameter>,
}

impl TypeDatabaseContext {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            properties: Vec::new(),
            methods: Vec::new(),
            functions: Vec::new(),
            type_parameters: Vec::new(),
        }
    }

    pub fn check_type_compatibility(&self, from: &TypeRef, to: &TypeRef) -> Result<()> {
        if from != to {
            // return Err(typech::errors::MismatchedTypes {
            //     range: from.range(),
            //     expected: from.clone(),
            //     found: to.clone(),
            // }
            // .into());
        }

        Ok(())
    }
}
