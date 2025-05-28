use lume_macros::Node;
use lume_span::{ExpressionId, ItemId, Location, PackageId, StatementId};

pub mod map;
pub mod symbols;

pub const SELF_TYPE_NAME: &str = "self";

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunctionId(pub u64);

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct TypeId(pub u64);

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct ImplId(pub u64);

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct UseId(pub u64);

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct PropertyId(pub u64);

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct MethodId(pub u64);

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct TypeParameterId(pub u64);

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
pub struct SymbolName {
    /// Defines the namespace which the symbol was defined in.
    pub namespace: Option<PathRoot>,

    /// Defines the relative name of the symbol within it's namespace.
    pub name: PathSegment,

    pub location: Location,
}

impl SymbolName {
    pub fn as_ident(&self) -> &Identifier {
        self.name.identifier()
    }

    pub fn as_str(&self) -> &str {
        &self.as_ident().name
    }

    /// Gets the parent symbol, which contains the current symbol instance.
    ///
    /// For example, given a [`SymbolName`] of `std::io::File::open()`, returns
    /// `Some(std::io::File)`. If no namespace is defined, returns `None`.
    pub fn parent(self) -> Option<Self> {
        if let Some(root) = self.namespace {
            let (name, root) = root.segments.split_last()?;

            Some(Self {
                name: name.to_owned(),
                namespace: Some(PathRoot {
                    segments: root.to_vec(),
                }),
                location: self.location,
            })
        } else {
            None
        }
    }

    /// Determines whether the roots (or namespaces) of the two
    /// given symbol names are equal.
    pub fn roots_eq(&self, other: &SymbolName) -> bool {
        match (&self.namespace, &other.namespace) {
            (Some(s), Some(o)) => s == o,
            (None, Some(o)) => o.segments.is_empty(),
            (Some(s), None) => s.segments.is_empty(),
            (None, None) => true,
        }
    }
}

impl PartialEq for SymbolName {
    fn eq(&self, other: &SymbolName) -> bool {
        self.name == other.name && self.roots_eq(other)
    }
}

impl std::hash::Hash for SymbolName {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.namespace.hash(state);
        self.name.hash(state);
    }
}

impl SymbolName {
    pub fn rooted(name: impl Into<String>) -> Self {
        let name = PathSegment::from(name.into());

        Self {
            namespace: None,
            name,
            location: Location::empty(),
        }
    }

    pub fn from_parts(
        namespace: Option<impl IntoIterator<Item = impl Into<PathSegment>>>,
        name: impl Into<String>,
    ) -> Self {
        let namespace = namespace.map(|ns| PathRoot::from(ns));
        let name = PathSegment::from(name.into());

        Self {
            namespace,
            name,
            location: Location::empty(),
        }
    }

    pub fn with_root(base: SymbolName, name: PathSegment) -> Self {
        let mut namespace = base.namespace.unwrap_or_default();
        namespace.segments.push(base.name);

        Self {
            namespace: Some(namespace),
            name,
            location: base.location.clone(),
        }
    }

    pub fn void() -> Self {
        Self::rooted("Void")
    }

    pub fn i8() -> Self {
        Self::from_parts(Some(["std"]), "Int8")
    }

    pub fn u8() -> Self {
        Self::from_parts(Some(["std"]), "UInt8")
    }

    pub fn i16() -> Self {
        Self::from_parts(Some(["std"]), "Int8")
    }

    pub fn u16() -> Self {
        Self::from_parts(Some(["std"]), "UInt16")
    }

    pub fn i32() -> Self {
        Self::from_parts(Some(["std"]), "Int32")
    }

    pub fn u32() -> Self {
        Self::from_parts(Some(["std"]), "UInt32")
    }

    pub fn i64() -> Self {
        Self::from_parts(Some(["std"]), "Int64")
    }

    pub fn u64() -> Self {
        Self::from_parts(Some(["std"]), "UInt64")
    }

    pub fn iptr() -> Self {
        Self::from_parts(Some(["std"]), "IPtr")
    }

    pub fn uptr() -> Self {
        Self::from_parts(Some(["std"]), "UPtr")
    }

    pub fn float() -> Self {
        Self::from_parts(Some(["std"]), "Float")
    }

    pub fn double() -> Self {
        Self::from_parts(Some(["std"]), "Double")
    }

    pub fn string() -> Self {
        Self::from_parts(Some(["std"]), "String")
    }

    pub fn boolean() -> Self {
        Self::from_parts(Some(["std"]), "Boolean")
    }
}

impl std::fmt::Display for SymbolName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ns) = &self.namespace {
            write!(f, "{ns}::")?;
        }

        write!(f, "{}", self.name)
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum PathSegment {
    Named(Identifier),
    Typed(Identifier, Vec<Type>),
}

impl PathSegment {
    #[inline]
    pub fn identifier(&self) -> &Identifier {
        match self {
            Self::Named(i) | Self::Typed(i, _) => i,
        }
    }

    #[inline]
    pub fn location(&self) -> &Location {
        &self.identifier().location
    }
}

impl From<Identifier> for PathSegment {
    fn from(value: Identifier) -> Self {
        Self::Named(value)
    }
}

impl From<String> for PathSegment {
    fn from(value: String) -> Self {
        Self::Named(Identifier::from(value))
    }
}

impl From<&str> for PathSegment {
    fn from(value: &str) -> Self {
        Self::Named(Identifier::from(value))
    }
}

impl std::fmt::Display for PathSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.identifier())
    }
}

#[derive(Hash, Default, Debug, Clone, PartialEq, Eq)]
pub struct PathRoot {
    pub segments: Vec<PathSegment>,
}

impl std::fmt::Display for PathRoot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (idx, segment) in self.segments.iter().enumerate() {
            write!(f, "{segment}")?;

            if idx < self.segments.len() - 1 {
                write!(f, "::")?;
            }
        }

        Ok(())
    }
}

impl<T: IntoIterator<Item = impl Into<PathSegment>>> From<T> for PathRoot {
    fn from(value: T) -> Self {
        let segments = value
            .into_iter()
            .map(Into::<PathSegment>::into)
            .collect::<Vec<PathSegment>>();

        Self { segments }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
    pub root: Vec<PathSegment>,
    pub name: PathSegment,
}

impl Path {
    pub fn to_pathroot(self) -> PathRoot {
        let mut segments = self.root;
        segments.push(self.name);

        PathRoot { segments }
    }
}

/// Trait for HIR nodes which can contain some amount of type parameters.
pub trait WithTypeParameters {
    /// Gets all the type parameters of this node.
    fn type_params(&self) -> &Vec<TypeParameter>;
}

/// Trait for HIR nodes which have some location attached.
pub trait Node {
    fn location(&self) -> &Location;
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub location: Location,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub enum Symbol {
    Function(Box<FunctionDefinition>),
    Type(Box<TypeDefinition>),
    Use(Box<TraitImplementation>),
    Impl(Box<Implementation>),
}

impl Symbol {
    pub fn id(&self) -> ItemId {
        match self {
            Symbol::Function(symbol) => symbol.id,
            Symbol::Type(symbol) => symbol.id(),
            Symbol::Use(symbol) => symbol.id,
            Symbol::Impl(symbol) => symbol.id,
        }
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct ExternalSymbol {
    pub name: SymbolName,
    pub location: Location,
}

impl ExternalSymbol {
    pub fn ident(&self) -> &PathSegment {
        &self.name.name
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Visibility {
    // Order matters here, since `Ord` and `PartialOrd` determines
    // the order of enums by the order of their variants!
    Private,
    Public,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub id: ItemId,
    pub func_id: Option<FunctionId>,
    pub visibility: Visibility,
    pub name: SymbolName,
    pub parameters: Vec<Parameter>,
    pub type_parameters: Vec<TypeParameter>,
    pub return_type: Option<Type>,
    pub block: Option<Block>,
    pub location: Location,
}

impl FunctionDefinition {
    pub fn ident(&self) -> &PathSegment {
        &self.name.name
    }
}

impl WithTypeParameters for FunctionDefinition {
    fn type_params(&self) -> &Vec<TypeParameter> {
        &self.type_parameters
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Identifier,
    pub param_type: Type,
    pub location: Location,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    Enum(Box<EnumDefinition>),
    Alias(Box<AliasDefinition>),
    Struct(Box<StructDefinition>),
    Trait(Box<TraitDefinition>),
}

impl TypeDefinition {
    pub fn id(&self) -> ItemId {
        match self {
            TypeDefinition::Enum(def) => def.id,
            TypeDefinition::Alias(def) => def.id,
            TypeDefinition::Struct(def) => def.id,
            TypeDefinition::Trait(def) => def.id,
        }
    }

    pub fn name(&self) -> &SymbolName {
        match self {
            TypeDefinition::Enum(def) => def.name(),
            TypeDefinition::Alias(def) => def.name(),
            TypeDefinition::Struct(def) => def.name(),
            TypeDefinition::Trait(def) => def.name(),
        }
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct EnumDefinition {
    pub id: ItemId,
    pub type_id: Option<TypeId>,
    pub name: SymbolName,
    pub cases: Vec<EnumDefinitionCase>,
    pub location: Location,
}

impl EnumDefinition {
    pub fn name(&self) -> &SymbolName {
        &self.name
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct EnumDefinitionCase {
    pub name: SymbolName,
    pub parameters: Vec<Box<Type>>,
    pub location: Location,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct AliasDefinition {
    pub id: ItemId,
    pub type_id: Option<TypeId>,
    pub name: SymbolName,
    pub definition: Box<Type>,
    pub location: Location,
}

impl AliasDefinition {
    pub fn name(&self) -> &SymbolName {
        &self.name
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct StructDefinition {
    pub id: ItemId,
    pub type_id: Option<TypeId>,
    pub name: SymbolName,
    pub builtin: bool,
    pub properties: Vec<Property>,
    pub methods: Vec<MethodDefinition>,
    pub type_parameters: Vec<TypeParameter>,
    pub location: Location,
}

impl StructDefinition {
    pub fn name(&self) -> &SymbolName {
        &self.name
    }

    pub fn properties(&self) -> impl Iterator<Item = &Property> {
        self.properties.iter()
    }

    pub fn properties_mut(&mut self) -> impl Iterator<Item = &mut Property> {
        self.properties.iter_mut()
    }

    pub fn methods(&self) -> impl Iterator<Item = &MethodDefinition> {
        self.methods.iter()
    }

    pub fn methods_mut(&mut self) -> impl Iterator<Item = &mut MethodDefinition> {
        self.methods.iter_mut()
    }
}

impl WithTypeParameters for StructDefinition {
    fn type_params(&self) -> &Vec<TypeParameter> {
        &self.type_parameters
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct Implementation {
    pub id: ItemId,
    pub impl_id: Option<ImplId>,
    pub target: Box<Type>,
    pub methods: Vec<MethodDefinition>,
    pub type_parameters: Vec<TypeParameter>,
    pub location: Location,
}

impl WithTypeParameters for Implementation {
    fn type_params(&self) -> &Vec<TypeParameter> {
        &self.type_parameters
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub enum StructMember {
    Property(Box<Property>),
    Method(Box<MethodDefinition>),
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct Property {
    pub prop_id: Option<PropertyId>,
    pub visibility: Visibility,
    pub name: Identifier,
    pub property_type: Type,
    pub default_value: Option<Expression>,
    pub location: Location,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct MethodDefinition {
    pub method_id: Option<MethodId>,
    pub visibility: Visibility,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub type_parameters: Vec<TypeParameter>,
    pub return_type: Option<Type>,
    pub block: Option<Block>,
    pub location: Location,
}

impl WithTypeParameters for MethodDefinition {
    fn type_params(&self) -> &Vec<TypeParameter> {
        &self.type_parameters
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct TraitDefinition {
    pub id: ItemId,
    pub type_id: Option<TypeId>,
    pub name: SymbolName,
    pub type_parameters: Vec<TypeParameter>,
    pub methods: Vec<TraitMethodDefinition>,
    pub location: Location,
}

impl TraitDefinition {
    pub fn name(&self) -> &SymbolName {
        &self.name
    }
}

impl WithTypeParameters for TraitDefinition {
    fn type_params(&self) -> &Vec<TypeParameter> {
        &self.type_parameters
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct TraitMethodDefinition {
    pub method_id: Option<MethodId>,
    pub visibility: Visibility,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub type_parameters: Vec<TypeParameter>,
    pub return_type: Option<Type>,
    pub block: Option<Block>,
    pub location: Location,
}

impl WithTypeParameters for TraitMethodDefinition {
    fn type_params(&self) -> &Vec<TypeParameter> {
        &self.type_parameters
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct TraitImplementation {
    pub id: ItemId,
    pub use_id: Option<UseId>,
    pub name: Box<Type>,
    pub target: Box<Type>,
    pub methods: Vec<TraitMethodImplementation>,
    pub location: Location,
}

impl TraitImplementation {
    pub fn ident(&self) -> &PathSegment {
        self.name.ident()
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct TraitMethodImplementation {
    pub method_id: Option<MethodId>,
    pub visibility: Visibility,
    pub name: SymbolName,
    pub parameters: Vec<Parameter>,
    pub type_parameters: Vec<TypeParameter>,
    pub return_type: Option<Type>,
    pub block: Block,
    pub location: Location,
}

impl TraitMethodImplementation {
    pub fn ident(&self) -> &PathSegment {
        &self.name.name
    }
}

impl WithTypeParameters for TraitMethodImplementation {
    fn type_params(&self) -> &Vec<TypeParameter> {
        &self.type_parameters
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct Statement {
    pub id: StatementId,
    pub kind: StatementKind,
    pub location: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    Variable(Box<VariableDeclaration>),
    Break(Box<Break>),
    Continue(Box<Continue>),
    Return(Box<Return>),
    If(Box<If>),
    Unless(Box<Unless>),
    InfiniteLoop(Box<InfiniteLoop>),
    IteratorLoop(Box<IteratorLoop>),
    PredicateLoop(Box<PredicateLoop>),
    Expression(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub id: StatementId,
    pub name: Identifier,
    pub declared_type: Option<Type>,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Break {
    pub id: StatementId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Continue {
    pub id: StatementId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub id: StatementId,
    pub value: Option<Expression>,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct If {
    pub id: StatementId,
    pub cases: Vec<Condition>,
    pub location: Location,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct Unless {
    pub id: StatementId,
    pub cases: Vec<Condition>,
    pub location: Location,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct Condition {
    pub id: StatementId,
    pub condition: Option<Expression>,
    pub block: Block,
    pub location: Location,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct InfiniteLoop {
    pub id: StatementId,
    pub block: Block,
    pub location: Location,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct IteratorLoop {
    pub id: StatementId,
    pub collection: Expression,
    pub block: Block,
    pub location: Location,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct PredicateLoop {
    pub id: StatementId,
    pub condition: Expression,
    pub block: Block,
    pub location: Location,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct Expression {
    pub id: ExpressionId,
    pub location: Location,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Assignment(Box<Assignment>),

    /// Defines a call which was invoked without any callee or receiver.
    ///
    /// These are either invoked from:
    /// - a path (`std::Int32::new()`),
    /// - or as a function call (`foo()`),
    StaticCall(Box<StaticCall>),

    /// Defines a call which was invoked within the context of a receiver
    ///
    /// ```lume
    /// let a = foo();
    /// a.bar();
    /// ```
    InstanceCall(Box<InstanceCall>),
    Literal(Box<Literal>),
    Member(Box<Member>),
    Variable(Box<Variable>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallExpression<'a> {
    /// Defines a call which was invoked without any callee or receiver.
    ///
    /// These are either invoked from:
    /// - a path (`std::Int32::new()`),
    /// - or as a function call (`foo()`),
    Static(&'a StaticCall),

    /// Defines a call which was invoked within the context of a receiver
    ///
    /// ```lume
    /// let a = foo();
    /// a.bar();
    /// ```
    Instanced(&'a InstanceCall),
}

impl CallExpression<'_> {
    #[inline]
    pub fn arguments(&self) -> &[Expression] {
        match self {
            Self::Instanced(call) => &call.arguments,
            Self::Static(call) => &call.arguments,
        }
    }

    #[inline]
    pub fn type_arguments(&self) -> &[TypeArgument] {
        match self {
            Self::Instanced(call) => &call.type_arguments,
            Self::Static(call) => &call.type_arguments,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub id: ExpressionId,
    pub target: Expression,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StaticCall {
    pub id: ExpressionId,
    pub name: SymbolName,
    pub type_arguments: Vec<TypeArgument>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstanceCall {
    pub id: ExpressionId,
    pub callee: Expression,
    pub name: PathSegment,
    pub type_arguments: Vec<TypeArgument>,
    pub arguments: Vec<Expression>,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct Literal {
    pub id: ExpressionId,
    pub location: Location,
    pub kind: LiteralKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Int(Box<IntLiteral>),
    Float(Box<FloatLiteral>),
    String(Box<StringLiteral>),
    Boolean(Box<BooleanLiteral>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntLiteral {
    pub id: ExpressionId,
    pub value: i64,
    pub kind: IntKind,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum IntKind {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    IPtr,
    UPtr,
}

impl From<lume_ast::IntKind> for IntKind {
    fn from(kind: lume_ast::IntKind) -> Self {
        match kind {
            lume_ast::IntKind::I8 => IntKind::I8,
            lume_ast::IntKind::U8 => IntKind::U8,
            lume_ast::IntKind::I16 => IntKind::I16,
            lume_ast::IntKind::U16 => IntKind::U16,
            lume_ast::IntKind::I32 => IntKind::I32,
            lume_ast::IntKind::U32 => IntKind::U32,
            lume_ast::IntKind::I64 => IntKind::I64,
            lume_ast::IntKind::U64 => IntKind::U64,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FloatLiteral {
    pub id: ExpressionId,
    pub value: f64,
    pub kind: FloatKind,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FloatKind {
    F32,
    F64,
}

impl From<lume_ast::FloatKind> for FloatKind {
    fn from(kind: lume_ast::FloatKind) -> Self {
        match kind {
            lume_ast::FloatKind::F32 => FloatKind::F32,
            lume_ast::FloatKind::F64 => FloatKind::F64,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub id: ExpressionId,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BooleanLiteral {
    pub id: ExpressionId,
    pub value: bool,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct Member {
    pub id: ExpressionId,
    pub callee: Expression,
    pub name: String,
    pub location: Location,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct Variable {
    pub id: ExpressionId,
    pub reference: StatementId,
    pub name: Identifier,
    pub location: Location,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct TypeParameter {
    pub name: Identifier,
    pub type_id: Option<TypeId>,
    pub type_param_id: Option<TypeParameterId>,
    pub constraints: Vec<Box<Type>>,
    pub location: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeArgument {
    /// Defines a named type argument, which was specified by the user, but not yet resolved.
    Named { ty: Type, location: Location },

    /// Defines an implicit type argument, which is up to the compiler to infer.
    Implicit { location: Location },
}

impl TypeArgument {
    /// Determines whether this type argument is named.
    pub fn is_named(&self) -> bool {
        matches!(self, TypeArgument::Named { .. })
    }

    /// Determines whether this type argument is implicit.
    pub fn is_implicit(&self) -> bool {
        matches!(self, TypeArgument::Implicit { .. })
    }
}

impl Node for TypeArgument {
    fn location(&self) -> &Location {
        match self {
            TypeArgument::Named { location, .. } | TypeArgument::Implicit { location } => location,
        }
    }
}

#[derive(Node, Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub id: ItemId,
    pub name: SymbolName,
    pub type_params: Vec<Box<Type>>,
    pub location: Location,
}

impl Type {
    pub fn ident(&self) -> &PathSegment {
        &self.name.name
    }
}

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}
