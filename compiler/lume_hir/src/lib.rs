use lume_macros::Node;
use lume_span::{Location, PackageId, hash_id};
use lume_types::{
    FunctionId, Identifier, ImplId, MethodId, NamespacePath, PropertyId, SymbolName, TypeId, TypeParameterId,
    Visibility,
};

mod errors;
pub mod lower;
pub mod map;
mod stdlib;
pub(crate) mod symbols;

pub const SELF_TYPE_NAME: &str = "self";

/// Trait for HIR nodes which can contain some amount of type parameters.
pub trait WithTypeParameters {
    /// Gets all the type parameters of this node.
    fn type_params(&self) -> &Vec<TypeParameter>;
}

/// Uniquely identifies a definition within a module, such as a type, function or class.
#[derive(Hash, Clone, Copy, PartialEq, Eq)]
pub struct ItemId(pub PackageId, pub u64);

impl std::fmt::Debug for ItemId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", hash_id(self))
    }
}

/// Uniquely identifies any local expression, such as variables, arguments, calls or otherwise.
#[derive(Hash, Clone, Copy, PartialEq, Eq)]
pub struct LocalId(pub u64);

impl LocalId {
    pub fn empty() -> Self {
        Self(0)
    }
}

impl std::fmt::Debug for LocalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Uniquely identifies any local statement.
#[derive(Hash, Clone, Copy, PartialEq, Eq)]
pub struct StatementId(pub PackageId, pub LocalId);

impl StatementId {
    pub fn empty() -> Self {
        Self(PackageId::empty(), LocalId::empty())
    }

    pub fn local(&self) -> LocalId {
        self.1
    }
}

impl std::fmt::Debug for StatementId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", hash_id(self))
    }
}

/// Uniquely identifies any local expression, such as variables, literals, calls or otherwise.
#[derive(Hash, Clone, Copy, PartialEq, Eq)]
pub struct ExpressionId(pub PackageId, pub LocalId);

impl ExpressionId {
    pub fn empty() -> Self {
        Self(PackageId::empty(), LocalId::empty())
    }

    pub fn local(&self) -> LocalId {
        self.1
    }
}

impl std::fmt::Debug for ExpressionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", hash_id(self))
    }
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
    pub id: ItemId,
    pub name: SymbolName,
    pub location: Location,
}

impl ExternalSymbol {
    pub fn ident(&self) -> &Identifier {
        &self.name.name
    }
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
    pub fn ident(&self) -> &Identifier {
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
    pub id: ExpressionId,
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
    pub id: ItemId,
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
    pub name: Box<Type>,
    pub target: Box<Type>,
    pub methods: Vec<TraitMethodImplementation>,
    pub location: Location,
}

impl TraitImplementation {
    pub fn ident(&self) -> &Identifier {
        self.name.ident()
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct TraitMethodImplementation {
    pub visibility: Visibility,
    pub name: SymbolName,
    pub parameters: Vec<Parameter>,
    pub type_parameters: Vec<TypeParameter>,
    pub return_type: Option<Type>,
    pub block: Block,
    pub location: Location,
}

impl TraitMethodImplementation {
    pub fn ident(&self) -> &Identifier {
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
    pub name: Identifier,
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

    /// Defines a named type argument, which has been resolved.
    Resolved {
        ty: lume_types::TypeRef,
        location: Location,
    },

    /// Defines an implicit type argument, which is up to the compiler to infer.
    Implicit { location: Location },
}

impl TypeArgument {
    /// Determines whether this type argument is named.
    pub fn is_named(&self) -> bool {
        matches!(self, TypeArgument::Named { .. })
    }

    /// Determines whether this type argument is resolved.
    pub fn is_resolved(&self) -> bool {
        matches!(self, TypeArgument::Resolved { .. })
    }

    /// Determines whether this type argument is implicit.
    pub fn is_implicit(&self) -> bool {
        matches!(self, TypeArgument::Implicit { .. })
    }
}

impl Node for TypeArgument {
    fn location(&self) -> &Location {
        match self {
            TypeArgument::Named { location, .. }
            | TypeArgument::Resolved { location, .. }
            | TypeArgument::Implicit { location } => location,
        }
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct Type {
    pub name: SymbolName,
    pub type_params: Vec<Box<Type>>,
    pub location: Location,
}

impl Type {
    pub fn ident(&self) -> &Identifier {
        &self.name.name
    }
}

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}
