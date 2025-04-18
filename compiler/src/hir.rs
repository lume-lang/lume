use crate::{
    driver::{ModuleFileId, ModuleId},
    id::hash_id,
};
use derive::Node;

mod errors;
pub(crate) mod lower;
pub(crate) mod map;
pub(crate) mod symbols;

pub type Map = map::Map;

/// Trait for types that can be converted into an owner ID.
pub trait IntoOwner {
    /// Gets the ID of the module file that owns this node.
    fn owner(&self) -> ModuleFileId;
}

/// Uniquely identifies a definition within a module, such as a type, function or class.
#[derive(serde::Serialize, Hash, Clone, Copy, PartialEq, Eq)]
pub struct ItemId(pub ModuleFileId, pub u64);

impl std::fmt::Debug for ItemId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", hash_id(self))
    }
}

impl IntoOwner for ItemId {
    fn owner(&self) -> ModuleFileId {
        self.0
    }
}

#[derive(serde::Serialize, Hash, Clone, Copy, PartialEq, Eq)]
pub enum LocalKind {
    Statement,
    Expression,
}

/// Uniquely identifies any local expression, such as variables, arguments, calls or otherwise.
#[derive(serde::Serialize, Hash, Clone, Copy, PartialEq, Eq)]
pub struct LocalId(pub u64, pub LocalKind);

impl LocalId {
    pub fn empty() -> Self {
        Self(0, LocalKind::Statement)
    }

    pub fn kind(&self) -> LocalKind {
        self.1
    }
}

impl std::fmt::Debug for LocalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Uniquely identifies any local expression, such as variables, arguments, calls or otherwise.
#[derive(serde::Serialize, Hash, Clone, Copy, PartialEq, Eq)]
pub struct NodeId(pub ModuleFileId, pub LocalId);

impl NodeId {
    pub fn empty() -> Self {
        Self(ModuleFileId::empty(), LocalId::empty())
    }

    pub fn local(&self) -> LocalId {
        self.1
    }

    pub fn kind(&self) -> LocalKind {
        self.local().kind()
    }
}

impl std::fmt::Debug for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", hash_id(self))
    }
}

impl IntoOwner for NodeId {
    fn owner(&self) -> ModuleFileId {
        self.0
    }
}

#[derive(serde::Serialize, Hash, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    /// Defines the file which the location refers to.
    pub file: ModuleFileId,

    /// Defines the index within the source file, where the location starts.
    pub start: usize,

    /// Defines the length of the span.
    pub length: usize,
}

impl Location {
    pub fn empty() -> Self {
        Self {
            file: ModuleFileId(ModuleId(0), 0),
            start: 0,
            length: 0,
        }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.start + self.length
    }

    pub fn len(&self) -> usize {
        self.length
    }
}

impl Into<std::ops::Range<usize>> for Location {
    fn into(self) -> std::ops::Range<usize> {
        self.start..self.end()
    }
}

impl std::fmt::Debug for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}:{}:{}", self.file, self.start, self.end())
    }
}

pub trait Node {
    fn location(&self) -> &Location;
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub location: Location,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
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

#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
pub struct IdentifierPath {
    pub path: Vec<Identifier>,
    pub location: Location,
}

impl IdentifierPath {
    pub fn empty() -> Self {
        Self {
            path: Vec::new(),
            location: Location::empty(),
        }
    }
}

impl std::hash::Hash for IdentifierPath {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.path.hash(state);
    }
}

#[derive(serde::Serialize, Hash, Clone, PartialEq, Eq)]
pub struct SymbolName {
    /// Defines the namespace which the symbol was defined in.
    pub namespace: IdentifierPath,

    /// Defines the relative name of the symbol within it's namespace.
    pub name: Identifier,
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

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum Symbol {
    Extern(Box<ExternalSymbol>),
    Function(Box<FunctionDefinition>),
    ExternalFunction(Box<ExternalFunctionDefinition>),
    Type(Box<TypeDefinition>),
    Impl(Box<TraitImplementation>),
}

impl Symbol {
    pub fn id(&self) -> ItemId {
        match self {
            Symbol::Extern(symbol) => symbol.id,
            Symbol::Function(symbol) => symbol.id,
            Symbol::ExternalFunction(symbol) => symbol.id,
            Symbol::Type(symbol) => symbol.id(),
            Symbol::Impl(symbol) => symbol.id,
        }
    }

    pub fn ident(&self) -> &Identifier {
        match self {
            Symbol::Extern(symbol) => symbol.ident(),
            Symbol::Function(symbol) => symbol.ident(),
            Symbol::ExternalFunction(symbol) => symbol.ident(),
            Symbol::Type(symbol) => symbol.ident(),
            Symbol::Impl(symbol) => symbol.ident(),
        }
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
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

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub id: ItemId,
    pub visibility: Visibility,
    pub name: SymbolName,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub block: Block,
    pub location: Location,
}

impl FunctionDefinition {
    pub fn ident(&self) -> &Identifier {
        &self.name.name
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct ExternalFunctionDefinition {
    pub id: ItemId,
    pub visibility: Visibility,
    pub name: SymbolName,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub location: Location,
}

impl ExternalFunctionDefinition {
    pub fn ident(&self) -> &Identifier {
        &self.name.name
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Parameter {
    pub id: NodeId,
    pub name: Identifier,
    pub param_type: Type,
    pub location: Location,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Visibility {
    // Order matters here, since `Ord` and `PartialOrd` determines
    // the order of enums by the order of their variants!
    Private,
    Public,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    Enum(Box<EnumDefinition>),
    Alias(Box<AliasDefinition>),
    Class(Box<ClassDefinition>),
    Trait(Box<TraitDefinition>),
}

impl TypeDefinition {
    pub fn id(&self) -> ItemId {
        match self {
            TypeDefinition::Enum(def) => def.id,
            TypeDefinition::Alias(def) => def.id,
            TypeDefinition::Class(def) => def.id,
            TypeDefinition::Trait(def) => def.id,
        }
    }

    pub fn ident(&self) -> &Identifier {
        match self {
            TypeDefinition::Enum(def) => def.ident(),
            TypeDefinition::Alias(def) => def.ident(),
            TypeDefinition::Class(def) => def.ident(),
            TypeDefinition::Trait(def) => def.ident(),
        }
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct EnumDefinition {
    pub id: ItemId,
    pub name: SymbolName,
    pub cases: Vec<EnumDefinitionCase>,
    pub location: Location,
}

impl EnumDefinition {
    pub fn ident(&self) -> &Identifier {
        &self.name.name
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct EnumDefinitionCase {
    pub id: ItemId,
    pub name: SymbolName,
    pub parameters: Vec<Box<Type>>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct AliasDefinition {
    pub id: ItemId,
    pub name: SymbolName,
    pub definition: Box<Type>,
    pub location: Location,
}

impl AliasDefinition {
    pub fn ident(&self) -> &Identifier {
        &self.name.name
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct ClassDefinition {
    pub id: ItemId,
    pub name: SymbolName,
    pub builtin: bool,
    pub members: Vec<ClassMember>,
    pub location: Location,
}

impl ClassDefinition {
    pub fn ident(&self) -> &Identifier {
        &self.name.name
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum ClassMember {
    Property(Box<Property>),
    Method(Box<MethodDefinition>),
    ExternalMethod(Box<ExternalMethodDefinition>),
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Property {
    pub visibility: Visibility,
    pub name: Identifier,
    pub property_type: Type,
    pub default_value: Option<Expression>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct MethodDefinition {
    pub visibility: Visibility,
    pub name: SymbolName,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct ExternalMethodDefinition {
    pub visibility: Visibility,
    pub name: SymbolName,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct TraitDefinition {
    pub id: ItemId,
    pub name: SymbolName,
    pub methods: Vec<TraitMethodDefinition>,
    pub location: Location,
}

impl TraitDefinition {
    pub fn ident(&self) -> &Identifier {
        &self.name.name
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct TraitMethodDefinition {
    pub visibility: Visibility,
    pub name: SymbolName,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub block: Option<Block>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct TraitImplementation {
    pub id: ItemId,
    pub name: SymbolName,
    pub target: SymbolName,
    pub methods: Vec<TraitMethodImplementation>,
    pub location: Location,
}

impl TraitImplementation {
    pub fn ident(&self) -> &Identifier {
        &self.name.name
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct TraitMethodImplementation {
    pub visibility: Visibility,
    pub name: SymbolName,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub block: Block,
    pub location: Location,
}

impl TraitMethodImplementation {
    pub fn ident(&self) -> &Identifier {
        &self.name.name
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Statement {
    pub id: NodeId,
    pub kind: StatementKind,
    pub location: Location,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
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

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub id: NodeId,
    pub name: Identifier,
    pub declared_type: Option<Type>,
    pub value: Expression,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Break {
    pub id: NodeId,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Continue {
    pub id: NodeId,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Return {
    pub id: NodeId,
    pub value: Option<Expression>,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct If {
    pub id: NodeId,
    pub cases: Vec<Condition>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Unless {
    pub id: NodeId,
    pub cases: Vec<Condition>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Condition {
    pub id: NodeId,
    pub condition: Option<Expression>,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct InfiniteLoop {
    pub id: NodeId,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct IteratorLoop {
    pub id: NodeId,
    pub collection: Expression,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct PredicateLoop {
    pub id: NodeId,
    pub condition: Expression,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Expression {
    pub id: NodeId,
    pub location: Location,
    pub kind: ExpressionKind,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Assignment(Box<Assignment>),
    New(Box<New>),
    FunctionCall(Box<FunctionCall>),
    MethodCall(Box<MethodCall>),
    Literal(Box<Literal>),
    Member(Box<Member>),
    Range(Box<Range>),
    Variable(Box<Variable>),
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Assignment {
    pub id: NodeId,
    pub target: Expression,
    pub value: Expression,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct New {
    pub id: NodeId,
    pub name: Box<Type>,
    pub arguments: Vec<Expression>,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub id: NodeId,
    pub reference: ItemId,
    pub name: Identifier,
    pub arguments: Vec<Expression>,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct MethodCall {
    pub id: NodeId,
    pub callee: Expression,
    pub name: Identifier,
    pub arguments: Vec<Expression>,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Literal {
    pub id: NodeId,
    pub location: Location,
    pub kind: LiteralKind,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Int(Box<IntLiteral>),
    Float(Box<FloatLiteral>),
    String(Box<StringLiteral>),
    Boolean(Box<BooleanLiteral>),
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct IntLiteral {
    pub id: NodeId,
    pub value: i64,
    pub kind: IntKind,
}

#[derive(serde::Serialize, Debug, Copy, Clone, PartialEq)]
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

impl From<ast::ast::IntKind> for IntKind {
    fn from(kind: ast::ast::IntKind) -> Self {
        match kind {
            ast::ast::IntKind::I8 => IntKind::I8,
            ast::ast::IntKind::U8 => IntKind::U8,
            ast::ast::IntKind::I16 => IntKind::I16,
            ast::ast::IntKind::U16 => IntKind::U16,
            ast::ast::IntKind::I32 => IntKind::I32,
            ast::ast::IntKind::U32 => IntKind::U32,
            ast::ast::IntKind::I64 => IntKind::I64,
            ast::ast::IntKind::U64 => IntKind::U64,
        }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct FloatLiteral {
    pub id: NodeId,
    pub value: f64,
    pub kind: FloatKind,
}

#[derive(serde::Serialize, Debug, Copy, Clone, PartialEq)]
pub enum FloatKind {
    F32,
    F64,
}

impl From<ast::ast::FloatKind> for FloatKind {
    fn from(kind: ast::ast::FloatKind) -> Self {
        match kind {
            ast::ast::FloatKind::F32 => FloatKind::F32,
            ast::ast::FloatKind::F64 => FloatKind::F64,
        }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub id: NodeId,
    pub value: String,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct BooleanLiteral {
    pub id: NodeId,
    pub value: bool,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Member {
    pub id: NodeId,
    pub callee: Expression,
    pub name: String,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Range {
    pub id: NodeId,
    pub lower: Expression,
    pub upper: Expression,
    pub inclusive: bool,
    pub location: Location,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Variable {
    pub id: NodeId,
    pub reference: NodeId,
    pub name: Identifier,
}

impl Node for Variable {
    fn location(&self) -> &Location {
        &self.name.location
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum Type {
    Scalar(Box<ScalarType>),
    Array(Box<ArrayType>),
    Generic(Box<GenericType>),
    SelfRef(Box<SelfType>),
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct ScalarType {
    pub reference: ItemId,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub element_type: Box<Type>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct GenericType {
    pub reference: ItemId,
    pub type_params: Vec<Box<Type>>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct SelfType {
    pub reference: ItemId,
    pub location: Location,
}
