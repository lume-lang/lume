use std::collections::HashMap;

use crate::{ModuleFileId, ModuleId, id::hash_id};
use ast::ast;
use derive::Node;

mod errors;
pub(crate) mod lower;
pub(crate) mod map;
pub(crate) mod symbols;

pub type Map = map::Map;

pub type ParsedExpressions = HashMap<ModuleFileId, Vec<ast::TopLevelExpression>>;

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Symbols(pub HashMap<ModuleFileId, Vec<Symbol>>);

impl Symbols {
    pub fn new() -> Self {
        Symbols(HashMap::new())
    }
}

/// Uniquely identifies a definition within a module, such as a type, function or class.
#[derive(serde::Serialize, Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct ItemId(pub ModuleId, pub u64);

/// Uniquely identifies any local expression, such as variables, arguments, calls or otherwise.
#[derive(serde::Serialize, Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct LocalId(pub u64);

/// Defines a trait which can convert any Id type into a `u64` type.
pub trait Indexable {
    /// Converts the Id type into a `u64` value.
    fn index(&self) -> u64;
}

impl Indexable for ItemId {
    fn index(&self) -> u64 {
        hash_id(&(self.0, self.1))
    }
}

impl Indexable for LocalId {
    fn index(&self) -> u64 {
        self.0
    }
}

/// Defines a generic struct which any `Id` type can turn into.
#[derive(serde::Serialize, Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Index(pub u64);

impl Index {
    pub fn from<T: Indexable>(id: &T) -> Self {
        Self(id.index())
    }
}

impl Indexable for Index {
    fn index(&self) -> u64 {
        self.0
    }
}

#[derive(serde::Serialize, Hash, Debug, Clone, Copy, PartialEq, Eq)]
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
            file: ModuleFileId(0),
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

#[derive(serde::Serialize, Hash, Debug, Clone, PartialEq, Eq)]
pub struct SymbolName {
    /// Defines the namespace which the symbol was defined in.
    pub namespace: IdentifierPath,

    /// Defines the relative name of the symbol within it's namespace.
    pub name: Identifier,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum Symbol {
    Extern(Box<ExternalSymbol>),
    Function(Box<FunctionDefinition>),
    ExternalFunction(Box<ExternalFunctionDefinition>),
    Type(Box<TypeDefinition>),
}

impl Symbol {
    pub fn id(&self) -> ItemId {
        match self {
            Symbol::Extern(symbol) => symbol.id,
            Symbol::Function(symbol) => symbol.id,
            Symbol::ExternalFunction(symbol) => symbol.id,
            Symbol::Type(symbol) => symbol.id(),
        }
    }

    pub fn ident(&self) -> &Identifier {
        match self {
            Symbol::Extern(symbol) => symbol.ident(),
            Symbol::Function(symbol) => symbol.ident(),
            Symbol::ExternalFunction(symbol) => symbol.ident(),
            Symbol::Type(symbol) => symbol.ident(),
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
    pub id: LocalId,
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
}

impl TypeDefinition {
    pub fn id(&self) -> ItemId {
        match self {
            TypeDefinition::Enum(def) => def.id,
            TypeDefinition::Alias(def) => def.id,
            TypeDefinition::Class(def) => def.id,
        }
    }

    pub fn ident(&self) -> &Identifier {
        match self {
            TypeDefinition::Enum(def) => def.ident(),
            TypeDefinition::Alias(def) => def.ident(),
            TypeDefinition::Class(def) => def.ident(),
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
    Use(Box<UseTrait>),
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
pub struct UseTrait {
    pub trait_type: Box<Type>,
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
pub struct Statement {
    pub id: LocalId,
    pub kind: StatementKind,
    pub location: Location,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum StatementKind {
    Const(Box<ConstDeclaration>),
    Variable(Box<VariableDeclaration>),
    Break(Box<Break>),
    Continue(Box<Continue>),
    Return(Box<Return>),
    Expression(Box<Expression>),
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct ConstDeclaration {
    pub id: LocalId,
    pub name: Identifier,
    pub declared_type: Option<Type>,
    pub value: Expression,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub id: LocalId,
    pub name: Identifier,
    pub declared_type: Option<Type>,
    pub value: Expression,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Break {
    pub id: LocalId,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Continue {
    pub id: LocalId,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Return {
    pub id: LocalId,
    pub value: Expression,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Expression {
    pub id: LocalId,
    pub location: Location,
    pub kind: ExpressionKind,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Assignment(Box<Assignment>),
    FunctionCall(Box<FunctionCall>),
    MethodCall(Box<MethodCall>),
    Literal(Box<Literal>),
    Member(Box<Member>),
    Range(Box<Range>),
    Variable(Box<Variable>),
    If(Box<If>),
    Unless(Box<Unless>),
    InfiniteLoop(Box<InfiniteLoop>),
    IteratorLoop(Box<IteratorLoop>),
    PredicateLoop(Box<PredicateLoop>),
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Assignment {
    pub id: LocalId,
    pub target: Expression,
    pub value: Expression,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub id: LocalId,
    pub reference: ItemId,
    pub name: Identifier,
    pub arguments: Vec<Expression>,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct MethodCall {
    pub id: LocalId,
    pub callee: Expression,
    pub name: Identifier,
    pub arguments: Vec<Expression>,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Literal {
    pub id: LocalId,
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
    pub id: LocalId,
    pub value: i64,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct FloatLiteral {
    pub id: LocalId,
    pub value: f64,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub id: LocalId,
    pub value: String,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct BooleanLiteral {
    pub id: LocalId,
    pub value: bool,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Member {
    pub id: LocalId,
    pub callee: Expression,
    pub name: String,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Range {
    pub id: LocalId,
    pub lower: Expression,
    pub upper: Expression,
    pub inclusive: bool,
    pub location: Location,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Variable {
    pub id: LocalId,
    pub reference: LocalId,
    pub name: Identifier,
}

impl Node for Variable {
    fn location(&self) -> &Location {
        &self.name.location
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct If {
    pub id: LocalId,
    pub cases: Vec<Condition>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Unless {
    pub id: LocalId,
    pub cases: Vec<Condition>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Condition {
    pub id: LocalId,
    pub condition: Option<Expression>,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct InfiniteLoop {
    pub id: LocalId,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct IteratorLoop {
    pub id: LocalId,
    pub collection: Expression,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct PredicateLoop {
    pub id: LocalId,
    pub condition: Expression,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum Type {
    Scalar(Box<ScalarType>),
    Array(Box<ArrayType>),
    Generic(Box<GenericType>),
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
