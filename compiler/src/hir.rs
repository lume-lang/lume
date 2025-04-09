use std::collections::HashMap;

use crate::ModuleFileId;
use ast::ast;
use derive::Node;

pub(crate) mod lower;

pub type ParsedExpressions = HashMap<ModuleFileId, Vec<ast::TopLevelExpression>>;

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Map(pub HashMap<ModuleFileId, Vec<Symbol>>);

impl Map {
    pub fn new() -> Self {
        Map(HashMap::new())
    }
}

/// Uniquely identifies a local value, such as a variable, argument or otherwise.
#[derive(serde::Serialize, Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct LocalId(pub u64);

/// Uniquely identifies a type definition, such as an alias, a class or enum.
#[derive(serde::Serialize, Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeId(pub u64);

#[derive(serde::Serialize, Hash, Debug, Clone, PartialEq, Eq)]
pub struct Location {
    /// Defines the file which the location refers to.
    pub file: ModuleFileId,

    /// Defines the index within the source file
    pub range: std::ops::Range<usize>,
}

impl Location {
    pub fn start(&self) -> usize {
        self.range.start
    }

    pub fn end(&self) -> usize {
        self.range.end
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

#[derive(serde::Serialize, Hash, Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
    pub location: Location,
}

#[derive(serde::Serialize, Hash, Debug, Clone, PartialEq, Eq)]
pub struct IdentifierPath {
    pub path: Vec<Identifier>,
    pub location: Location,
}

#[derive(serde::Serialize, Hash, Debug, Clone, PartialEq, Eq)]
pub struct SymbolName {
    /// Defines the namespace which the symbol was defined in, if any.
    pub namespace: Option<IdentifierPath>,

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

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct ExternalSymbol {
    pub name: SymbolName,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub visibility: Visibility,
    pub name: SymbolName,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct ExternalFunctionDefinition {
    pub visibility: Visibility,
    pub name: SymbolName,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub location: Location,
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

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct EnumDefinition {
    pub type_id: TypeId,
    pub name: SymbolName,
    pub cases: Vec<EnumDefinitionCase>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct EnumDefinitionCase {
    pub name: SymbolName,
    pub parameters: Vec<Box<Type>>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct AliasDefinition {
    pub type_id: TypeId,
    pub name: SymbolName,
    pub definition: Box<Type>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct ClassDefinition {
    pub type_id: TypeId,
    pub name: SymbolName,
    pub builtin: bool,
    pub members: Vec<ClassMember>,
    pub location: Location,
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
    pub property_type: Box<Type>,
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
pub enum Statement {
    Const(Box<ConstDeclaration>),
    Variable(Box<VariableDeclaration>),
    Break(Box<Break>),
    Continue(Box<Continue>),
    Return(Box<Return>),
    Expression(Box<Expression>),
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct ConstDeclaration {
    pub id: LocalId,
    pub name: Identifier,
    pub variable_type: Type,
    pub value: Expression,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub id: LocalId,
    pub name: Identifier,
    pub variable_type: Type,
    pub value: Expression,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Break {
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Continue {
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Return {
    pub value: Expression,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum Expression {
    Assignment(Box<Assignment>),
    Call(Box<Call>),
    Literal(Box<Literal>),
    Member(Box<Member>),
    Range(Box<Range>),
    Variable(Box<Variable>),
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Assignment {
    pub target: Expression,
    pub value: Expression,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Call {
    pub callee: Expression,
    pub name: Identifier,
    pub arguments: Vec<Expression>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum Literal {
    Int(Box<IntLiteral>),
    Float(Box<FloatLiteral>),
    String(Box<StringLiteral>),
    Boolean(Box<BooleanLiteral>),
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct IntLiteral {
    pub value: i64,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct FloatLiteral {
    pub value: f64,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub value: String,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct BooleanLiteral {
    pub value: bool,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Member {
    pub callee: Expression,
    pub name: String,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Range {
    pub lower: Expression,
    pub upper: Expression,
    pub inclusive: bool,
    pub location: Location,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Variable {
    pub reference: LocalId,
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
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct ScalarType {
    pub name: SymbolName,
    pub definition: TypeDefinition,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub element_type: Box<Type>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct GenericType {
    pub name: Identifier,
    pub type_params: Vec<Box<Type>>,
    pub location: Location,
}
