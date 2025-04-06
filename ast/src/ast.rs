use std::ops::Range;

use derive::Node;

#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Location(pub Range<usize>);

impl Location {
    pub fn start(&self) -> usize {
        self.0.start
    }

    pub fn end(&self) -> usize {
        self.0.end
    }
}

impl From<Range<usize>> for Location {
    fn from(range: Range<usize>) -> Location {
        Location(range)
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

impl Block {
    pub fn from_location(location: impl Into<Location>) -> Self {
        Self {
            statements: Vec::new(),
            location: location.into(),
        }
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Identifier,
    pub param_type: Type,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum Visibility {
    Public(Box<Public>),
    Private(Box<Private>),
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Public {
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Private {
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum TopLevelExpression {
    Import(Box<Import>),
    Class(Box<ClassDefinition>),
    FunctionDefinition(Box<FunctionDefinition>),
    TypeDefinition(Box<TypeDefinition>),
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Import {
    pub path: IdentifierPath,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct ClassDefinition {
    pub name: Identifier,
    pub builtin: bool,
    pub members: Vec<ClassMember>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum ClassMember {
    Property(Box<Property>),
    MethodDefinition(Box<MethodDefinition>),
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Property {
    pub visibility: Visibility,
    pub name: Identifier,
    pub property_type: Option<Box<Type>>,
    pub default_value: Option<Expression>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct MethodDefinition {
    pub visibility: Visibility,
    pub external: bool,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub visibility: Visibility,
    pub external: bool,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    Enum(Box<EnumDefinition>),
    Alias(Box<AliasDefinition>),
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct EnumDefinition {
    pub name: Identifier,
    pub cases: Vec<EnumDefinitionCase>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct EnumDefinitionCase {
    pub name: Identifier,
    pub parameters: Vec<Box<Type>>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct AliasDefinition {
    pub name: Identifier,
    pub definition: Box<Type>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum Statement {
    VariableDeclaration(Box<VariableDeclaration>),
    If(Box<IfCondition>),
    Unless(Box<UnlessCondition>),
    Return(Box<Return>),
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub variable_type: Option<Type>,
    pub value: Expression,
    pub is_const: bool,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct IfCondition {
    pub cases: Vec<Condition>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct UnlessCondition {
    pub cases: Vec<Condition>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Condition {
    pub condition: Option<Expression>,
    pub block: Block,
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
    Identifier(Box<Identifier>),
    Literal(Box<Literal>),
    Member(Box<Member>),
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
    pub callee: Option<Expression>,
    pub name: Identifier,
    pub arguments: Vec<Expression>,
    pub location: Location,
}

impl Call {
    pub fn new(
        callee: Option<Expression>,
        name: Identifier,
        arguments: Vec<Expression>,
        location: Location,
    ) -> Expression {
        let call = Call {
            callee,
            name,
            arguments,
            location,
        };

        Expression::Call(Box::new(call))
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
    pub location: Location,
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq, Eq)]
pub struct IdentifierPath {
    pub path: Vec<Identifier>,
    pub location: Location,
}

impl std::fmt::Display for IdentifierPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let joined = self
            .path
            .iter()
            .map(|i| i.name.as_str())
            .collect::<Vec<&str>>()
            .join(".");

        f.write_str(&joined)
    }
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

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Variable {
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
    pub name: String,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub element_type: Box<Type>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct GenericType {
    pub element_types: Vec<Box<Type>>,
    pub location: Location,
}
