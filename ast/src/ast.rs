#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
}

impl Block {
    pub fn empty() -> Self {
        Self { statements: Vec::new() }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Identifier,
    pub param_type: Type,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum Visibility {
    Public(Box<Public>),
    Private(Box<Private>),
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Public {}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Private {}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum TopLevelExpression {
    Import(Box<Import>),
    Class(Box<ClassDefinition>),
    FunctionDefinition(Box<FunctionDefinition>),
    TypeDefinition(Box<TypeDefinition>),
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Import {
    pub path: IdentifierPath,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct ClassDefinition {
    pub name: Identifier,
    pub builtin: bool,
    pub members: Vec<ClassMember>,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum ClassMember {
    Property(Box<Property>),
    MethodDefinition(Box<MethodDefinition>),
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Property {
    pub visibility: Visibility,
    pub name: Identifier,
    pub property_type: Option<Box<Type>>,
    pub default_value: Option<Expression>,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct MethodDefinition {
    pub visibility: Visibility,
    pub external: bool,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub block: Block,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub visibility: Visibility,
    pub external: bool,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub block: Block,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    Enum(Box<EnumDefinition>),
    Alias(Box<AliasDefinition>),
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct EnumDefinition {
    pub name: Identifier,
    pub cases: Vec<EnumDefinitionCase>,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct EnumDefinitionCase {
    pub name: Identifier,

    pub parameters: Vec<Box<Type>>,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct AliasDefinition {
    pub name: Identifier,
    pub definition: Box<Type>,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum Statement {
    VariableDeclaration(Box<VariableDeclaration>),
    If(Box<IfCondition>),
    Unless(Box<UnlessCondition>),
    Return(Box<Return>),
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub variable_type: Option<Type>,
    pub value: Expression,
    pub is_const: bool,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct IfCondition {
    pub cases: Vec<Condition>,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct UnlessCondition {
    pub cases: Vec<Condition>,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Condition {
    pub condition: Option<Expression>,
    pub block: Block,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Return {
    pub value: Expression,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum Expression {
    Assignment(Box<Assignment>),
    Call(Box<Call>),
    Identifier(Box<Identifier>),
    Literal(Box<Literal>),
    Member(Box<Member>),
    Variable(Box<Variable>),
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Assignment {
    pub target: Expression,
    pub value: Expression,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Call {
    pub callee: Option<Expression>,
    pub name: Identifier,
    pub arguments: Vec<Expression>,
}

impl Call {
    pub fn new(callee: Option<Expression>, name: Identifier, arguments: Vec<Expression>) -> Expression {
        let call = Call {
            callee,
            name,
            arguments,
        };

        Expression::Call(Box::new(call))
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
}

impl From<String> for Identifier {
    fn from(name: String) -> Self {
        Identifier { name }
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

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum Literal {
    Int(Box<IntLiteral>),
    Float(Box<FloatLiteral>),
    String(Box<StringLiteral>),
    Boolean(Box<BooleanLiteral>),
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct IntLiteral {
    pub value: i64,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct FloatLiteral {
    pub value: f64,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub value: String,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct BooleanLiteral {
    pub value: bool,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Member {
    pub callee: Expression,
    pub name: String,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: Identifier,
}

impl From<&str> for Variable {
    fn from(value: &str) -> Variable {
        let ident = Identifier {
            name: value.to_string(),
        };

        Variable { name: ident }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum Type {
    Scalar(Box<ScalarType>),
    Array(Box<ArrayType>),
    Generic(Box<GenericType>),
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct ScalarType {
    pub name: String,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub element_type: Box<Type>,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct GenericType {
    pub element_types: Vec<Box<Type>>,
}
