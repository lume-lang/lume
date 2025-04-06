use std::ops::Range;

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

macro_rules! impl_node {
    ($name:ident) => {
        impl Node for $name {
            fn location(&self) -> &Location {
                &self.location
            }
        }
    };
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
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

impl_node!(Block);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Identifier,
    pub param_type: Type,
    pub location: Location,
}

impl_node!(Parameter);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum Visibility {
    Public(Box<Public>),
    Private(Box<Private>),
}

impl Node for Visibility {
    fn location(&self) -> &Location {
        match self {
            Visibility::Public(c) => c.location(),
            Visibility::Private(c) => c.location(),
        }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Public {
    pub location: Location,
}

impl_node!(Public);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Private {
    pub location: Location,
}

impl_node!(Private);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum TopLevelExpression {
    Import(Box<Import>),
    Class(Box<ClassDefinition>),
    FunctionDefinition(Box<FunctionDefinition>),
    TypeDefinition(Box<TypeDefinition>),
}

impl Node for TopLevelExpression {
    fn location(&self) -> &Location {
        match self {
            TopLevelExpression::Import(c) => c.location(),
            TopLevelExpression::Class(c) => c.location(),
            TopLevelExpression::FunctionDefinition(c) => c.location(),
            TopLevelExpression::TypeDefinition(c) => c.location(),
        }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Import {
    pub path: IdentifierPath,
    pub location: Location,
}

impl_node!(Import);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct ClassDefinition {
    pub name: Identifier,
    pub builtin: bool,
    pub members: Vec<ClassMember>,
    pub location: Location,
}

impl_node!(ClassDefinition);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum ClassMember {
    Property(Box<Property>),
    MethodDefinition(Box<MethodDefinition>),
}

impl Node for ClassMember {
    fn location(&self) -> &Location {
        match self {
            ClassMember::Property(c) => c.location(),
            ClassMember::MethodDefinition(c) => c.location(),
        }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Property {
    pub visibility: Visibility,
    pub name: Identifier,
    pub property_type: Option<Box<Type>>,
    pub default_value: Option<Expression>,
    pub location: Location,
}

impl_node!(Property);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct MethodDefinition {
    pub visibility: Visibility,
    pub external: bool,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub block: Block,
    pub location: Location,
}

impl_node!(MethodDefinition);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub visibility: Visibility,
    pub external: bool,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub block: Block,
    pub location: Location,
}

impl_node!(FunctionDefinition);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    Enum(Box<EnumDefinition>),
    Alias(Box<AliasDefinition>),
}

impl Node for TypeDefinition {
    fn location(&self) -> &Location {
        match self {
            TypeDefinition::Enum(c) => c.location(),
            TypeDefinition::Alias(c) => c.location(),
        }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct EnumDefinition {
    pub name: Identifier,
    pub cases: Vec<EnumDefinitionCase>,
    pub location: Location,
}

impl_node!(EnumDefinition);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct EnumDefinitionCase {
    pub name: Identifier,
    pub parameters: Vec<Box<Type>>,
    pub location: Location,
}

impl_node!(EnumDefinitionCase);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct AliasDefinition {
    pub name: Identifier,
    pub definition: Box<Type>,
    pub location: Location,
}

impl_node!(AliasDefinition);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum Statement {
    VariableDeclaration(Box<VariableDeclaration>),
    If(Box<IfCondition>),
    Unless(Box<UnlessCondition>),
    Return(Box<Return>),
}

impl Node for Statement {
    fn location(&self) -> &Location {
        match self {
            Statement::VariableDeclaration(c) => c.location(),
            Statement::If(c) => c.location(),
            Statement::Unless(c) => c.location(),
            Statement::Return(c) => c.location(),
        }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub variable_type: Option<Type>,
    pub value: Expression,
    pub is_const: bool,
    pub location: Location,
}

impl_node!(VariableDeclaration);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct IfCondition {
    pub cases: Vec<Condition>,
    pub location: Location,
}

impl_node!(IfCondition);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct UnlessCondition {
    pub cases: Vec<Condition>,
    pub location: Location,
}

impl_node!(UnlessCondition);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Condition {
    pub condition: Option<Expression>,
    pub block: Block,
    pub location: Location,
}

impl_node!(Condition);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Return {
    pub value: Expression,
    pub location: Location,
}

impl_node!(Return);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum Expression {
    Assignment(Box<Assignment>),
    Call(Box<Call>),
    Identifier(Box<Identifier>),
    Literal(Box<Literal>),
    Member(Box<Member>),
    Variable(Box<Variable>),
}

impl Node for Expression {
    fn location(&self) -> &Location {
        match self {
            Expression::Assignment(c) => c.location(),
            Expression::Call(c) => c.location(),
            Expression::Identifier(c) => c.location(),
            Expression::Literal(c) => c.location(),
            Expression::Member(c) => c.location(),
            Expression::Variable(c) => c.location(),
        }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Assignment {
    pub target: Expression,
    pub value: Expression,
    pub location: Location,
}

impl_node!(Assignment);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Call {
    pub callee: Option<Expression>,
    pub name: Identifier,
    pub arguments: Vec<Expression>,
    pub location: Location,
}

impl_node!(Call);

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

#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
    pub location: Location,
}

impl_node!(Identifier);

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

impl_node!(IdentifierPath);

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

impl Node for Literal {
    fn location(&self) -> &Location {
        match self {
            Literal::Int(c) => c.location(),
            Literal::Float(c) => c.location(),
            Literal::String(c) => c.location(),
            Literal::Boolean(c) => c.location(),
        }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct IntLiteral {
    pub value: i64,
    pub location: Location,
}

impl_node!(IntLiteral);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct FloatLiteral {
    pub value: f64,
    pub location: Location,
}

impl_node!(FloatLiteral);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub value: String,
    pub location: Location,
}

impl_node!(StringLiteral);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct BooleanLiteral {
    pub value: bool,
    pub location: Location,
}

impl_node!(BooleanLiteral);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Member {
    pub callee: Expression,
    pub name: String,
    pub location: Location,
}

impl_node!(Member);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: Identifier,
}

impl Node for Variable {
    fn location(&self) -> &Location {
        &self.name.location
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum Type {
    Scalar(Box<ScalarType>),
    Array(Box<ArrayType>),
    Generic(Box<GenericType>),
}

impl Node for Type {
    fn location(&self) -> &Location {
        match self {
            Type::Scalar(c) => c.location(),
            Type::Array(c) => c.location(),
            Type::Generic(c) => c.location(),
        }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct ScalarType {
    pub name: String,
    pub location: Location,
}

impl_node!(ScalarType);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub element_type: Box<Type>,
    pub location: Location,
}

impl_node!(ArrayType);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct GenericType {
    pub element_types: Vec<Box<Type>>,
    pub location: Location,
}

impl_node!(GenericType);
