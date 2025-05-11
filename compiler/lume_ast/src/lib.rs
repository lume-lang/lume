use lume_macros::Node;

#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Location(pub std::ops::Range<usize>);

impl Location {
    pub fn start(&self) -> usize {
        self.0.start
    }

    pub fn end(&self) -> usize {
        self.0.end
    }

    pub fn len(&self) -> usize {
        self.0.end - self.0.start
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl From<std::ops::Range<usize>> for Location {
    fn from(range: std::ops::Range<usize>) -> Location {
        Location(range)
    }
}

pub trait Node {
    fn location(&self) -> &Location;
}

#[derive(serde::Serialize, Node, Debug, Clone, Eq)]
pub struct Identifier {
    pub name: String,
    pub location: Location,
}

impl Identifier {
    pub fn new(name: &str) -> Self {
        Identifier {
            name: name.to_string(),
            location: Location(0..0),
        }
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

impl std::hash::Hash for Identifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, Eq)]
pub struct NamespacePath {
    pub path: Vec<Identifier>,
    pub location: Location,
}

impl NamespacePath {
    pub fn empty() -> Self {
        NamespacePath {
            path: Vec::new(),
            location: Location(0..0),
        }
    }

    pub fn new(name: &[&str]) -> Self {
        let path = name.iter().map(|&s| Identifier::new(s)).collect();

        NamespacePath {
            path,
            location: Location(0..0),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.path.is_empty()
    }
}

impl std::fmt::Display for NamespacePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let joined = self
            .path
            .iter()
            .map(|i| i.name.as_str())
            .collect::<Vec<&str>>()
            .join("::");

        f.write_str(&joined)
    }
}

impl std::hash::Hash for NamespacePath {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

impl PartialEq for NamespacePath {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq, Eq)]
pub struct Path {
    pub name: Identifier,
    pub root: NamespacePath,
    pub location: Location,
}

impl Path {
    pub fn rooted(name: Identifier) -> Self {
        let location = name.location.clone();

        Path {
            name,
            root: NamespacePath::empty(),
            location,
        }
    }

    pub fn merge(&mut self, other: Path) {
        self.root.path.extend(other.root.path);
        self.root.path.insert(0, other.name);
        self.location = (self.location.start()..other.location.end()).into();
    }
}

impl From<Identifier> for Path {
    fn from(identifier: Identifier) -> Path {
        Path::rooted(identifier)
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}::{}", self.root, self.name))
    }
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
    Namespace(Box<Namespace>),
    FunctionDefinition(Box<FunctionDefinition>),
    TypeDefinition(Box<TypeDefinition>),
    Use(Box<UseTrait>),
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Import {
    pub path: NamespacePath,
    pub names: Vec<Identifier>,
    pub location: Location,
}

impl Import {
    pub fn from_names(path: &[&'static str], names: &[&'static str]) -> Self {
        let path = NamespacePath::new(path);
        let names = names.iter().map(|p| Identifier::new(p)).collect();

        Self {
            path,
            names,
            location: Location(0..0),
        }
    }

    pub fn std(names: &[&'static str]) -> Self {
        Self::from_names(&["std"], names)
    }

    pub fn flatten(self) -> Vec<NamespacePath> {
        self.names
            .iter()
            .map(|n| {
                let mut path = self.path.path.clone();
                path.push(n.clone());

                NamespacePath {
                    path,
                    location: self.location.clone(),
                }
            })
            .collect::<Vec<_>>()
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Namespace {
    pub path: NamespacePath,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub visibility: Visibility,
    pub external: bool,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub type_parameters: Vec<TypeParameter>,
    pub return_type: Option<Box<Type>>,
    pub block: Block,
    pub location: Location,
    pub documentation: Option<String>,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    Class(Box<ClassDefinition>),
    Trait(Box<TraitDefinition>),
    Enum(Box<EnumDefinition>),
    Alias(Box<AliasDefinition>),
}

impl TypeDefinition {
    pub fn name(&self) -> &Identifier {
        match self {
            TypeDefinition::Class(class) => &class.name,
            TypeDefinition::Trait(trait_def) => &trait_def.name,
            TypeDefinition::Enum(enum_def) => &enum_def.name,
            TypeDefinition::Alias(alias_def) => &alias_def.name,
        }
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct ClassDefinition {
    pub name: Identifier,
    pub builtin: bool,
    pub members: Vec<ClassMember>,
    pub type_parameters: Vec<TypeParameter>,
    pub location: Location,
    pub documentation: Option<String>,
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
    pub property_type: Type,
    pub default_value: Option<Expression>,
    pub location: Location,
    pub documentation: Option<String>,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct MethodDefinition {
    pub visibility: Visibility,
    pub external: bool,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub type_parameters: Vec<TypeParameter>,
    pub return_type: Option<Box<Type>>,
    pub block: Block,
    pub location: Location,
    pub documentation: Option<String>,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct TraitDefinition {
    pub name: Identifier,
    pub type_parameters: Vec<TypeParameter>,
    pub methods: Vec<TraitMethodDefinition>,
    pub location: Location,
    pub documentation: Option<String>,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct TraitMethodDefinition {
    pub visibility: Visibility,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub type_parameters: Vec<TypeParameter>,
    pub return_type: Option<Box<Type>>,
    pub block: Option<Block>,
    pub location: Location,
    pub documentation: Option<String>,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct EnumDefinition {
    pub name: Identifier,
    pub cases: Vec<EnumDefinitionCase>,
    pub location: Location,
    pub documentation: Option<String>,
}

impl std::fmt::Display for EnumDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name.to_string())
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct EnumDefinitionCase {
    pub name: Identifier,
    pub parameters: Vec<Box<Type>>,
    pub location: Location,
    pub documentation: Option<String>,
}

impl std::fmt::Display for EnumDefinitionCase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name.to_string())
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct AliasDefinition {
    pub name: Identifier,
    pub definition: Box<Type>,
    pub location: Location,
    pub documentation: Option<String>,
}

impl std::fmt::Display for AliasDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name.to_string())
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct UseTrait {
    pub name: Box<Type>,
    pub target: Box<Type>,
    pub methods: Vec<TraitMethodImplementation>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct TraitMethodImplementation {
    pub visibility: Visibility,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub type_parameters: Vec<TypeParameter>,
    pub return_type: Option<Box<Type>>,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum Statement {
    VariableDeclaration(Box<VariableDeclaration>),
    Break(Box<Break>),
    Continue(Box<Continue>),
    Return(Box<Return>),
    If(Box<IfCondition>),
    Unless(Box<UnlessCondition>),
    InfiniteLoop(Box<InfiniteLoop>),
    IteratorLoop(Box<IteratorLoop>),
    PredicateLoop(Box<PredicateLoop>),
    Expression(Box<Expression>),
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub variable_type: Option<Type>,
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
    pub value: Option<Expression>,
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
pub struct InfiniteLoop {
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct IteratorLoop {
    pub pattern: Identifier,
    pub collection: Expression,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct PredicateLoop {
    pub condition: Expression,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum Expression {
    Array(Box<Array>),
    Assignment(Box<Assignment>),
    Call(Box<Call>),
    Literal(Box<Literal>),
    Member(Box<Member>),
    Range(Box<Range>),
    Variable(Box<Variable>),
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Array {
    pub values: Vec<Expression>,
    pub location: Location,
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
    pub name: Path,
    pub arguments: Vec<Expression>,
    pub type_arguments: Vec<TypeArgument>,
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
    pub kind: IntKind,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum IntKind {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct FloatLiteral {
    pub value: f64,
    pub location: Location,
    pub kind: FloatKind,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum FloatKind {
    F32,
    F64,
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
    pub name: Identifier,
}

impl Node for Variable {
    fn location(&self) -> &Location {
        &self.name.location
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct TypeParameter {
    pub name: Identifier,
    pub constraints: Vec<Box<Type>>,
}

impl Node for TypeParameter {
    fn location(&self) -> &Location {
        &self.name.location
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct TypeArgument {
    pub ty: Type,
}

impl Node for TypeArgument {
    fn location(&self) -> &Location {
        self.ty.location()
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum Type {
    Scalar(Box<ScalarType>),
    Array(Box<ArrayType>),
    Generic(Box<GenericType>),
    SelfType(Box<SelfType>),
}

impl Type {
    /// Checks whether the current type is a `self` type.
    pub fn is_self(&self) -> bool {
        matches!(self, Type::SelfType(_))
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Scalar(t) => f.write_fmt(format_args!("{}", t)),
            Type::Array(t) => f.write_fmt(format_args!("{}", t)),
            Type::Generic(t) => f.write_fmt(format_args!("{}", t)),
            Type::SelfType(t) => f.write_fmt(format_args!("{}", t)),
        }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct ScalarType {
    pub name: Path,
}

impl Node for ScalarType {
    fn location(&self) -> &Location {
        &self.name.location
    }
}

impl std::fmt::Display for ScalarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.name))
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub element_type: Box<Type>,
    pub location: Location,
}

impl std::fmt::Display for ArrayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("[{}]", self.element_type))
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct GenericType {
    pub name: Path,
    pub type_params: Vec<Box<Type>>,
    pub location: Location,
}

impl std::fmt::Display for GenericType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.name))?;

        if !self.type_params.is_empty() {
            f.write_str("<")?;

            for type_param in &self.type_params {
                f.write_fmt(format_args!("{}", type_param))?;
            }

            f.write_str(">")?;
        }

        Ok(())
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct SelfType {
    pub location: Location,
}

impl std::fmt::Display for SelfType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("self")
    }
}
