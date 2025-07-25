#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Location(pub std::ops::Range<usize>);

macro_rules! node_location {
    ($name:ident) => {
        impl Node for $name {
            #[inline]
            fn location(&self) -> &Location {
                &self.location
            }
        }
    };
}

impl Location {
    #[inline]
    #[must_use]
    pub fn start(&self) -> usize {
        self.0.start
    }

    #[inline]
    #[must_use]
    pub fn end(&self) -> usize {
        self.0.end
    }

    #[inline]
    #[must_use]
    pub fn len(&self) -> usize {
        self.0.end - self.0.start
    }

    #[inline]
    #[must_use]
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

#[derive(Debug, Clone, Eq)]
pub struct Identifier {
    pub name: String,
    pub location: Location,
}

node_location!(Identifier);

impl Identifier {
    #[must_use]
    pub fn new(name: &str) -> Self {
        Identifier {
            name: name.to_string(),
            location: Location(0..0),
        }
    }

    #[must_use]
    #[inline]
    pub fn as_var(self) -> Expression {
        Expression::Variable(Box::new(Variable { name: self }))
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn is_lower(&self) -> bool {
        self.name.starts_with(|c: char| c.is_ascii_lowercase())
    }

    #[inline]
    pub fn is_all_upper(&self) -> bool {
        self.name.chars().all(|c: char| c == '_' || c.is_ascii_uppercase())
    }
}

impl From<String> for Identifier {
    fn from(value: String) -> Self {
        Self {
            name: value,
            location: Location(0..0),
        }
    }
}

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Self {
            name: value.to_owned(),
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

#[derive(Debug, Clone, Eq)]
pub struct ImportPath {
    pub path: Vec<Identifier>,
    pub location: Location,
}

node_location!(ImportPath);

impl ImportPath {
    #[must_use]
    pub fn empty() -> Self {
        ImportPath {
            path: Vec::new(),
            location: Location(0..0),
        }
    }

    #[must_use]
    pub fn new(name: &[&str]) -> Self {
        let path = name.iter().map(|&s| Identifier::new(s)).collect();

        ImportPath {
            path,
            location: Location(0..0),
        }
    }

    #[inline]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.path.is_empty()
    }
}

impl std::fmt::Display for ImportPath {
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

impl std::hash::Hash for ImportPath {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

impl PartialEq for ImportPath {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

#[derive(Debug, Clone, Eq)]
pub struct NamespacePath {
    pub path: Vec<Identifier>,
    pub location: Location,
}

node_location!(NamespacePath);

impl NamespacePath {
    #[must_use]
    pub fn empty() -> Self {
        NamespacePath {
            path: Vec::new(),
            location: Location(0..0),
        }
    }

    #[must_use]
    pub fn new(name: &[&str]) -> Self {
        let path = name.iter().map(|&s| Identifier::new(s)).collect();

        NamespacePath {
            path,
            location: Location(0..0),
        }
    }

    #[inline]
    #[must_use]
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

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum PathSegment {
    /// Denotes a segment which refers to a namespace.
    ///
    /// ```lm
    /// std::io::File
    /// ^^^  ^^ both namespace segments
    /// ```
    Namespace { name: Identifier },

    /// Denotes a segment which refers to a type, optionally with type arguments.
    ///
    /// ```lm
    /// std::io::File
    ///          ^^^^ type segment
    /// ```
    Type {
        name: Identifier,
        type_arguments: Vec<Type>,
        location: Location,
    },

    /// Denotes a segment which refers to a callable, such as a function or method.
    ///
    /// ```lm
    /// std::io::File::open()
    ///                ^^^^ callable segment
    ///
    /// std::io::read_file()
    ///          ^^^^^^^^^ callable segment
    /// ```
    Callable {
        name: Identifier,
        type_arguments: Vec<Type>,
        location: Location,
    },
}

impl PathSegment {
    /// Creates a new namespace segment, with the given name.
    pub fn namespace(identifier: impl Into<Identifier>) -> Self {
        Self::Namespace {
            name: identifier.into(),
        }
    }

    /// Creates a new type segment, with the given name.
    pub fn ty(identifier: impl Into<Identifier>) -> Self {
        let identifier = identifier.into();

        Self::Type {
            location: identifier.location.clone(),
            name: identifier,
            type_arguments: Vec::new(),
        }
    }

    /// Creates a new callable segment, with the given name.
    pub fn callable(identifier: impl Into<Identifier>) -> Self {
        let identifier = identifier.into();

        Self::Callable {
            location: identifier.location.clone(),
            name: identifier,
            type_arguments: Vec::new(),
        }
    }

    /// Gets the name of the path segment.
    pub fn name(&self) -> &Identifier {
        match self {
            Self::Namespace { name } | Self::Type { name, .. } | Self::Callable { name, .. } => name,
        }
    }

    /// Gets the type arguments of the path segment.
    pub fn type_arguments(&self) -> &[Type] {
        match self {
            Self::Namespace { .. } => &[],
            Self::Type { type_arguments, .. } | Self::Callable { type_arguments, .. } => type_arguments.as_slice(),
        }
    }

    /// Takes the type arguments from the path segment.
    pub fn take_type_arguments(self) -> Vec<Type> {
        match self {
            Self::Namespace { .. } => Vec::new(),
            Self::Type { type_arguments, .. } | Self::Callable { type_arguments, .. } => type_arguments,
        }
    }
}

impl std::fmt::Display for PathSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Namespace { name } => f.write_str(name.as_str()),
            Self::Type {
                name, type_arguments, ..
            }
            | Self::Callable {
                name, type_arguments, ..
            } => {
                f.write_fmt(format_args!("{name}"))?;

                if !type_arguments.is_empty() {
                    write!(
                        f,
                        "<{}>",
                        type_arguments
                            .iter()
                            .map(std::string::ToString::to_string)
                            .collect::<Vec<String>>()
                            .join(", ")
                    )?;
                }

                Ok(())
            }
        }
    }
}

impl Node for PathSegment {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::Namespace { name } => &name.location,
            Self::Type { location, .. } | Self::Callable { location, .. } => location,
        }
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Path {
    pub root: Vec<PathSegment>,
    pub name: PathSegment,
    pub location: Location,
}

node_location!(Path);

impl Path {
    #[must_use]
    pub fn rooted(name: PathSegment) -> Self {
        let location = name.location().clone();

        Path {
            name,
            root: Vec::new(),
            location,
        }
    }

    #[must_use]
    pub fn with_root(namespace: Vec<impl Into<PathSegment>>, name: impl Into<PathSegment>) -> Self {
        let root = namespace.into_iter().map(Into::<PathSegment>::into).collect();

        Self {
            root,
            name: name.into(),
            location: Location(0..0),
        }
    }

    pub fn merge(&mut self, other: Path) {
        self.root.extend(other.root);
        self.root.insert(0, other.name);
        self.location = (other.location.start()..self.location.end()).into();
    }

    pub fn type_arguments(&self) -> &[Type] {
        self.name.type_arguments()
    }

    pub fn take_type_arguments(self) -> Vec<Type> {
        self.name.take_type_arguments()
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for segment in &self.root {
            f.write_fmt(format_args!("{segment}::"))?;
        }

        f.write_fmt(format_args!("{}", self.name))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub location: Location,
}

node_location!(Block);

impl Block {
    pub fn from_location(location: impl Into<Location>) -> Self {
        Self {
            statements: Vec::new(),
            location: location.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Identifier,
    pub param_type: Type,
    pub vararg: bool,
    pub location: Location,
}

node_location!(Parameter);

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public(Box<Public>),
    Private(Box<Private>),
}

impl Node for Visibility {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::Public(e) => &e.location,
            Self::Private(e) => &e.location,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Public {
    pub location: Location,
}

node_location!(Public);

#[derive(Debug, Clone, PartialEq)]
pub struct Private {
    pub location: Location,
}

node_location!(Private);

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelExpression {
    Import(Box<Import>),
    Namespace(Box<Namespace>),
    FunctionDefinition(Box<FunctionDefinition>),
    TypeDefinition(Box<TypeDefinition>),
    Impl(Box<Implementation>),
    Use(Box<UseTrait>),
}

impl Node for TopLevelExpression {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::Import(e) => &e.location,
            Self::Namespace(e) => &e.location,
            Self::FunctionDefinition(e) => &e.location,
            Self::TypeDefinition(e) => e.location(),
            Self::Impl(e) => &e.location,
            Self::Use(e) => &e.location,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub path: ImportPath,
    pub names: Vec<Identifier>,
    pub location: Location,
}

node_location!(Import);

impl Import {
    #[must_use]
    pub fn from_names(path: &[&'static str], names: &[&'static str]) -> Self {
        let path = ImportPath::new(path);
        let names = names.iter().map(|p| Identifier::new(p)).collect();

        Self {
            path,
            names,
            location: Location(0..0),
        }
    }

    #[must_use]
    pub fn std(names: &[&'static str]) -> Self {
        Self::from_names(&["std"], names)
    }

    #[must_use]
    pub fn flatten(self) -> Vec<ImportPath> {
        self.names
            .iter()
            .map(|n| {
                let mut path = self.path.path.clone();
                path.push(n.clone());

                ImportPath {
                    path,
                    location: self.location.clone(),
                }
            })
            .collect::<Vec<_>>()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Namespace {
    pub path: ImportPath,
    pub location: Location,
}

node_location!(Namespace);

#[derive(Debug, Clone, PartialEq)]
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

node_location!(FunctionDefinition);

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    Struct(Box<StructDefinition>),
    Trait(Box<TraitDefinition>),
    Enum(Box<EnumDefinition>),
}

impl Node for TypeDefinition {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::Struct(e) => &e.location,
            Self::Trait(e) => &e.location,
            Self::Enum(e) => &e.location,
        }
    }
}

impl TypeDefinition {
    #[inline]
    #[must_use]
    pub fn name(&self) -> &Identifier {
        match self {
            TypeDefinition::Struct(struct_def) => &struct_def.name,
            TypeDefinition::Trait(trait_def) => &trait_def.name,
            TypeDefinition::Enum(enum_def) => &enum_def.name,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefinition {
    pub visibility: Visibility,
    pub name: Identifier,
    pub builtin: bool,
    pub properties: Vec<Property>,
    pub type_parameters: Vec<TypeParameter>,
    pub location: Location,
    pub documentation: Option<String>,
}

node_location!(StructDefinition);

#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    pub visibility: Visibility,
    pub name: Identifier,
    pub property_type: Type,
    pub default_value: Option<Expression>,
    pub location: Location,
    pub documentation: Option<String>,
}

node_location!(Property);

#[derive(Debug, Clone, PartialEq)]
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

node_location!(MethodDefinition);

#[derive(Debug, Clone, PartialEq)]
pub struct TraitDefinition {
    pub visibility: Visibility,
    pub name: Identifier,
    pub type_parameters: Vec<TypeParameter>,
    pub methods: Vec<TraitMethodDefinition>,
    pub location: Location,
    pub documentation: Option<String>,
}

node_location!(TraitDefinition);

#[derive(Debug, Clone, PartialEq)]
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

node_location!(TraitMethodDefinition);

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDefinition {
    pub visibility: Visibility,
    pub name: Identifier,
    pub cases: Vec<EnumDefinitionCase>,
    pub location: Location,
    pub documentation: Option<String>,
}

node_location!(EnumDefinition);

impl std::fmt::Display for EnumDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name.to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDefinitionCase {
    pub name: Identifier,
    pub parameters: Vec<Box<Type>>,
    pub location: Location,
    pub documentation: Option<String>,
}

node_location!(EnumDefinitionCase);

impl std::fmt::Display for EnumDefinitionCase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name.to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Implementation {
    pub visibility: Visibility,
    pub name: Box<Type>,
    pub methods: Vec<MethodDefinition>,
    pub type_parameters: Vec<TypeParameter>,
    pub location: Location,
}

node_location!(Implementation);

#[derive(Debug, Clone, PartialEq)]
pub struct UseTrait {
    pub visibility: Visibility,
    pub name: Box<Type>,
    pub target: Box<Type>,
    pub methods: Vec<TraitMethodImplementation>,
    pub type_parameters: Vec<TypeParameter>,
    pub location: Location,
}

node_location!(UseTrait);

#[derive(Debug, Clone, PartialEq)]
pub struct TraitMethodImplementation {
    pub visibility: Visibility,
    pub external: bool,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub type_parameters: Vec<TypeParameter>,
    pub return_type: Option<Box<Type>>,
    pub block: Block,
    pub location: Location,
}

node_location!(TraitMethodImplementation);

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    VariableDeclaration(Box<VariableDeclaration>),
    Break(Box<Break>),
    Continue(Box<Continue>),
    Return(Box<Return>),
    If(Box<IfCondition>),
    InfiniteLoop(Box<InfiniteLoop>),
    IteratorLoop(Box<IteratorLoop>),
    PredicateLoop(Box<PredicateLoop>),
    Expression(Box<Expression>),
}

impl Node for Statement {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::VariableDeclaration(e) => &e.location,
            Self::Break(e) => &e.location,
            Self::Continue(e) => &e.location,
            Self::Return(e) => &e.location,
            Self::If(e) => &e.location,
            Self::InfiniteLoop(e) => &e.location,
            Self::IteratorLoop(e) => &e.location,
            Self::PredicateLoop(e) => &e.location,
            Self::Expression(e) => e.location(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub variable_type: Option<Type>,
    pub value: Expression,
    pub location: Location,
}

node_location!(VariableDeclaration);

#[derive(Debug, Clone, PartialEq)]
pub struct Break {
    pub location: Location,
}

node_location!(Break);

#[derive(Debug, Clone, PartialEq)]
pub struct Continue {
    pub location: Location,
}

node_location!(Continue);

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub value: Option<Expression>,
    pub location: Location,
}

node_location!(Return);

#[derive(Debug, Clone, PartialEq)]
pub struct IfCondition {
    pub cases: Vec<Condition>,
    pub location: Location,
}

node_location!(IfCondition);

#[derive(Debug, Clone, PartialEq)]
pub struct Condition {
    pub condition: Option<Expression>,
    pub block: Block,
    pub location: Location,
}

node_location!(Condition);

#[derive(Debug, Clone, PartialEq)]
pub struct InfiniteLoop {
    pub block: Block,
    pub location: Location,
}

node_location!(InfiniteLoop);

#[derive(Debug, Clone, PartialEq)]
pub struct IteratorLoop {
    pub pattern: Identifier,
    pub collection: Expression,
    pub block: Block,
    pub location: Location,
}

node_location!(IteratorLoop);

#[derive(Debug, Clone, PartialEq)]
pub struct PredicateLoop {
    pub condition: Expression,
    pub block: Block,
    pub location: Location,
}

node_location!(PredicateLoop);

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Array(Box<Array>),
    Assignment(Box<Assignment>),
    Binary(Box<Binary>),
    Call(Box<Call>),
    Cast(Box<Cast>),
    Construct(Box<Construct>),
    IntrinsicCall(Box<IntrinsicCall>),
    Literal(Box<Literal>),
    Logical(Box<Logical>),
    Member(Box<Member>),
    Range(Box<Range>),
    Variable(Box<Variable>),
    Variant(Box<Variant>),
}

impl Node for Expression {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::Array(e) => &e.location,
            Self::Assignment(e) => &e.location,
            Self::Binary(e) => &e.location,
            Self::Call(e) => &e.location,
            Self::Cast(e) => &e.location,
            Self::Construct(e) => &e.location,
            Self::IntrinsicCall(e) => e.location(),
            Self::Literal(e) => e.location(),
            Self::Logical(e) => &e.location,
            Self::Member(e) => &e.location,
            Self::Range(e) => &e.location,
            Self::Variable(e) => e.location(),
            Self::Variant(e) => e.location(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    pub values: Vec<Expression>,
    pub location: Location,
}

node_location!(Array);

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub target: Expression,
    pub value: Expression,
    pub location: Location,
}

node_location!(Assignment);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperatorKind {
    And,
    Or,
    Xor,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperator {
    pub kind: BinaryOperatorKind,
    pub location: Location,
}

node_location!(BinaryOperator);

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub lhs: Expression,
    pub op: BinaryOperator,
    pub rhs: Expression,
    pub location: Location,
}

node_location!(Binary);

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callee: Option<Expression>,
    pub name: Path,
    pub arguments: Vec<Expression>,
    pub location: Location,
}

node_location!(Call);

#[derive(Debug, Clone, PartialEq)]
pub struct IntrinsicCall {
    pub callee: Expression,
    pub name: Path,
    pub arguments: Vec<Expression>,
    pub location: Location,
}

node_location!(IntrinsicCall);

#[derive(Debug, Clone, PartialEq)]
pub struct Cast {
    pub source: Expression,
    pub target_type: Type,
    pub location: Location,
}

node_location!(Cast);

#[derive(Debug, Clone, PartialEq)]
pub struct Construct {
    pub path: Path,
    pub fields: Vec<Field>,
    pub location: Location,
}

node_location!(Construct);

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: Identifier,
    pub value: Expression,
    pub location: Location,
}

node_location!(Field);

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(Box<IntLiteral>),
    Float(Box<FloatLiteral>),
    String(Box<StringLiteral>),
    Boolean(Box<BooleanLiteral>),
}

impl Node for Literal {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::Int(e) => &e.location,
            Self::Float(e) => &e.location,
            Self::String(e) => &e.location,
            Self::Boolean(e) => &e.location,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntLiteral {
    pub value: i64,
    pub location: Location,
    pub kind: IntKind,
}

node_location!(IntLiteral);

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct FloatLiteral {
    pub value: f64,
    pub location: Location,
    pub kind: FloatKind,
}

node_location!(FloatLiteral);

#[derive(Debug, Clone, PartialEq)]
pub enum FloatKind {
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub value: String,
    pub location: Location,
}

node_location!(StringLiteral);

#[derive(Debug, Clone, PartialEq)]
pub struct BooleanLiteral {
    pub value: bool,
    pub location: Location,
}

node_location!(BooleanLiteral);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LogicalOperatorKind {
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LogicalOperator {
    pub kind: LogicalOperatorKind,
    pub location: Location,
}

node_location!(LogicalOperator);

#[derive(Debug, Clone, PartialEq)]
pub struct Logical {
    pub lhs: Expression,
    pub op: LogicalOperator,
    pub rhs: Expression,
    pub location: Location,
}

node_location!(Logical);

#[derive(Debug, Clone, PartialEq)]
pub struct Member {
    pub callee: Expression,
    pub name: String,
    pub location: Location,
}

node_location!(Member);

#[derive(Debug, Clone, PartialEq)]
pub struct Range {
    pub lower: Expression,
    pub upper: Expression,
    pub inclusive: bool,
    pub location: Location,
}

node_location!(Range);

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: Identifier,
}

impl Node for Variable {
    fn location(&self) -> &Location {
        &self.name.location
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variant {
    pub name: Path,
}

impl Node for Variant {
    fn location(&self) -> &Location {
        &self.name.location
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParameter {
    pub name: Identifier,
    pub constraints: Vec<Box<Type>>,
}

impl Node for TypeParameter {
    fn location(&self) -> &Location {
        &self.name.location
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Named(Box<NamedType>),
    Array(Box<ArrayType>),
    SelfType(Box<SelfType>),
}

impl Node for Type {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::Named(e) => e.location(),
            Self::Array(e) => &e.location,
            Self::SelfType(e) => &e.location,
        }
    }
}

impl Type {
    /// Checks whether the current type is a `self` type.
    #[inline]
    #[must_use]
    pub fn is_self(&self) -> bool {
        matches!(self, Type::SelfType(_))
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Named(t) => f.write_fmt(format_args!("{t}")),
            Type::Array(t) => f.write_fmt(format_args!("{t}")),
            Type::SelfType(t) => f.write_fmt(format_args!("{t}")),
        }
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct NamedType {
    pub name: Path,
}

impl Node for NamedType {
    fn location(&self) -> &Location {
        &self.name.location
    }
}

impl std::fmt::Display for NamedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.name))?;

        if !self.name.name.type_arguments().is_empty() {
            f.write_str("<")?;

            for type_param in self.name.name.type_arguments() {
                f.write_fmt(format_args!("{type_param}"))?;
            }

            f.write_str(">")?;
        }

        Ok(())
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ArrayType {
    pub element_type: Box<Type>,
    pub location: Location,
}

node_location!(ArrayType);

impl std::fmt::Display for ArrayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("[{}]", self.element_type))
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct SelfType {
    pub location: Location,
}

node_location!(SelfType);

impl std::fmt::Display for SelfType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("self")
    }
}
