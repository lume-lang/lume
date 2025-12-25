use std::hash::Hash;

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

    /// Denotes a segment which refers to a type, optionally with type
    /// arguments.
    ///
    /// ```lm
    /// std::io::File
    ///          ^^^^ type segment
    /// ```
    Type {
        name: Identifier,
        bound_types: Vec<Type>,
        location: Location,
    },

    /// Denotes a segment which refers to a callable, such as a function or
    /// method.
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
        bound_types: Vec<Type>,
        location: Location,
    },

    /// Denotes a segment which refers to an enum variant, with or without
    /// parameters.
    ///
    /// ```lm
    /// Option::None
    ///         ^^^^ variant segment
    ///
    /// Option::Some(false)
    ///         ^^^^^^^^^^^ variant segment
    /// ```
    Variant { name: Identifier, location: Location },
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
            bound_types: Vec::new(),
        }
    }

    /// Creates a new callable segment, with the given name.
    pub fn callable(identifier: impl Into<Identifier>) -> Self {
        let identifier = identifier.into();

        Self::Callable {
            location: identifier.location.clone(),
            name: identifier,
            bound_types: Vec::new(),
        }
    }

    /// Gets the name of the path segment.
    pub fn name(&self) -> &Identifier {
        match self {
            Self::Namespace { name }
            | Self::Type { name, .. }
            | Self::Callable { name, .. }
            | Self::Variant { name, .. } => name,
        }
    }

    /// Gets the bound types of the path segment.
    pub fn bound_types(&self) -> &[Type] {
        match self {
            Self::Namespace { .. } | Self::Variant { .. } => &[],
            Self::Type { bound_types, .. } | Self::Callable { bound_types, .. } => bound_types.as_slice(),
        }
    }

    /// Takes the bound types from the path segment.
    pub fn take_bound_types(&mut self) -> Vec<Type> {
        match self {
            Self::Namespace { .. } | Self::Variant { .. } => Vec::new(),
            Self::Type { bound_types, .. } | Self::Callable { bound_types, .. } => std::mem::take(bound_types),
        }
    }
}

impl std::fmt::Display for PathSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Namespace { name } => f.write_str(name.as_str()),
            Self::Type { name, bound_types, .. } | Self::Callable { name, bound_types, .. } => {
                f.write_fmt(format_args!("{name}"))?;

                if !bound_types.is_empty() {
                    write!(
                        f,
                        "<{}>",
                        bound_types
                            .iter()
                            .map(std::string::ToString::to_string)
                            .collect::<Vec<String>>()
                            .join(", ")
                    )?;
                }

                Ok(())
            }
            Self::Variant { name, .. } => f.write_fmt(format_args!("{name}")),
        }
    }
}

impl Node for PathSegment {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::Namespace { name } => &name.location,
            Self::Type { location, .. } | Self::Callable { location, .. } | Self::Variant { location, .. } => location,
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
        let name: PathSegment = name.into();
        let root: Vec<PathSegment> = namespace.into_iter().map(Into::<PathSegment>::into).collect();

        let start = root.first().map_or(name.location().start(), |s| s.location().start());
        let end = name.location().end();

        Self {
            root,
            name,
            location: Location(start..end),
        }
    }

    pub fn merge(&mut self, other: Path) {
        self.root.extend(other.root);
        self.root.insert(0, other.name);
        self.location = (other.location.start()..self.location.end()).into();
    }

    pub fn bound_types(&self) -> &[Type] {
        self.name.bound_types()
    }

    pub fn take_bound_types(&mut self) -> Vec<Type> {
        self.name.take_bound_types()
    }

    pub fn is_variant(&self) -> bool {
        matches!(self.name, PathSegment::Variant { .. })
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

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
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
pub struct Attribute {
    pub name: Identifier,
    pub arguments: Vec<AttributeArgument>,
    pub location: Location,
}

node_location!(Attribute);

#[derive(Debug, Clone, PartialEq)]
pub struct AttributeArgument {
    pub key: Identifier,
    pub value: Literal,
    pub location: Location,
}

node_location!(AttributeArgument);

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Identifier,
    pub param_type: Type,
    pub vararg: bool,
    pub location: Location,
}

node_location!(Parameter);

impl Parameter {
    /// Checks whether the current parameter is `self`.
    #[inline]
    #[must_use]
    pub fn is_self(&self) -> bool {
        self.name.as_str() == "self" && self.param_type.is_self()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public { location: Location },
    Internal { location: Location },
    Private { location: Location },
}

impl Node for Visibility {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::Public { location } | Self::Internal { location } | Self::Private { location } => location,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Public {
    pub location: Location,
}

node_location!(Public);

#[derive(Debug, Clone, PartialEq)]
pub struct PublicInternal {
    pub location: Location,
}

node_location!(PublicInternal);

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
    TraitImpl(Box<TraitImplementation>),
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
            Self::TraitImpl(e) => &e.location,
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
    pub attributes: Vec<Attribute>,
    pub visibility: Option<Visibility>,
    pub external: bool,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub type_parameters: Vec<TypeParameter>,
    pub return_type: Option<Box<Type>>,
    pub block: Option<Block>,
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
    pub attributes: Vec<Attribute>,
    pub visibility: Option<Visibility>,
    pub name: Identifier,
    pub builtin: bool,
    pub fields: Vec<Field>,
    pub type_parameters: Vec<TypeParameter>,
    pub location: Location,
    pub documentation: Option<String>,
}

node_location!(StructDefinition);

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub visibility: Option<Visibility>,
    pub name: Identifier,
    pub field_type: Type,
    pub default_value: Option<Expression>,
    pub location: Location,
    pub documentation: Option<String>,
}

node_location!(Field);

#[derive(Debug, Clone, PartialEq)]
pub struct MethodDefinition {
    pub attributes: Vec<Attribute>,
    pub visibility: Option<Visibility>,
    pub external: bool,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub type_parameters: Vec<TypeParameter>,
    pub return_type: Option<Box<Type>>,
    pub block: Option<Block>,
    pub location: Location,
    pub documentation: Option<String>,
}

node_location!(MethodDefinition);

#[derive(Debug, Clone, PartialEq)]
pub struct TraitDefinition {
    pub attributes: Vec<Attribute>,
    pub visibility: Option<Visibility>,
    pub name: Identifier,
    pub type_parameters: Vec<TypeParameter>,
    pub methods: Vec<TraitMethodDefinition>,
    pub location: Location,
    pub documentation: Option<String>,
}

node_location!(TraitDefinition);

#[derive(Debug, Clone, PartialEq)]
pub struct TraitMethodDefinition {
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
    pub visibility: Option<Visibility>,
    pub name: Identifier,
    pub type_parameters: Vec<TypeParameter>,
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
    pub name: Box<Type>,
    pub methods: Vec<MethodDefinition>,
    pub type_parameters: Vec<TypeParameter>,
    pub location: Location,
}

node_location!(Implementation);

#[derive(Debug, Clone, PartialEq)]
pub struct TraitImplementation {
    pub name: Box<Type>,
    pub target: Box<Type>,
    pub methods: Vec<TraitMethodImplementation>,
    pub type_parameters: Vec<TypeParameter>,
    pub location: Location,
}

node_location!(TraitImplementation);

#[derive(Debug, Clone, PartialEq)]
pub struct TraitMethodImplementation {
    pub external: bool,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub type_parameters: Vec<TypeParameter>,
    pub return_type: Option<Box<Type>>,
    pub block: Option<Block>,
    pub location: Location,
}

node_location!(TraitMethodImplementation);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    VariableDeclaration(Box<VariableDeclaration>),
    Break(Box<Break>),
    Continue(Box<Continue>),
    Final(Box<Final>),
    Return(Box<Return>),
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
            Self::Final(e) => e.location(),
            Self::Return(e) => &e.location,
            Self::InfiniteLoop(e) => &e.location,
            Self::IteratorLoop(e) => &e.location,
            Self::PredicateLoop(e) => &e.location,
            Self::Expression(e) => e.location(),
        }
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub variable_type: Option<Type>,
    pub value: Expression,
    pub location: Location,
}

node_location!(VariableDeclaration);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Break {
    pub location: Location,
}

node_location!(Break);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Continue {
    pub location: Location,
}

node_location!(Continue);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Final {
    pub value: Expression,
}

impl Node for Final {
    fn location(&self) -> &Location {
        self.value.location()
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Return {
    pub value: Option<Expression>,
    pub location: Location,
}

node_location!(Return);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct IfCondition {
    pub cases: Vec<Condition>,
    pub location: Location,
}

node_location!(IfCondition);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Condition {
    pub condition: Option<Expression>,
    pub block: Block,
    pub location: Location,
}

node_location!(Condition);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct InfiniteLoop {
    pub block: Block,
    pub location: Location,
}

node_location!(InfiniteLoop);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct IteratorLoop {
    pub pattern: Identifier,
    pub collection: Expression,
    pub block: Block,
    pub location: Location,
}

node_location!(IteratorLoop);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct PredicateLoop {
    pub condition: Expression,
    pub block: Block,
    pub location: Location,
}

node_location!(PredicateLoop);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Array(Box<Array>),
    Assignment(Box<Assignment>),
    Call(Box<Call>),
    Cast(Box<Cast>),
    Construct(Box<Construct>),
    If(Box<IfCondition>),
    IntrinsicCall(Box<IntrinsicCall>),
    Is(Box<Is>),
    Literal(Box<Literal>),
    Member(Box<Member>),
    Range(Box<Range>),
    Scope(Box<Scope>),
    Switch(Box<Switch>),
    Variable(Box<Variable>),
    Variant(Box<Variant>),
}

impl Node for Expression {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::Array(e) => &e.location,
            Self::Assignment(e) => &e.location,
            Self::Call(e) => &e.location,
            Self::Cast(e) => &e.location,
            Self::Construct(e) => &e.location,
            Self::If(e) => &e.location,
            Self::IntrinsicCall(e) => e.location(),
            Self::Is(e) => &e.location,
            Self::Literal(e) => e.location(),
            Self::Member(e) => &e.location,
            Self::Range(e) => &e.location,
            Self::Scope(e) => &e.location,
            Self::Switch(e) => &e.location,
            Self::Variable(e) => e.location(),
            Self::Variant(e) => e.location(),
        }
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Array {
    pub values: Vec<Expression>,
    pub location: Location,
}

node_location!(Array);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Assignment {
    pub target: Expression,
    pub value: Expression,
    pub location: Location,
}

node_location!(Assignment);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Call {
    pub callee: Option<Expression>,
    pub name: Path,
    pub arguments: Vec<Expression>,
    pub location: Location,
}

node_location!(Call);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct IntrinsicCall {
    pub kind: IntrinsicKind,
    pub location: Location,
}

node_location!(IntrinsicCall);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum IntrinsicKind {
    // Arithmetic intrinsics
    Add { lhs: Box<Expression>, rhs: Box<Expression> },
    Sub { lhs: Box<Expression>, rhs: Box<Expression> },
    Mul { lhs: Box<Expression>, rhs: Box<Expression> },
    Div { lhs: Box<Expression>, rhs: Box<Expression> },
    And { lhs: Box<Expression>, rhs: Box<Expression> },
    Or { lhs: Box<Expression>, rhs: Box<Expression> },
    Negate { target: Box<Expression> },
    Increment { target: Box<Expression> },
    Decrement { target: Box<Expression> },

    // Logical intrinsics
    BinaryAnd { lhs: Box<Expression>, rhs: Box<Expression> },
    BinaryOr { lhs: Box<Expression>, rhs: Box<Expression> },
    BinaryXor { lhs: Box<Expression>, rhs: Box<Expression> },
    Not { target: Box<Expression> },

    // Comparison intrinsics
    Equal { lhs: Box<Expression>, rhs: Box<Expression> },
    NotEqual { lhs: Box<Expression>, rhs: Box<Expression> },
    Less { lhs: Box<Expression>, rhs: Box<Expression> },
    LessEqual { lhs: Box<Expression>, rhs: Box<Expression> },
    Greater { lhs: Box<Expression>, rhs: Box<Expression> },
    GreaterEqual { lhs: Box<Expression>, rhs: Box<Expression> },
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Cast {
    pub source: Expression,
    pub target_type: Type,
    pub location: Location,
}

node_location!(Cast);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Construct {
    pub path: Path,
    pub fields: Vec<ConstructorField>,
    pub location: Location,
}

node_location!(Construct);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ConstructorField {
    pub name: Identifier,
    pub value: Option<Expression>,
    pub location: Location,
}

node_location!(ConstructorField);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Is {
    pub target: Expression,
    pub pattern: Pattern,
    pub location: Location,
}

node_location!(Is);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
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

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct IntLiteral {
    pub value: i64,
    pub location: Location,
    pub kind: Option<IntKind>,
}

node_location!(IntLiteral);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
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
    pub kind: Option<FloatKind>,
}

node_location!(FloatLiteral);

impl Hash for FloatLiteral {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.to_bits().hash(state);
        self.location.hash(state);
        self.kind.hash(state);
    }
}

impl Eq for FloatLiteral {}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum FloatKind {
    F32,
    F64,
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct StringLiteral {
    pub value: String,
    pub location: Location,
}

node_location!(StringLiteral);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct BooleanLiteral {
    pub value: bool,
    pub location: Location,
}

node_location!(BooleanLiteral);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Member {
    pub callee: Expression,
    pub name: Identifier,
    pub location: Location,
}

node_location!(Member);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Range {
    pub lower: Expression,
    pub upper: Expression,
    pub inclusive: bool,
    pub location: Location,
}

node_location!(Range);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    pub body: Vec<Statement>,
    pub location: Location,
}

node_location!(Scope);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Switch {
    pub operand: Expression,
    pub cases: Vec<SwitchCase>,
    pub location: Location,
}

node_location!(Switch);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct SwitchCase {
    pub pattern: Pattern,
    pub branch: Expression,
    pub location: Location,
}

node_location!(SwitchCase);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Variable {
    pub name: Identifier,
}

impl Node for Variable {
    fn location(&self) -> &Location {
        &self.name.location
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Variant {
    pub name: Path,
    pub arguments: Vec<Expression>,
    pub location: Location,
}

node_location!(Variant);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Literal(Literal),
    Identifier(Identifier),
    Variant(VariantPattern),
    Wildcard(WildcardPattern),
}

impl Node for Pattern {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::Literal(p) => p.location(),
            Self::Identifier(p) => &p.location,
            Self::Variant(p) => p.location(),
            Self::Wildcard(p) => &p.location,
        }
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct VariantPattern {
    pub name: Path,
    pub fields: Vec<Pattern>,
    pub location: Location,
}

node_location!(VariantPattern);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct WildcardPattern {
    pub location: Location,
}

node_location!(WildcardPattern);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
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
            Type::Named(t) => std::fmt::Display::fmt(t, f),
            Type::Array(t) => std::fmt::Display::fmt(t, f),
            Type::SelfType(t) => std::fmt::Display::fmt(t, f),
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

        if !self.name.name.bound_types().is_empty() {
            f.write_str("<")?;

            for type_param in self.name.name.bound_types() {
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
