use std::hash::Hash;

#[macro_use]
pub mod macros;
pub mod support;

pub mod generated {
    pub mod ast;
}

pub use generated::ast::*;

#[cfg(test)]
mod tests;

pub trait AstNode {
    /// Attempts to cast the given syntax node into the current node type.
    ///
    /// If the conversion fails, [`None`] is returned.
    fn cast(syntax: lume_syntax::SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    /// Gets the underlying [`lume_syntax::SyntaxNode`] for this AST node.
    fn syntax(&self) -> &lume_syntax::SyntaxNode;

    /// Gets the source text which this node encapsulates.
    fn as_text(&self) -> String {
        self.syntax().text().to_string()
    }

    /// Gets the text range of this node in the source file.
    fn range(&self) -> std::ops::Range<usize> {
        let text_range = self.syntax().text_range();

        text_range.start().into()..text_range.end().into()
    }

    /// Gets the location of this node in the source file.
    #[inline]
    fn location(&self) -> Location {
        Location(self.range())
    }

    /// Returns an independent copy of the subtree rooted at this node.
    ///
    /// The parent of the returned node will be [`None`], the start offset will
    /// be zero, but, otherwise, it'll be equivalent to the source node.
    fn clone_subtree(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_subtree()).unwrap()
    }
}

/// An iterator over `SyntaxNode` children of a particular AST type.
#[derive(Debug, Clone)]
pub struct AstChildren<N> {
    inner: lume_syntax::SyntaxNodeChildren,
    _data: std::marker::PhantomData<N>,
}

impl<N> AstChildren<N> {
    fn new(parent: &lume_syntax::SyntaxNode) -> Self {
        AstChildren {
            inner: parent.children(),
            _data: std::marker::PhantomData,
        }
    }
}

impl<N: AstNode> Iterator for AstChildren<N> {
    type Item = N;

    fn next(&mut self) -> Option<N> {
        self.inner.find_map(N::cast)
    }
}

/*
#[derive(Debug, Clone)]
pub struct SyntaxTree<'ast> {
    pub items: Vec<Item<'ast>>,
    pub arena: &'ast lume_data_structures::UntypedArena,
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Location(pub std::ops::Range<usize>);

macro_rules! node_location {
    ($name:ty) => {
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
pub struct Identifier<'ast> {
    pub name: &'ast str,
    pub location: Location,
}

node_location!(Identifier<'_>);

impl<'ast> Identifier<'ast> {
    #[must_use]
    pub fn new(tree: &'ast SyntaxTree, name: &str) -> Self {
        Identifier {
            name: tree.arena.alloc_str(name),
            location: Location(0..0),
        }
    }

    #[must_use]
    #[inline]
    pub fn as_var(self) -> Expression<'ast> {
        Expression::Variable(Box::new(Variable { name: self }))
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        self.name
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

impl<'ast> From<&'ast str> for Identifier<'ast> {
    fn from(name: &'ast str) -> Self {
        Identifier {
            name,
            location: Location(0..0),
        }
    }
}

impl std::fmt::Display for Identifier<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name)
    }
}

impl std::hash::Hash for Identifier<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl PartialEq for Identifier<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Debug, Clone, Eq)]
pub struct ImportPath<'ast> {
    pub path: Vec<Identifier<'ast>>,
    pub location: Location,
}

node_location!(ImportPath<'_>);

impl<'ast> ImportPath<'ast> {
    #[must_use]
    pub fn empty() -> Self {
        ImportPath {
            path: Vec::new(),
            location: Location(0..0),
        }
    }

    #[must_use]
    pub fn new(tree: &'ast SyntaxTree, name: &[&str]) -> Self {
        let path = name.iter().map(|&s| Identifier::new(tree, s)).collect();

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

impl std::fmt::Display for ImportPath<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let joined = self.path.iter().map(|i| i.name).collect::<Vec<&str>>().join("::");

        f.write_str(&joined)
    }
}

impl std::hash::Hash for ImportPath<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

impl PartialEq for ImportPath<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

#[derive(Debug, Clone, Eq)]
pub struct NamespacePath<'ast> {
    pub path: Vec<Identifier<'ast>>,
    pub location: Location,
}

node_location!(NamespacePath<'_>);

impl<'ast> NamespacePath<'ast> {
    #[must_use]
    pub fn empty() -> Self {
        NamespacePath {
            path: Vec::new(),
            location: Location(0..0),
        }
    }

    #[must_use]
    pub fn new(tree: &'ast SyntaxTree, name: &[&'ast str]) -> Self {
        let path = name.iter().map(|&s| Identifier::new(tree, s)).collect();

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

impl std::fmt::Display for NamespacePath<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let joined = self.path.iter().map(|i| i.name).collect::<Vec<&str>>().join("::");

        f.write_str(&joined)
    }
}

impl std::hash::Hash for NamespacePath<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

impl PartialEq for NamespacePath<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum PathSegment<'ast> {
    /// Denotes a segment which refers to a namespace.
    ///
    /// ```lm
    /// std::io::File
    /// ^^^  ^^ both namespace segments
    /// ```
    Namespace { name: Identifier<'ast> },

    /// Denotes a segment which refers to a type, optionally with type
    /// arguments.
    ///
    /// ```lm
    /// std::io::File
    ///          ^^^^ type segment
    /// ```
    Type {
        name: Identifier<'ast>,
        bound_types: Vec<Type<'ast>>,
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
        name: Identifier<'ast>,
        bound_types: Vec<Type<'ast>>,
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
    Variant { name: Identifier<'ast>, location: Location },
}

impl<'ast> PathSegment<'ast> {
    /// Creates a new namespace segment, with the given name.
    #[inline]
    pub fn namespace<N: Into<Identifier<'ast>>>(name: N) -> Self {
        Self::Namespace { name: name.into() }
    }

    /// Creates a new type segment, with the given name.
    #[inline]
    pub fn ty<N: Into<Identifier<'ast>>>(name: N) -> Self {
        let name = name.into();

        Self::Type {
            location: name.location.clone(),
            name,
            bound_types: Vec::new(),
        }
    }

    /// Creates a new callable segment, with the given name.
    #[inline]
    pub fn callable<N: Into<Identifier<'ast>>>(name: N) -> Self {
        let name = name.into();

        Self::Callable {
            location: name.location.clone(),
            name,
            bound_types: Vec::new(),
        }
    }

    /// Creates a new variant segment, with the given name.
    #[inline]
    pub fn variant<N: Into<Identifier<'ast>>>(name: N) -> Self {
        let name = name.into();

        Self::Variant {
            location: name.location.clone(),
            name,
        }
    }

    /// Gets the name of the path segment.
    pub fn name(&self) -> &Identifier<'ast> {
        match self {
            Self::Namespace { name }
            | Self::Type { name, .. }
            | Self::Callable { name, .. }
            | Self::Variant { name, .. } => name,
        }
    }

    /// Gets the bound types of the path segment.
    pub fn bound_types(&self) -> &[Type<'ast>] {
        match self {
            Self::Namespace { .. } | Self::Variant { .. } => &[],
            Self::Type { bound_types, .. } | Self::Callable { bound_types, .. } => bound_types.as_slice(),
        }
    }

    /// Takes the bound types from the path segment.
    pub fn take_bound_types(&mut self) -> Vec<Type<'ast>> {
        match self {
            Self::Namespace { .. } | Self::Variant { .. } => Vec::new(),
            Self::Type { bound_types, .. } | Self::Callable { bound_types, .. } => std::mem::take(bound_types),
        }
    }

    pub fn is_self_type(&self) -> bool {
        if let Self::Type { name, .. } = self {
            name.as_str() == "Self"
        } else {
            false
        }
    }
}

impl std::fmt::Display for PathSegment<'_> {
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

impl Node for PathSegment<'_> {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::Namespace { name } => &name.location,
            Self::Type { location, .. } | Self::Callable { location, .. } | Self::Variant { location, .. } => location,
        }
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Path<'ast> {
    pub root: Vec<PathSegment<'ast>>,
    pub name: PathSegment<'ast>,
    pub location: Location,
}

node_location!(Path<'_>);

impl<'ast> Path<'ast> {
    pub fn new(root: Vec<PathSegment<'ast>>, name: PathSegment<'ast>) -> Self {
        let mut location = name.location().clone();

        location.0.start = root
            .iter()
            .map(|r| r.name().location.start())
            .min()
            .unwrap_or(location.0.start);

        Self { root, name, location }
    }

    #[must_use]
    pub fn rooted(name: PathSegment<'ast>) -> Self {
        let location = name.location().clone();

        Path {
            name,
            root: Vec::new(),
            location,
        }
    }

    #[must_use]
    pub fn with_root(root: Path<'ast>, name: PathSegment<'ast>) -> Self {
        let mut new_root = root.root;
        new_root.reserve(1);
        new_root.push(root.name);

        let start = new_root
            .first()
            .map_or(name.location().start(), |s| s.location().start());
        let end = name.location().end();

        Self {
            root: new_root,
            name,
            location: Location(start..end),
        }
    }

    pub fn merge(&mut self, other: Path<'ast>) {
        self.root.extend(other.root);
        self.root.insert(0, other.name);
        self.location = (other.location.start()..self.location.end()).into();
    }

    pub fn bound_types(&self) -> &[Type<'ast>] {
        self.name.bound_types()
    }

    pub fn is_self_type(&self) -> bool {
        self.root.is_empty() && self.name.is_self_type()
    }

    pub fn is_variant(&self) -> bool {
        matches!(self.name, PathSegment::Variant { .. })
    }
}

impl std::fmt::Display for Path<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for segment in &self.root {
            f.write_fmt(format_args!("{segment}::"))?;
        }

        f.write_fmt(format_args!("{}", self.name))
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Block<'ast> {
    pub statements: Vec<Statement<'ast>>,
    pub location: Location,
}

node_location!(Block<'_>);

impl Block<'_> {
    pub fn from_location(location: impl Into<Location>) -> Self {
        Self {
            statements: Vec::new(),
            location: location.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attribute<'ast> {
    pub name: Identifier<'ast>,
    pub arguments: Vec<AttributeArgument<'ast>>,
    pub location: Location,
}

node_location!(Attribute<'_>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttributeArgument<'ast> {
    pub key: Identifier<'ast>,
    pub value: Literal<'ast>,
    pub location: Location,
}

node_location!(AttributeArgument<'_>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter<'ast> {
    pub name: Identifier<'ast>,
    pub param_type: Type<'ast>,
    pub vararg: bool,
    pub location: Location,
}

node_location!(Parameter<'_>);

impl Parameter<'_> {
    /// Checks whether the current parameter is `self`.
    #[inline]
    #[must_use]
    pub fn is_self(&self) -> bool {
        self.name.as_str() == "self" && self.param_type.is_self()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
pub enum Item<'ast> {
    Import(Box<Import<'ast>>),
    Namespace(Box<Namespace<'ast>>),
    FunctionDefinition(Box<FunctionDefinition<'ast>>),
    StructDefinition(Box<StructDefinition<'ast>>),
    TraitDefinition(Box<TraitDefinition<'ast>>),
    EnumDefinition(Box<EnumDefinition<'ast>>),
    Implementation(Box<Implementation<'ast>>),
    TraitImplementation(Box<TraitImplementation<'ast>>),
}

impl Node for Item<'_> {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::Import(e) => &e.location,
            Self::Namespace(e) => &e.location,
            Self::FunctionDefinition(e) => &e.location,
            Self::StructDefinition(e) => e.location(),
            Self::TraitDefinition(e) => e.location(),
            Self::EnumDefinition(e) => e.location(),
            Self::Implementation(e) => &e.location,
            Self::TraitImplementation(e) => &e.location,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import<'ast> {
    pub path: ImportPath<'ast>,
    pub names: Vec<Identifier<'ast>>,
    pub location: Location,
}

node_location!(Import<'_>);

impl<'ast> Import<'ast> {
    #[must_use]
    pub fn from_names(tree: &'ast SyntaxTree, path: &[&'ast str], names: &[&'ast str]) -> Self {
        let path = ImportPath::new(tree, path);
        let names = names.iter().map(|p| Identifier::new(tree, p)).collect();

        Self {
            path,
            names,
            location: Location(0..0),
        }
    }

    #[must_use]
    pub fn std(tree: &'ast SyntaxTree, names: &[&'ast str]) -> Self {
        Self::from_names(tree, &["std"], names)
    }

    #[must_use]
    pub fn flatten(self) -> Vec<ImportPath<'ast>> {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Namespace<'ast> {
    pub path: ImportPath<'ast>,
    pub location: Location,
}

node_location!(Namespace<'_>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signature<'ast> {
    pub external: bool,
    pub name: Identifier<'ast>,
    pub type_parameters: Vec<TypeParameter<'ast>>,
    pub parameters: Vec<Parameter<'ast>>,
    pub return_type: Option<Type<'ast>>,
    pub location: Location,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefinition<'ast> {
    pub attributes: Vec<Attribute<'ast>>,
    pub visibility: Option<Visibility>,
    pub signature: Signature<'ast>,
    pub block: Option<Block<'ast>>,
    pub location: Location,
    pub documentation: Option<String>,
}

node_location!(FunctionDefinition<'_>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDefinition<'ast> {
    pub attributes: Vec<Attribute<'ast>>,
    pub visibility: Option<Visibility>,
    pub name: Identifier<'ast>,
    pub fields: Vec<Field<'ast>>,
    pub type_parameters: Vec<TypeParameter<'ast>>,
    pub location: Location,
    pub documentation: Option<String>,
}

node_location!(StructDefinition<'_>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field<'ast> {
    pub visibility: Option<Visibility>,
    pub name: Identifier<'ast>,
    pub field_type: Type<'ast>,
    pub default_value: Option<Expression<'ast>>,
    pub location: Location,
    pub documentation: Option<String>,
}

node_location!(Field<'_>);

#[derive(Debug, Clone, PartialEq)]
pub struct MethodDefinition<'ast> {
    pub attributes: Vec<Attribute<'ast>>,
    pub visibility: Option<Visibility>,
    pub signature: Signature<'ast>,
    pub block: Option<Block<'ast>>,
    pub location: Location,
    pub documentation: Option<String>,
}

node_location!(MethodDefinition<'_>);

#[derive(Debug, Clone, PartialEq)]
pub struct TraitDefinition<'ast> {
    pub attributes: Vec<Attribute<'ast>>,
    pub visibility: Option<Visibility>,
    pub name: Identifier<'ast>,
    pub type_parameters: Vec<TypeParameter<'ast>>,
    pub methods: Vec<TraitMethodDefinition<'ast>>,
    pub location: Location,
    pub documentation: Option<String>,
}

node_location!(TraitDefinition<'_>);

#[derive(Debug, Clone, PartialEq)]
pub struct TraitMethodDefinition<'ast> {
    pub attributes: Vec<Attribute<'ast>>,
    pub signature: Signature<'ast>,
    pub block: Option<Block<'ast>>,
    pub location: Location,
    pub documentation: Option<String>,
}

node_location!(TraitMethodDefinition<'_>);

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDefinition<'ast> {
    pub visibility: Option<Visibility>,
    pub name: Identifier<'ast>,
    pub type_parameters: Vec<TypeParameter<'ast>>,
    pub cases: Vec<EnumDefinitionCase<'ast>>,
    pub location: Location,
    pub documentation: Option<String>,
}

node_location!(EnumDefinition<'_>);

impl std::fmt::Display for EnumDefinition<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name.to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDefinitionCase<'ast> {
    pub name: Identifier<'ast>,
    pub parameters: Vec<Type<'ast>>,
    pub location: Location,
    pub documentation: Option<String>,
}

node_location!(EnumDefinitionCase<'_>);

impl std::fmt::Display for EnumDefinitionCase<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name.to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Implementation<'ast> {
    pub name: Box<Type<'ast>>,
    pub methods: Vec<MethodDefinition<'ast>>,
    pub type_parameters: Vec<TypeParameter<'ast>>,
    pub location: Location,
}

node_location!(Implementation<'_>);

#[derive(Debug, Clone, PartialEq)]
pub struct TraitImplementation<'ast> {
    pub name: Box<Type<'ast>>,
    pub target: Box<Type<'ast>>,
    pub methods: Vec<TraitMethodImplementation<'ast>>,
    pub type_parameters: Vec<TypeParameter<'ast>>,
    pub location: Location,
}

node_location!(TraitImplementation<'_>);

#[derive(Debug, Clone, PartialEq)]
pub struct TraitMethodImplementation<'ast> {
    pub signature: Signature<'ast>,
    pub block: Option<Block<'ast>>,
    pub location: Location,
}

node_location!(TraitMethodImplementation<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Statement<'ast> {
    VariableDeclaration(Box<VariableDeclaration<'ast>>),
    Break(Box<Break>),
    Continue(Box<Continue>),
    Final(Box<Final<'ast>>),
    Return(Box<Return<'ast>>),
    InfiniteLoop(Box<InfiniteLoop<'ast>>),
    IteratorLoop(Box<IteratorLoop<'ast>>),
    PredicateLoop(Box<PredicateLoop<'ast>>),
    Expression(Box<Expression<'ast>>),
}

impl Node for Statement<'_> {
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
pub struct VariableDeclaration<'ast> {
    pub name: Identifier<'ast>,
    pub variable_type: Option<Type<'ast>>,
    pub value: Expression<'ast>,
    pub location: Location,
}

node_location!(VariableDeclaration<'_>);

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
pub struct Final<'ast> {
    pub value: Expression<'ast>,
}

impl Node for Final<'_> {
    fn location(&self) -> &Location {
        self.value.location()
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Return<'ast> {
    pub value: Option<Expression<'ast>>,
    pub location: Location,
}

node_location!(Return<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct IfCondition<'ast> {
    pub cases: Vec<Condition<'ast>>,
    pub location: Location,
}

node_location!(IfCondition<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Condition<'ast> {
    pub condition: Option<Expression<'ast>>,
    pub block: Block<'ast>,
    pub location: Location,
}

node_location!(Condition<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct InfiniteLoop<'ast> {
    pub block: Block<'ast>,
    pub location: Location,
}

node_location!(InfiniteLoop<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct IteratorLoop<'ast> {
    pub pattern: Identifier<'ast>,
    pub collection: Expression<'ast>,
    pub block: Block<'ast>,
    pub location: Location,
}

node_location!(IteratorLoop<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct PredicateLoop<'ast> {
    pub condition: Expression<'ast>,
    pub block: Block<'ast>,
    pub location: Location,
}

node_location!(PredicateLoop<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Expression<'ast> {
    Array(Box<Array<'ast>>),
    Assignment(Box<Assignment<'ast>>),
    Call(Box<Call<'ast>>),
    Cast(Box<Cast<'ast>>),
    Construct(Box<Construct<'ast>>),
    If(Box<IfCondition<'ast>>),
    IntrinsicCall(Box<IntrinsicCall<'ast>>),
    Is(Box<Is<'ast>>),
    Literal(Box<Literal<'ast>>),
    Member(Box<Member<'ast>>),
    Range(Box<Range<'ast>>),
    Scope(Box<Scope<'ast>>),
    Switch(Box<Switch<'ast>>),
    Variable(Box<Variable<'ast>>),
    Variant(Box<Variant<'ast>>),
}

impl Node for Expression<'_> {
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
pub struct Array<'ast> {
    pub values: Vec<Expression<'ast>>,
    pub location: Location,
}

node_location!(Array<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Assignment<'ast> {
    pub target: Expression<'ast>,
    pub value: Expression<'ast>,
    pub location: Location,
}

node_location!(Assignment<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Call<'ast> {
    pub callee: Option<Expression<'ast>>,
    pub name: Path<'ast>,
    pub arguments: Vec<Expression<'ast>>,
    pub location: Location,
}

node_location!(Call<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct IntrinsicCall<'ast> {
    pub kind: IntrinsicKind<'ast>,
    pub location: Location,
}

node_location!(IntrinsicCall<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum IntrinsicKind<'ast> {
    // Arithmetic intrinsics
    Add {
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
    },
    Sub {
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
    },
    Mul {
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
    },
    Div {
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
    },
    And {
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
    },
    Or {
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
    },
    Negate {
        target: Box<Expression<'ast>>,
    },
    Increment {
        target: Box<Expression<'ast>>,
    },
    Decrement {
        target: Box<Expression<'ast>>,
    },

    // Logical intrinsics
    BinaryAnd {
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
    },
    BinaryOr {
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
    },
    BinaryXor {
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
    },
    Not {
        target: Box<Expression<'ast>>,
    },

    // Comparison intrinsics
    Equal {
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
    },
    NotEqual {
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
    },
    Less {
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
    },
    LessEqual {
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
    },
    Greater {
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
    },
    GreaterEqual {
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
    },
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Cast<'ast> {
    pub source: Expression<'ast>,
    pub target_type: Type<'ast>,
    pub location: Location,
}

node_location!(Cast<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Construct<'ast> {
    pub path: Path<'ast>,
    pub fields: Vec<ConstructorField<'ast>>,
    pub location: Location,
}

node_location!(Construct<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ConstructorField<'ast> {
    pub name: Identifier<'ast>,
    pub value: Option<Expression<'ast>>,
    pub location: Location,
}

node_location!(ConstructorField<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Is<'ast> {
    pub target: Expression<'ast>,
    pub pattern: Pattern<'ast>,
    pub location: Location,
}

node_location!(Is<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Literal<'ast> {
    Int(Box<IntLiteral>),
    Float(Box<FloatLiteral>),
    String(Box<StringLiteral<'ast>>),
    Boolean(Box<BooleanLiteral>),
}

impl Node for Literal<'_> {
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
    pub value: i128,
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
pub struct StringLiteral<'ast> {
    pub value: &'ast str,
    pub location: Location,
}

node_location!(StringLiteral<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct BooleanLiteral {
    pub value: bool,
    pub location: Location,
}

node_location!(BooleanLiteral);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Member<'ast> {
    pub callee: Expression<'ast>,
    pub name: Identifier<'ast>,
    pub location: Location,
}

node_location!(Member<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Range<'ast> {
    pub lower: Expression<'ast>,
    pub upper: Expression<'ast>,
    pub inclusive: bool,
    pub location: Location,
}

node_location!(Range<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Scope<'ast> {
    pub body: Vec<Statement<'ast>>,
    pub location: Location,
}

node_location!(Scope<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Switch<'ast> {
    pub operand: Expression<'ast>,
    pub cases: Vec<SwitchCase<'ast>>,
    pub location: Location,
}

node_location!(Switch<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct SwitchCase<'ast> {
    pub pattern: Pattern<'ast>,
    pub branch: Expression<'ast>,
    pub location: Location,
}

node_location!(SwitchCase<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Variable<'ast> {
    pub name: Identifier<'ast>,
}

impl Node for Variable<'_> {
    fn location(&self) -> &Location {
        &self.name.location
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Variant<'ast> {
    pub name: Path<'ast>,
    pub arguments: Vec<Expression<'ast>>,
    pub location: Location,
}

node_location!(Variant<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Pattern<'ast> {
    Literal(Literal<'ast>),
    Identifier(Identifier<'ast>),
    Variant(VariantPattern<'ast>),
    Wildcard(WildcardPattern),
}

impl Node for Pattern<'_> {
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
pub struct VariantPattern<'ast> {
    pub name: Path<'ast>,
    pub fields: Vec<Pattern<'ast>>,
    pub location: Location,
}

node_location!(VariantPattern<'_>);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct WildcardPattern {
    pub location: Location,
}

node_location!(WildcardPattern);

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct TypeParameter<'ast> {
    pub name: Identifier<'ast>,
    pub constraints: Vec<Type<'ast>>,
}

impl Node for TypeParameter<'_> {
    fn location(&self) -> &Location {
        &self.name.location
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Type<'ast> {
    Named(NamedType<'ast>),
    Array(ArrayType<'ast>),
    SelfType(SelfType),
}

impl Node for Type<'_> {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::Named(e) => e.location(),
            Self::Array(e) => &e.location,
            Self::SelfType(e) => &e.location,
        }
    }
}

impl Type<'_> {
    /// Checks whether the current type is a `self` type.
    #[inline]
    #[must_use]
    pub fn is_self(&self) -> bool {
        matches!(self, Type::SelfType(_))
    }
}

impl std::fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Named(t) => std::fmt::Display::fmt(t, f),
            Type::Array(t) => std::fmt::Display::fmt(t, f),
            Type::SelfType(t) => std::fmt::Display::fmt(t, f),
        }
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct NamedType<'ast> {
    pub name: Path<'ast>,
}

impl Node for NamedType<'_> {
    fn location(&self) -> &Location {
        &self.name.location
    }
}

impl std::fmt::Display for NamedType<'_> {
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
pub struct ArrayType<'ast> {
    pub element_type: Box<Type<'ast>>,
    pub location: Location,
}

node_location!(ArrayType<'_>);

impl std::fmt::Display for ArrayType<'_> {
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
