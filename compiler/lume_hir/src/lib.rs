use std::fmt::Display;

use lume_macros::Node;
use lume_span::{DefId, ExpressionId, Idx, ItemId, Location, PackageId, StatementId};

pub mod map;
pub mod symbols;

pub const SELF_TYPE_NAME: &str = "self";

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunctionId {
    pub package: PackageId,
    pub index: Idx,
}

#[derive(Default, Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct TypeId {
    pub package: PackageId,
    pub index: Idx,
}

impl TypeId {
    pub const fn new(package: PackageId, idx: usize) -> Self {
        Self {
            package,
            index: Idx::from_usize(idx),
        }
    }
}

impl Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TypeId({}, {})", self.package.as_usize(), self.index.as_usize())
    }
}

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct ImplId(pub usize);

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct UseId(pub usize);

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct FieldId(pub usize);

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct MethodId(pub usize);

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct TypeParameterId(pub usize);

#[derive(Debug, Node, Clone, Eq)]
pub struct Identifier {
    pub name: String,
    pub location: Location,
}

impl Identifier {
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.name
    }
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

impl<T: Into<String>> From<T> for Identifier {
    fn from(name: T) -> Self {
        Self {
            name: name.into(),
            location: Location::empty(),
        }
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Identifier) -> bool {
        self.name == other.name
    }
}

#[derive(Debug, Clone, Eq)]
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

    /// Denotes a segment which refers to an enum variant, with or without parameters.
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
            location: identifier.location,
            name: identifier,
            type_arguments: Vec::new(),
        }
    }

    /// Creates a new callable segment, with the given name.
    pub fn callable(identifier: impl Into<Identifier>) -> Self {
        let identifier = identifier.into();

        Self::Callable {
            location: identifier.location,
            name: identifier,
            type_arguments: Vec::new(),
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

    /// Gets the type arguments of the path segment.
    pub fn type_arguments(&self) -> &[Type] {
        match self {
            Self::Namespace { .. } | Self::Variant { .. } => &[],
            Self::Type { type_arguments, .. } | Self::Callable { type_arguments, .. } => type_arguments.as_slice(),
        }
    }

    /// Takes the type arguments from the path segment.
    pub fn take_type_arguments(self) -> Vec<Type> {
        match self {
            Self::Namespace { .. } | Self::Variant { .. } => Vec::new(),
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
                write!(f, "{name}",)?;

                if !type_arguments.is_empty() {
                    write!(
                        f,
                        "<{}>",
                        type_arguments
                            .iter()
                            .map(std::string::ToString::to_string)
                            .collect::<Vec<_>>()
                            .join(", ")
                    )?;
                }

                Ok(())
            }
            Self::Variant { name, .. } => {
                write!(f, "{name}")
            }
        }
    }
}

impl Node for PathSegment {
    #[inline]
    fn location(&self) -> Location {
        match self {
            Self::Namespace { name } => name.location,
            Self::Type { location, .. } | Self::Callable { location, .. } | Self::Variant { location, .. } => *location,
        }
    }
}

impl PartialEq for PathSegment {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name()
    }
}

impl std::hash::Hash for PathSegment {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name().hash(state);
        self.type_arguments().hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct Path {
    pub root: Vec<PathSegment>,
    pub name: PathSegment,
    pub location: Location,
}

impl Path {
    pub fn rooted(name: impl Into<PathSegment>) -> Self {
        let name = name.into();

        Self {
            root: Vec::new(),
            location: name.location(),
            name,
        }
    }

    pub fn from_parts(
        root: Option<impl IntoIterator<Item = impl Into<PathSegment>>>,
        name: impl Into<PathSegment>,
    ) -> Self {
        let name = name.into();

        Self {
            root: root
                .map(|ns| ns.into_iter().map(Into::into).collect())
                .unwrap_or_default(),
            location: name.location(),
            name,
        }
    }

    pub fn with_root(base: Path, name: PathSegment) -> Self {
        let location = base.location;
        let root = base.as_root();

        Self { root, name, location }
    }

    pub fn as_root(self) -> Vec<PathSegment> {
        let mut root = self.root;
        root.reserve(1);
        root.push(self.name);

        root
    }

    pub fn void() -> Self {
        Self::rooted(PathSegment::ty("Void"))
    }

    pub fn i8() -> Self {
        Self::from_parts(Some([PathSegment::namespace("std")]), PathSegment::ty("Int8"))
    }

    pub fn u8() -> Self {
        Self::from_parts(Some([PathSegment::namespace("std")]), PathSegment::ty("UInt8"))
    }

    pub fn i16() -> Self {
        Self::from_parts(Some([PathSegment::namespace("std")]), PathSegment::ty("Int16"))
    }

    pub fn u16() -> Self {
        Self::from_parts(Some([PathSegment::namespace("std")]), PathSegment::ty("UInt16"))
    }

    pub fn i32() -> Self {
        Self::from_parts(Some([PathSegment::namespace("std")]), PathSegment::ty("Int32"))
    }

    pub fn u32() -> Self {
        Self::from_parts(Some([PathSegment::namespace("std")]), PathSegment::ty("UInt32"))
    }

    pub fn i64() -> Self {
        Self::from_parts(Some([PathSegment::namespace("std")]), PathSegment::ty("Int64"))
    }

    pub fn u64() -> Self {
        Self::from_parts(Some([PathSegment::namespace("std")]), PathSegment::ty("UInt64"))
    }

    pub fn f32() -> Self {
        Self::from_parts(Some([PathSegment::namespace("std")]), PathSegment::ty("Float"))
    }

    pub fn f64() -> Self {
        Self::from_parts(Some([PathSegment::namespace("std")]), PathSegment::ty("Double"))
    }

    pub fn string() -> Self {
        Self::from_parts(Some([PathSegment::namespace("std")]), PathSegment::ty("String"))
    }

    pub fn pointer() -> Self {
        Self::from_parts(Some([PathSegment::namespace("std")]), PathSegment::ty("Pointer"))
    }

    pub fn array() -> Self {
        Self::from_parts(Some([PathSegment::namespace("std")]), PathSegment::ty("Array"))
    }

    pub fn boolean() -> Self {
        Self::from_parts(Some([PathSegment::namespace("std")]), PathSegment::ty("Boolean"))
    }

    pub fn cast() -> Self {
        Self::from_parts(
            Some([PathSegment::namespace("std"), PathSegment::namespace("ops")]),
            PathSegment::ty("Cast"),
        )
    }

    /// Gets the parent symbol, which contains the current symbol instance.
    ///
    /// For example, given a [`Path`] of `std::io::File::open()`, returns
    /// `Some(std::io::File)`. If no namespace is defined, returns `None`.
    pub fn parent(self) -> Option<Self> {
        let (name, root) = self.root.split_last()?;

        Some(Self {
            name: name.to_owned(),
            root: root.to_vec(),
            location: self.location,
        })
    }

    /// Gets the name of the path segment.
    pub fn name(&self) -> &Identifier {
        self.name.name()
    }

    /// Gets the type arguments of the path segment.
    pub fn type_arguments(&self) -> &[Type] {
        self.name.type_arguments()
    }

    /// Gets the all type arguments of all the path segments.
    pub fn all_type_arguments(&self) -> Vec<Type> {
        let mut args = self.type_arguments().to_vec();
        for segment in &self.root {
            args.extend_from_slice(segment.type_arguments());
        }

        args
    }

    /// Gets the all type arguments of all the root path segments.
    pub fn all_root_type_arguments(&self) -> Vec<Type> {
        let mut args = Vec::new();
        for segment in &self.root {
            args.extend_from_slice(segment.type_arguments());
        }

        args
    }

    /// Determines whether the path refers to a type.
    pub fn is_type(&self) -> bool {
        matches!(self.name, PathSegment::Type { .. })
    }

    /// Determines whether the given [`Path`]s match in terms of name.
    ///
    /// Type arguments are not test for a match.
    pub fn is_name_match(&self, other: &Self) -> bool {
        if self.root.len() != other.root.len() {
            return false;
        }

        for (s, o) in self.root.iter().zip(other.root.iter()) {
            if s.name() != o.name() {
                return false;
            }
        }

        self.name() == other.name()
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.sign_plus() {
            for segment in &self.root {
                write!(f, "{segment}::")?;
            }
        }

        write!(f, "{}", self.name)
    }
}

impl std::hash::Hash for Path {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.root.hash(state);
        self.name.hash(state);
    }
}

impl PartialEq for Path {
    fn eq(&self, other: &Self) -> bool {
        self.root == other.root && self.name == other.name
    }
}

impl Eq for Path {}

impl From<&[PathSegment]> for Path {
    fn from(value: &[PathSegment]) -> Self {
        let (name, root) = value.split_last().unwrap();

        Self::from_parts(Some(root.to_owned()), name.to_owned())
    }
}

impl From<Vec<PathSegment>> for Path {
    fn from(value: Vec<PathSegment>) -> Self {
        let (name, root) = value.split_last().unwrap();

        Self::from_parts(Some(root.to_owned()), name.to_owned())
    }
}

/// Trait for HIR nodes which can contain some amount of type parameters.
pub trait WithTypeParameters {
    /// Gets all the type parameters of this node.
    fn type_params(&self) -> &TypeParameters;
}

/// Trait for HIR nodes which have some location attached.
pub trait Node {
    fn location(&self) -> Location;
}

#[derive(Debug, PartialEq, Eq)]
pub struct Signature<'a> {
    pub name: &'a Identifier,
    pub type_parameters: &'a [TypeParameter],
    pub parameters: &'a [Parameter],
    pub return_type: &'a Type,
}

impl Signature<'_> {
    pub fn to_owned(&self) -> SignatureOwned {
        SignatureOwned {
            name: self.name.clone(),
            type_parameters: self.type_parameters.to_vec().into(),
            parameters: self.parameters.to_vec(),
            return_type: self.return_type.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SignatureOwned {
    pub name: Identifier,
    pub type_parameters: TypeParameters,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
}

impl std::fmt::Display for SignatureOwned {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}", self.name)?;

        if !self.type_parameters.is_empty() {
            write!(
                f,
                "<{}>",
                self.type_parameters
                    .iter()
                    .map(|t| t.name.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
        }

        write!(
            f,
            "({}) -> {}",
            self.parameters
                .iter()
                .map(|t| t.name.to_string())
                .collect::<Vec<_>>()
                .join(", "),
            self.return_type.name
        )
    }
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub location: Location,
}

impl Block {
    /// Determines whether all branches from the block return from the
    /// control flow.
    pub fn is_returning(&self) -> bool {
        self.statements.iter().all(Statement::is_returning)
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub enum Item {
    Function(Box<FunctionDefinition>),
    Type(Box<TypeDefinition>),
    TraitImpl(Box<TraitImplementation>),
    Impl(Box<Implementation>),
}

impl Item {
    pub fn id(&self) -> ItemId {
        match self {
            Item::Function(symbol) => symbol.id,
            Item::Type(symbol) => symbol.id(),
            Item::TraitImpl(symbol) => symbol.id,
            Item::Impl(symbol) => symbol.id,
        }
    }

    pub fn type_parameters(&self) -> &TypeParameters {
        static EMPTY: TypeParameters = TypeParameters::new();

        match self {
            Item::Function(symbol) => &symbol.type_parameters,
            Item::Type(symbol) => match &**symbol {
                TypeDefinition::Struct(s) => &s.type_parameters,
                TypeDefinition::Trait(t) => &t.type_parameters,
                TypeDefinition::Enum(e) => &e.type_parameters,
            },
            Item::Impl(symbol) => &symbol.type_parameters,
            Item::TraitImpl(_) => &EMPTY,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Def<'a> {
    Item(&'a Item),
    Field(&'a Field),
    Method(&'a MethodDefinition),
    TraitMethodDef(&'a TraitMethodDefinition),
    TraitMethodImpl(&'a TraitMethodImplementation),
    Pattern(&'a Pattern),
    Statement(&'a Statement),
    Expression(&'a Expression),
}

impl Def<'_> {
    pub fn id(&self) -> DefId {
        match self {
            Def::Item(def) => DefId::Item(def.id()),
            Def::Method(def) => def.id,
            Def::TraitMethodDef(def) => def.id,
            Def::TraitMethodImpl(def) => def.id,
            Def::Pattern(def) => def.id,
            Def::Statement(def) => DefId::Statement(def.id),
            Def::Expression(def) => DefId::Expression(def.id),
            Def::Field(def) => def.id,
        }
    }

    pub fn type_parameters(&self) -> &TypeParameters {
        static EMPTY: TypeParameters = TypeParameters::new();

        match self {
            Self::Item(item) => item.type_parameters(),
            Self::Method(def) => &def.type_parameters,
            Self::TraitMethodDef(def) => &def.type_parameters,
            Self::TraitMethodImpl(def) => &def.type_parameters,
            Self::Field(_) | Self::Pattern(_) | Self::Statement(_) | Self::Expression(_) => &EMPTY,
        }
    }

    pub fn location(&self) -> Location {
        match self {
            Def::Item(def) => def.location(),
            Def::Method(def) => def.location(),
            Def::TraitMethodDef(def) => def.location(),
            Def::TraitMethodImpl(def) => def.location(),
            Def::Statement(def) => def.location(),
            Def::Expression(def) => def.location(),
            Def::Field(def) => def.location(),
            Def::Pattern(def) => def.location,
        }
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub enum Symbol {
    Function(Box<FunctionDefinition>),
    Type(Box<TypeDefinition>),
    Use(Box<TraitImplementation>),
    Impl(Box<Implementation>),
}

impl Symbol {
    pub fn id(&self) -> ItemId {
        match self {
            Symbol::Function(symbol) => symbol.id,
            Symbol::Type(symbol) => symbol.id(),
            Symbol::Use(symbol) => symbol.id,
            Symbol::Impl(symbol) => symbol.id,
        }
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct ExternalSymbol {
    pub name: Path,
    pub location: Location,
}

impl ExternalSymbol {
    pub fn ident(&self) -> &PathSegment {
        &self.name.name
    }
}

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Visibility {
    // Order matters here, since `Ord` and `PartialOrd` determines
    // the order of enums by the order of their variants!
    Private,
    Public,
}

impl std::fmt::Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Public => write!(f, "pub"),
            Self::Private => write!(f, "priv"),
        }
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub id: ItemId,
    pub func_id: Option<FunctionId>,
    pub visibility: Visibility,
    pub name: Path,
    pub parameters: Vec<Parameter>,
    pub type_parameters: TypeParameters,
    pub return_type: Type,
    pub block: Option<Block>,
    pub location: Location,
}

impl FunctionDefinition {
    pub fn ident(&self) -> &PathSegment {
        &self.name.name
    }
}

impl WithTypeParameters for FunctionDefinition {
    fn type_params(&self) -> &TypeParameters {
        &self.type_parameters
    }
}

#[derive(Node, Debug, Clone, Eq)]
pub struct Parameter {
    pub index: usize,
    pub name: Identifier,
    pub param_type: Type,
    pub vararg: bool,
    pub location: Location,
}

impl Parameter {
    pub fn is_self(&self) -> bool {
        self.name.as_str() == SELF_TYPE_NAME
    }
}

impl std::hash::Hash for Parameter {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
        self.name.hash(state);
        self.param_type.hash(state);
        self.vararg.hash(state);
    }
}

impl PartialEq for Parameter {
    fn eq(&self, other: &Self) -> bool {
        if self.is_self() && other.is_self() {
            return true;
        }

        self.param_type == other.param_type && self.vararg == other.vararg
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    Enum(Box<EnumDefinition>),
    Struct(Box<StructDefinition>),
    Trait(Box<TraitDefinition>),
}

impl TypeDefinition {
    pub fn id(&self) -> ItemId {
        match self {
            TypeDefinition::Enum(def) => def.id,
            TypeDefinition::Struct(def) => def.id,
            TypeDefinition::Trait(def) => def.id,
        }
    }

    pub fn name(&self) -> &Path {
        match self {
            TypeDefinition::Enum(def) => def.name(),
            TypeDefinition::Struct(def) => def.name(),
            TypeDefinition::Trait(def) => def.name(),
        }
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct EnumDefinition {
    pub id: ItemId,
    pub type_id: Option<TypeId>,
    pub name: Path,
    pub type_parameters: TypeParameters,
    pub visibility: Visibility,
    pub cases: Vec<EnumDefinitionCase>,
    pub location: Location,
}

impl EnumDefinition {
    pub fn name(&self) -> &Path {
        &self.name
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct EnumDefinitionCase {
    pub idx: usize,
    pub name: Path,
    pub parameters: Vec<Type>,
    pub location: Location,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct StructDefinition {
    pub id: ItemId,
    pub type_id: Option<TypeId>,
    pub name: Path,
    pub visibility: Visibility,
    pub builtin: bool,
    pub fields: Vec<Field>,
    pub type_parameters: TypeParameters,
    pub location: Location,
}

impl StructDefinition {
    pub fn name(&self) -> &Path {
        &self.name
    }

    pub fn fields(&self) -> impl Iterator<Item = &Field> {
        self.fields.iter()
    }

    pub fn fields_mut(&mut self) -> impl Iterator<Item = &mut Field> {
        self.fields.iter_mut()
    }
}

impl WithTypeParameters for StructDefinition {
    fn type_params(&self) -> &TypeParameters {
        &self.type_parameters
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct Implementation {
    pub id: ItemId,
    pub impl_id: Option<ImplId>,
    pub target: Box<Type>,
    pub methods: Vec<MethodDefinition>,
    pub type_parameters: TypeParameters,
    pub location: Location,
}

impl WithTypeParameters for Implementation {
    fn type_params(&self) -> &TypeParameters {
        &self.type_parameters
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub enum StructMember {
    Field(Box<Field>),
    Method(Box<MethodDefinition>),
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct Field {
    pub id: DefId,
    pub field_id: Option<FieldId>,
    pub visibility: Visibility,
    pub name: Identifier,
    pub field_type: Type,
    pub default_value: Option<Expression>,
    pub location: Location,
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct MethodDefinition {
    pub id: DefId,
    pub method_id: Option<MethodId>,
    pub visibility: Visibility,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub type_parameters: TypeParameters,
    pub return_type: Type,
    pub block: Option<Block>,
    pub location: Location,
}

impl WithTypeParameters for MethodDefinition {
    fn type_params(&self) -> &TypeParameters {
        &self.type_parameters
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct TraitDefinition {
    pub id: ItemId,
    pub type_id: Option<TypeId>,
    pub name: Path,
    pub visibility: Visibility,
    pub type_parameters: TypeParameters,
    pub methods: Vec<TraitMethodDefinition>,
    pub location: Location,
}

impl TraitDefinition {
    pub fn name(&self) -> &Path {
        &self.name
    }
}

impl WithTypeParameters for TraitDefinition {
    fn type_params(&self) -> &TypeParameters {
        &self.type_parameters
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct TraitMethodDefinition {
    pub id: DefId,
    pub method_id: Option<MethodId>,
    pub visibility: Visibility,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub type_parameters: TypeParameters,
    pub return_type: Type,
    pub block: Option<Block>,
    pub location: Location,
}

impl TraitMethodDefinition {
    pub fn signature(&'_ self) -> Signature<'_> {
        Signature {
            name: &self.name,
            type_parameters: &self.type_parameters.inner,
            parameters: &self.parameters,
            return_type: &self.return_type,
        }
    }
}

impl WithTypeParameters for TraitMethodDefinition {
    fn type_params(&self) -> &TypeParameters {
        &self.type_parameters
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct TraitImplementation {
    pub id: ItemId,
    pub use_id: Option<UseId>,
    pub name: Box<Type>,
    pub target: Box<Type>,
    pub visibility: Visibility,
    pub methods: Vec<TraitMethodImplementation>,
    pub type_parameters: TypeParameters,
    pub location: Location,
}

impl TraitImplementation {
    pub fn ident(&self) -> &PathSegment {
        self.name.ident()
    }
}

impl WithTypeParameters for TraitImplementation {
    fn type_params(&self) -> &TypeParameters {
        &self.type_parameters
    }
}

#[derive(Node, Debug, Clone, PartialEq)]
pub struct TraitMethodImplementation {
    pub id: DefId,
    pub method_id: Option<MethodId>,
    pub visibility: Visibility,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub type_parameters: TypeParameters,
    pub return_type: Type,
    pub block: Block,
    pub location: Location,
}

impl TraitMethodImplementation {
    pub fn signature(&'_ self) -> Signature<'_> {
        Signature {
            name: &self.name,
            type_parameters: &self.type_parameters.inner,
            parameters: &self.parameters,
            return_type: &self.return_type,
        }
    }
}

impl WithTypeParameters for TraitMethodImplementation {
    fn type_params(&self) -> &TypeParameters {
        &self.type_parameters
    }
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct Statement {
    pub id: StatementId,
    pub kind: StatementKind,
    pub location: Location,
}

impl Statement {
    /// Determines whether the given statement or all branches within
    /// the statement branch away from the current control flow.
    pub fn is_returning(&self) -> bool {
        match &self.kind {
            StatementKind::If(stmt) => stmt.is_returning(),
            StatementKind::InfiniteLoop(stmt) => stmt.is_returning(),
            StatementKind::IteratorLoop(stmt) => stmt.is_returning(),
            StatementKind::Final(_) | StatementKind::Return(_) | StatementKind::Continue(_) => true,
            StatementKind::Variable(_) | StatementKind::Break(_) | StatementKind::Expression(_) => false,
        }
    }

    /// Determines whether the given statement is a loop statement.
    pub fn is_loop(&self) -> bool {
        matches!(
            &self.kind,
            StatementKind::InfiniteLoop(_) | StatementKind::IteratorLoop(_)
        )
    }
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub enum StatementKind {
    Variable(Box<VariableDeclaration>),
    Break(Box<Break>),
    Continue(Box<Continue>),
    Final(Box<Final>),
    Return(Box<Return>),
    If(Box<If>),
    InfiniteLoop(Box<InfiniteLoop>),
    IteratorLoop(Box<IteratorLoop>),
    Expression(Box<Expression>),
}

#[derive(Node, Hash, Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub id: StatementId,
    pub name: Identifier,
    pub declared_type: Option<Type>,
    pub value: Expression,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct Break {
    pub id: StatementId,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct Continue {
    pub id: StatementId,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct Final {
    pub id: StatementId,
    pub value: Expression,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct Return {
    pub id: StatementId,
    pub value: Option<Expression>,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct If {
    pub id: StatementId,
    pub cases: Vec<Condition>,
    pub location: Location,
}

impl If {
    /// Determines whether all branches from the statement return from the
    /// control flow.
    pub fn is_returning(&self) -> bool {
        self.cases.iter().all(|case| case.block.is_returning())
    }

    /// Gets the `else` branch, if any is defined
    pub fn else_branch(&self) -> Option<&Condition> {
        self.cases.iter().find(|case| case.condition.is_none())
    }
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct Condition {
    pub condition: Option<Expression>,
    pub block: Block,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct InfiniteLoop {
    pub id: StatementId,
    pub block: Block,
    pub location: Location,
}

impl InfiniteLoop {
    /// Determines whether the block returns from the control flow.
    pub fn is_returning(&self) -> bool {
        self.block.is_returning()
    }
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct IteratorLoop {
    pub id: StatementId,
    pub collection: Expression,
    pub block: Block,
    pub location: Location,
}

impl IteratorLoop {
    /// Determines whether the block returns from the control flow.
    pub fn is_returning(&self) -> bool {
        self.block.is_returning()
    }
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct Expression {
    pub id: ExpressionId,
    pub kind: ExpressionKind,
    pub location: Location,
}

impl Expression {
    /// Sets the location of the expression.
    #[inline]
    #[must_use]
    pub fn with_location(mut self, location: Location) -> Self {
        self.location = location;
        self
    }

    /// Creates a new [`Expression`] with the given [`LiteralKind`] value.
    pub fn lit(kind: LiteralKind) -> Self {
        Self {
            id: ExpressionId::default(),
            location: Location::empty(),
            kind: ExpressionKind::Literal(Box::new(Literal {
                id: ExpressionId::default(),
                location: Location::empty(),
                kind,
            })),
        }
    }

    /// Creates a new [`Expression`] with a [`LiteralKind::Boolean`] value.
    pub fn lit_bool(value: bool) -> Self {
        Self::lit(LiteralKind::Boolean(Box::new(BooleanLiteral {
            id: ExpressionId::default(),
            value,
        })))
    }

    /// Creates a new [`Expression`] with a [`LiteralKind::String`] value.
    pub fn lit_string(value: impl Into<String>) -> Self {
        Self::lit(LiteralKind::String(Box::new(StringLiteral {
            id: ExpressionId::default(),
            value: value.into(),
        })))
    }

    /// Creates a new [`Expression`] with a [`LiteralKind::U64`] value.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `value` is too large to fit in a signed [`i64`] value.
    pub fn lit_u64(value: u64) -> error_snippet::Result<Self> {
        Ok(Self::lit(LiteralKind::Int(Box::new(IntLiteral {
            id: ExpressionId::default(),
            value: i64::try_from(value).map_err(error_snippet::IntoDiagnostic::into_diagnostic)?,
            kind: IntKind::U64,
        }))))
    }
}

macro_rules! expr_lit_int {
    (
        $func:ident,
        $kind:ident,
        $ty:ty
    ) => {
        impl Expression {
            pub fn $func(value: $ty) -> Self {
                Self::lit(LiteralKind::Int(Box::new(IntLiteral {
                    id: ExpressionId::default(),
                    value: value.into(),
                    kind: IntKind::$kind,
                })))
            }
        }
    };
}

expr_lit_int!(lit_i8, I8, i8);
expr_lit_int!(lit_i16, I16, i16);
expr_lit_int!(lit_i32, I32, i32);
expr_lit_int!(lit_i64, I64, i64);

expr_lit_int!(lit_u8, U8, u8);
expr_lit_int!(lit_u16, U16, u16);
expr_lit_int!(lit_u32, U32, u32);

#[derive(Hash, Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Assignment(Box<Assignment>),
    Binary(Box<Binary>),
    Cast(Box<Cast>),
    Construct(Box<Construct>),

    /// Defines a call which was invoked without any callee or receiver.
    ///
    /// These are either invoked from:
    /// - a path (`std::Int32::new()`),
    /// - or as a function call (`foo()`),
    StaticCall(Box<StaticCall>),

    /// Defines a call which was invoked within the context of a receiver
    ///
    /// ```lume
    /// let a = foo();
    /// a.bar();
    /// ```
    InstanceCall(Box<InstanceCall>),

    /// Defines an intrinsic call which was replaced by a call expression.
    ///
    /// ```lume
    /// let a = 1 + 2;
    /// ```
    IntrinsicCall(Box<IntrinsicCall>),
    Literal(Box<Literal>),
    Logical(Box<Logical>),
    Member(Box<Member>),

    /// Defines a reference to a field within a pattern.
    ///
    /// ```lume
    /// switch a {
    ///    Some(b) => b,
    ///               ^ Field expression
    /// }
    /// ```
    Field(Box<PatternField>),
    Scope(Box<Scope>),
    Switch(Box<Switch>),
    Variable(Box<Variable>),
    Variant(Box<Variant>),
}

#[derive(Hash, Debug, Clone, Copy, PartialEq)]
pub enum CallExpression<'a> {
    /// Defines a call which was invoked without any callee or receiver.
    ///
    /// These are either invoked from:
    /// - a path (`std::Int32::new()`),
    /// - or as a function call (`foo()`),
    Static(&'a StaticCall),

    /// Defines a call which was invoked within the context of a receiver
    ///
    /// ```lume
    /// let a = foo();
    /// a.bar();
    /// ```
    Instanced(&'a InstanceCall),

    /// Defines an intrinsic call which was replaced by a call expression.
    ///
    /// ```lume
    /// let a = 1 + 2;
    /// ```
    Intrinsic(&'a IntrinsicCall),
}

impl CallExpression<'_> {
    #[inline]
    pub fn id(&self) -> ExpressionId {
        match self {
            Self::Instanced(call) => call.id,
            Self::Static(call) => call.id,
            Self::Intrinsic(call) => call.id,
        }
    }

    #[inline]
    pub fn name(&self) -> &Identifier {
        match self {
            Self::Instanced(call) => call.name.name(),
            Self::Static(call) => call.name.name(),
            Self::Intrinsic(call) => call.name.name(),
        }
    }

    #[inline]
    pub fn arguments(&self) -> &[Expression] {
        match self {
            Self::Instanced(call) => &call.arguments,
            Self::Static(call) => &call.arguments,
            Self::Intrinsic(call) => &call.arguments,
        }
    }

    #[inline]
    pub fn type_arguments(&self) -> &[Type] {
        match self {
            Self::Instanced(call) => call.type_arguments(),
            Self::Static(call) => call.type_arguments(),
            Self::Intrinsic(call) => call.type_arguments(),
        }
    }

    #[inline]
    pub fn location(&self) -> Location {
        match self {
            Self::Instanced(call) => call.location,
            Self::Static(call) => call.location,
            Self::Intrinsic(call) => call.location,
        }
    }

    pub fn is_instance(&self) -> bool {
        matches!(self, Self::Instanced(_))
    }
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct Assignment {
    pub id: ExpressionId,
    pub target: Expression,
    pub value: Expression,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperatorKind {
    And,
    Or,
    Xor,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct BinaryOperator {
    pub kind: BinaryOperatorKind,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct Binary {
    pub id: ExpressionId,
    pub lhs: Expression,
    pub op: BinaryOperator,
    pub rhs: Expression,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct Cast {
    pub id: ExpressionId,
    pub source: Expression,
    pub target: Type,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct Construct {
    pub id: ExpressionId,
    pub path: Path,
    pub fields: Vec<ConstructorField>,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct ConstructorField {
    pub name: Identifier,
    pub value: Expression,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct StaticCall {
    pub id: ExpressionId,
    pub name: Path,
    pub arguments: Vec<Expression>,
    pub location: Location,
}

impl StaticCall {
    pub fn type_arguments(&self) -> &[Type] {
        self.name.type_arguments()
    }

    pub fn all_type_arguments(&self) -> Vec<Type> {
        self.name.all_type_arguments()
    }
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct InstanceCall {
    pub id: ExpressionId,
    pub callee: Expression,
    pub name: PathSegment,
    pub arguments: Vec<Expression>,
    pub location: Location,
}

impl InstanceCall {
    pub fn type_arguments(&self) -> &[Type] {
        self.name.type_arguments()
    }
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct IntrinsicCall {
    pub id: ExpressionId,
    pub name: PathSegment,
    pub arguments: Vec<Expression>,
    pub location: Location,
}

impl IntrinsicCall {
    /// Returns the callee expression of the intrinsic call.
    ///
    /// # Panics
    ///
    /// Panics if the intrinsic call has no arguments.
    pub fn callee(&self) -> &Expression {
        self.arguments.first().unwrap()
    }

    pub fn type_arguments(&self) -> &[Type] {
        self.name.type_arguments()
    }
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct Literal {
    pub id: ExpressionId,
    pub kind: LiteralKind,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Int(Box<IntLiteral>),
    Float(Box<FloatLiteral>),
    String(Box<StringLiteral>),
    Boolean(Box<BooleanLiteral>),
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct IntLiteral {
    pub id: ExpressionId,
    pub value: i64,
    pub kind: IntKind,
}

#[derive(Hash, Debug, Copy, Clone, PartialEq)]
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

impl From<lume_ast::IntKind> for IntKind {
    fn from(kind: lume_ast::IntKind) -> Self {
        match kind {
            lume_ast::IntKind::I8 => IntKind::I8,
            lume_ast::IntKind::U8 => IntKind::U8,
            lume_ast::IntKind::I16 => IntKind::I16,
            lume_ast::IntKind::U16 => IntKind::U16,
            lume_ast::IntKind::I32 => IntKind::I32,
            lume_ast::IntKind::U32 => IntKind::U32,
            lume_ast::IntKind::I64 => IntKind::I64,
            lume_ast::IntKind::U64 => IntKind::U64,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FloatLiteral {
    pub id: ExpressionId,
    pub value: f64,
    pub kind: FloatKind,
}

impl std::hash::Hash for FloatLiteral {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.value.to_bits().hash(state);
        self.kind.hash(state);
    }
}

#[derive(Hash, Debug, Copy, Clone, PartialEq)]
pub enum FloatKind {
    F32,
    F64,
}

impl From<lume_ast::FloatKind> for FloatKind {
    fn from(kind: lume_ast::FloatKind) -> Self {
        match kind {
            lume_ast::FloatKind::F32 => FloatKind::F32,
            lume_ast::FloatKind::F64 => FloatKind::F64,
        }
    }
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub id: ExpressionId,
    pub value: String,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct BooleanLiteral {
    pub id: ExpressionId,
    pub value: bool,
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOperatorKind {
    And,
    Or,
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct LogicalOperator {
    pub kind: LogicalOperatorKind,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Logical {
    pub id: ExpressionId,
    pub lhs: Expression,
    pub op: LogicalOperator,
    pub rhs: Expression,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct Member {
    pub id: ExpressionId,
    pub callee: Expression,
    pub name: String,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct PatternField {
    pub id: ExpressionId,
    pub pattern: DefId,
    pub field: usize,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct Scope {
    pub id: ExpressionId,
    pub body: Vec<Statement>,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct Switch {
    pub id: ExpressionId,
    pub operand: Expression,
    pub cases: Vec<SwitchCase>,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct SwitchCase {
    pub pattern: Pattern,
    pub branch: Expression,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct Variable {
    pub id: ExpressionId,
    pub reference: VariableSource,
    pub name: Identifier,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub enum VariableSource {
    Parameter(Parameter),
    Variable(VariableDeclaration),
    Pattern(Pattern),
}

impl Node for VariableSource {
    fn location(&self) -> Location {
        match self {
            Self::Parameter(pat) => pat.location,
            Self::Variable(pat) => pat.location,
            Self::Pattern(pat) => pat.location,
        }
    }
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Variant {
    pub id: ExpressionId,
    pub name: Path,
    pub arguments: Vec<Expression>,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Pattern {
    pub id: DefId,
    pub kind: PatternKind,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub enum PatternKind {
    Literal(LiteralPattern),
    Identifier(IdentifierPattern),
    Variant(VariantPattern),
    Wildcard(WildcardPattern),
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct LiteralPattern {
    pub literal: Literal,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct IdentifierPattern {
    pub name: Identifier,
    pub location: Location,
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct VariantPattern {
    pub name: Path,
    pub fields: Vec<Pattern>,
    pub location: Location,
}

impl VariantPattern {
    #[expect(clippy::missing_panics_doc)]
    pub fn enum_name(&self) -> Path {
        self.name.clone().parent().unwrap()
    }
}

#[derive(Hash, Node, Debug, Clone, PartialEq)]
pub struct WildcardPattern {
    pub location: Location,
}

#[derive(Node, Debug, Clone, Eq)]
pub struct TypeParameter {
    pub name: Identifier,
    pub type_id: Option<TypeId>,
    pub type_param_id: Option<TypeParameterId>,
    pub constraints: Vec<Type>,
    pub location: Location,
}

impl PartialEq for TypeParameter {
    fn eq(&self, other: &Self) -> bool {
        self.constraints == other.constraints
    }
}

impl std::hash::Hash for TypeParameter {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.type_id.hash(state);
        self.type_param_id.hash(state);
        self.constraints.hash(state);
    }
}

impl AsRef<TypeParameter> for TypeParameter {
    fn as_ref(&self) -> &TypeParameter {
        self
    }
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct TypeParameters {
    pub inner: Vec<TypeParameter>,
}

impl TypeParameters {
    pub const fn new() -> Self {
        Self { inner: Vec::new() }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &TypeParameter> {
        self.inner.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut TypeParameter> {
        self.inner.iter_mut()
    }

    pub fn as_refs(&self) -> Vec<&TypeParameter> {
        self.iter().map(AsRef::as_ref).collect::<Vec<_>>()
    }
}

impl From<Vec<TypeParameter>> for TypeParameters {
    fn from(value: Vec<TypeParameter>) -> Self {
        Self { inner: value }
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum TypeArgument {
    /// Defines a named type argument, which was specified by the user, but not yet resolved.
    Named { ty: Type, location: Location },

    /// Defines an implicit type argument, which is up to the compiler to infer.
    Implicit { location: Location },
}

impl TypeArgument {
    /// Determines whether this type argument is named.
    pub fn is_named(&self) -> bool {
        matches!(self, TypeArgument::Named { .. })
    }

    /// Determines whether this type argument is implicit.
    pub fn is_implicit(&self) -> bool {
        matches!(self, TypeArgument::Implicit { .. })
    }
}

impl Node for TypeArgument {
    fn location(&self) -> Location {
        match self {
            TypeArgument::Named { location, .. } | TypeArgument::Implicit { location } => *location,
        }
    }
}

impl std::fmt::Display for TypeArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeArgument::Named { ty, .. } => ty.fmt(f),
            TypeArgument::Implicit { .. } => write!(f, "?T"),
        }
    }
}

#[derive(Node, Debug, Clone, Eq)]
pub struct Type {
    pub id: ItemId,
    pub name: Path,
    pub location: Location,
}

impl Type {
    pub fn void() -> Type {
        Self {
            id: ItemId::new(PackageId::empty()),
            name: Path::void(),
            location: Location::empty(),
        }
    }

    pub fn ident(&self) -> &PathSegment {
        &self.name.name
    }

    /// Gets the type arguments of the path segment.
    pub fn type_arguments(&self) -> &[Type] {
        self.name.type_arguments()
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}
