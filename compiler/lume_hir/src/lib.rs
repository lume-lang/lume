use crate::id::{ModuleFileId, ModuleId, hash_id};
use lume_macros::Node;
use lume_types::{Identifier, IdentifierPath, SymbolName, TypeId, Visibility};

mod errors;
pub mod id;
pub mod lower;
pub mod map;
pub(crate) mod symbols;

/// Trait for types that can be converted into an owner ID.
pub trait IntoOwner {
    /// Gets the ID of the module that owns this node.
    fn owner(&self) -> ModuleId;
}

/// Uniquely identifies a definition within a module, such as a type, function or class.
#[derive(serde::Serialize, Hash, Clone, Copy, PartialEq, Eq)]
pub struct ItemId(pub ModuleId, pub u64);

impl std::fmt::Debug for ItemId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", hash_id(self))
    }
}

impl IntoOwner for ItemId {
    fn owner(&self) -> ModuleId {
        self.0
    }
}

/// Uniquely identifies any local expression, such as variables, arguments, calls or otherwise.
#[derive(serde::Serialize, Hash, Clone, Copy, PartialEq, Eq)]
pub struct LocalId(pub u64);

impl LocalId {
    pub fn empty() -> Self {
        Self(0)
    }
}

impl std::fmt::Debug for LocalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Uniquely identifies any local statement.
#[derive(serde::Serialize, Hash, Clone, Copy, PartialEq, Eq)]
pub struct StatementId(pub ModuleId, pub LocalId);

impl StatementId {
    pub fn empty() -> Self {
        Self(ModuleId::empty(), LocalId::empty())
    }

    pub fn local(&self) -> LocalId {
        self.1
    }
}

impl std::fmt::Debug for StatementId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", hash_id(self))
    }
}

impl IntoOwner for StatementId {
    fn owner(&self) -> ModuleId {
        self.0
    }
}

/// Uniquely identifies any local expression, such as variables, literals, calls or otherwise.
#[derive(serde::Serialize, Hash, Clone, Copy, PartialEq, Eq)]
pub struct ExpressionId(pub ModuleId, pub LocalId);

impl ExpressionId {
    pub fn empty() -> Self {
        Self(ModuleId::empty(), LocalId::empty())
    }

    pub fn local(&self) -> LocalId {
        self.1
    }
}

impl std::fmt::Debug for ExpressionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", hash_id(self))
    }
}

impl IntoOwner for ExpressionId {
    fn owner(&self) -> ModuleId {
        self.0
    }
}

#[derive(serde::Serialize, Hash, Clone, Copy, PartialEq, Eq)]
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
            file: ModuleFileId(ModuleId(0), 0),
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

impl std::fmt::Debug for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}:{}:{}", self.file, self.start, self.end())
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

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum Symbol {
    Function(Box<FunctionDefinition>),
    ExternalFunction(Box<ExternalFunctionDefinition>),
    Type(Box<TypeDefinition>),
    Impl(Box<TraitImplementation>),
}

impl Symbol {
    pub fn id(&self) -> ItemId {
        match self {
            Symbol::Function(symbol) => symbol.id,
            Symbol::ExternalFunction(symbol) => symbol.id,
            Symbol::Type(symbol) => symbol.id(),
            Symbol::Impl(symbol) => symbol.id,
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
    pub id: ExpressionId,
    pub name: Identifier,
    pub param_type: Type,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    Enum(Box<EnumDefinition>),
    Alias(Box<AliasDefinition>),
    Class(Box<ClassDefinition>),
    Trait(Box<TraitDefinition>),
}

impl TypeDefinition {
    pub fn id(&self) -> ItemId {
        match self {
            TypeDefinition::Enum(def) => def.id,
            TypeDefinition::Alias(def) => def.id,
            TypeDefinition::Class(def) => def.id,
            TypeDefinition::Trait(def) => def.id,
        }
    }

    pub fn name(&self) -> &SymbolName {
        match self {
            TypeDefinition::Enum(def) => def.name(),
            TypeDefinition::Alias(def) => def.name(),
            TypeDefinition::Class(def) => def.name(),
            TypeDefinition::Trait(def) => def.name(),
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
    pub fn name(&self) -> &SymbolName {
        &self.name
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
    pub fn name(&self) -> &SymbolName {
        &self.name
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct ClassDefinition {
    pub id: ItemId,
    pub type_id: Option<TypeId>,
    pub name: SymbolName,
    pub builtin: bool,
    pub members: Vec<ClassMember>,
    pub location: Location,
}

impl ClassDefinition {
    pub fn name(&self) -> &SymbolName {
        &self.name
    }

    pub fn members_iter(&self) -> impl Iterator<Item = &ClassMember> {
        self.members.iter()
    }

    pub fn properties(&self) -> Vec<&Property> {
        self.members_iter()
            .filter_map(|member| match member {
                ClassMember::Property(property) => Some(property.as_ref()),
                _ => None,
            })
            .collect()
    }

    pub fn methods(&self) -> Vec<&MethodDefinition> {
        self.members_iter()
            .filter_map(|member| match member {
                ClassMember::Method(method) => Some(method.as_ref()),
                _ => None,
            })
            .collect()
    }

    pub fn external_methods(&self) -> Vec<&ExternalMethodDefinition> {
        self.members_iter()
            .filter_map(|member| match member {
                ClassMember::ExternalMethod(method) => Some(method.as_ref()),
                _ => None,
            })
            .collect()
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub enum ClassMember {
    Property(Box<Property>),
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
pub struct MethodDefinition {
    pub visibility: Visibility,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct ExternalMethodDefinition {
    pub visibility: Visibility,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct TraitDefinition {
    pub id: ItemId,
    pub name: SymbolName,
    pub methods: Vec<TraitMethodDefinition>,
    pub location: Location,
}

impl TraitDefinition {
    pub fn name(&self) -> &SymbolName {
        &self.name
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct TraitMethodDefinition {
    pub visibility: Visibility,
    pub name: SymbolName,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub block: Option<Block>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct TraitImplementation {
    pub id: ItemId,
    pub name: Box<Type>,
    pub target: Box<Type>,
    pub methods: Vec<TraitMethodImplementation>,
    pub location: Location,
}

impl TraitImplementation {
    pub fn ident(&self) -> &Identifier {
        &self.name.ident()
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct TraitMethodImplementation {
    pub visibility: Visibility,
    pub name: SymbolName,
    pub parameters: Vec<Parameter>,
    pub return_type: Box<Type>,
    pub block: Block,
    pub location: Location,
}

impl TraitMethodImplementation {
    pub fn ident(&self) -> &Identifier {
        &self.name.name
    }
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Statement {
    pub id: StatementId,
    pub kind: StatementKind,
    pub location: Location,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum StatementKind {
    Variable(Box<VariableDeclaration>),
    Break(Box<Break>),
    Continue(Box<Continue>),
    Return(Box<Return>),
    If(Box<If>),
    Unless(Box<Unless>),
    InfiniteLoop(Box<InfiniteLoop>),
    IteratorLoop(Box<IteratorLoop>),
    PredicateLoop(Box<PredicateLoop>),
    Expression(Box<Expression>),
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub id: StatementId,
    pub name: Identifier,
    pub declared_type: Option<Type>,
    pub value: Expression,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Break {
    pub id: StatementId,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Continue {
    pub id: StatementId,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Return {
    pub id: StatementId,
    pub value: Option<Expression>,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct If {
    pub id: StatementId,
    pub cases: Vec<Condition>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Unless {
    pub id: StatementId,
    pub cases: Vec<Condition>,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Condition {
    pub id: StatementId,
    pub condition: Option<Expression>,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct InfiniteLoop {
    pub id: StatementId,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct IteratorLoop {
    pub id: StatementId,
    pub collection: Expression,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct PredicateLoop {
    pub id: StatementId,
    pub condition: Expression,
    pub block: Block,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Expression {
    pub id: ExpressionId,
    pub location: Location,
    pub kind: ExpressionKind,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Assignment(Box<Assignment>),
    New(Box<New>),
    FunctionCall(Box<FunctionCall>),
    MethodCall(Box<MethodCall>),
    Literal(Box<Literal>),
    Member(Box<Member>),
    Range(Box<Range>),
    Variable(Box<Variable>),
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Assignment {
    pub id: ExpressionId,
    pub target: Expression,
    pub value: Expression,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct New {
    pub id: ExpressionId,
    pub name: Box<Type>,
    pub arguments: Vec<Expression>,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub id: ExpressionId,
    pub name: SymbolName,
    pub arguments: Vec<Expression>,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct MethodCall {
    pub id: ExpressionId,
    pub callee: Expression,
    pub name: Identifier,
    pub arguments: Vec<Expression>,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Literal {
    pub id: ExpressionId,
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
    pub id: ExpressionId,
    pub value: i64,
    pub kind: IntKind,
}

#[derive(serde::Serialize, Debug, Copy, Clone, PartialEq)]
pub enum IntKind {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    IPtr,
    UPtr,
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

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct FloatLiteral {
    pub id: ExpressionId,
    pub value: f64,
    pub kind: FloatKind,
}

#[derive(serde::Serialize, Debug, Copy, Clone, PartialEq)]
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

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub id: ExpressionId,
    pub value: String,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct BooleanLiteral {
    pub id: ExpressionId,
    pub value: bool,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Member {
    pub id: ExpressionId,
    pub callee: Expression,
    pub name: String,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Range {
    pub id: ExpressionId,
    pub lower: Expression,
    pub upper: Expression,
    pub inclusive: bool,
    pub location: Location,
}

#[derive(serde::Serialize, Node, Debug, Clone, PartialEq)]
pub struct Variable {
    pub id: ExpressionId,
    pub reference: StatementId,
    pub name: Identifier,
    pub location: Location,
}

#[derive(serde::Serialize, Hash, Node, Debug, Clone, PartialEq)]
pub enum Type {
    Scalar(Box<ScalarType>),
    Array(Box<ArrayType>),
}

impl Type {
    pub fn ident(&self) -> &Identifier {
        match self {
            Type::Scalar(ty) => &ty.name.name,
            Type::Array(ty) => &ty.element_type.ident(),
        }
    }
}

#[derive(serde::Serialize, Hash, Node, Debug, Clone, PartialEq)]
pub struct ScalarType {
    pub name: SymbolName,
    pub type_params: Vec<Box<Type>>,
    pub location: Location,
}

#[derive(serde::Serialize, Hash, Node, Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub element_type: Box<Type>,
    pub location: Location,
}
