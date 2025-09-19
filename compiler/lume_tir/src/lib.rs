use indexmap::IndexMap;
use lume_span::{DefId, ExpressionId, Interned, Location, PatternId, StatementId};
use lume_type_metadata::{StaticMetadata, TypeMetadataId};
use lume_types::{Field, TypeRef};

#[derive(Debug, Default)]
pub struct TypedIR {
    pub metadata: StaticMetadata,
    pub functions: IndexMap<DefId, Function>,
}

#[derive(Debug, Clone, Eq)]
pub enum PathSegment {
    /// Denotes a segment which refers to a namespace.
    ///
    /// ```lm
    /// std::io::File
    /// ^^^  ^^ both namespace segments
    /// ```
    Namespace { name: String },

    /// Denotes a segment which refers to a type, optionally with type arguments.
    ///
    /// ```lm
    /// std::io::File
    ///          ^^^^ type segment
    /// ```
    Type { name: String, type_arguments: Vec<TypeRef> },

    /// Denotes a segment which refers to a callable, such as a function or method.
    ///
    /// ```lm
    /// std::io::File::open()
    ///                ^^^^ callable segment
    ///
    /// std::io::read_file()
    ///          ^^^^^^^^^ callable segment
    /// ```
    Callable { name: String, type_arguments: Vec<TypeRef> },

    /// Denotes a segment which refers to an enum variant, with or without parameters.
    ///
    /// ```lm
    /// Option::None
    ///         ^^^^ variant segment
    ///
    /// Option::Some(false)
    ///         ^^^^^^^^^^^ variant segment
    /// ```
    Variant { name: String },
}

impl PathSegment {
    /// Creates a new namespace segment, with the given name.
    pub fn namespace(name: impl Into<String>) -> Self {
        Self::Namespace { name: name.into() }
    }

    /// Creates a new type segment, with the given name.
    pub fn ty(name: impl Into<String>) -> Self {
        Self::Type {
            name: name.into(),
            type_arguments: Vec::new(),
        }
    }

    /// Creates a new callable segment, with the given name.
    pub fn callable(name: impl Into<String>) -> Self {
        Self::Callable {
            name: name.into(),
            type_arguments: Vec::new(),
        }
    }

    /// Gets the name of the path segment.
    pub fn name(&self) -> &str {
        match self {
            Self::Namespace { name }
            | Self::Type { name, .. }
            | Self::Callable { name, .. }
            | Self::Variant { name, .. } => name.as_str(),
        }
    }

    /// Gets the type arguments of the path segment.
    pub fn type_arguments(&self) -> &[TypeRef] {
        match self {
            Self::Namespace { .. } | Self::Variant { .. } => &[],
            Self::Type { type_arguments, .. } | Self::Callable { type_arguments, .. } => type_arguments.as_slice(),
        }
    }
}

impl std::fmt::Display for PathSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Namespace { name } | Self::Variant { name } => f.write_str(name.as_str()),
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
                            .enumerate()
                            .map(|(idx, _)| format!("`{}", idx + 1))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )?;
                }

                Ok(())
            }
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

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Path {
    pub root: Vec<PathSegment>,
    pub name: PathSegment,
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for segment in &self.root {
            write!(f, "{segment}::")?;
        }

        write!(f, "{}", self.name)
    }
}

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct VariableId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionKind {
    Static,
    Dynamic,
    Unreachable,
}

impl FunctionKind {
    /// Determines whether this function kind should be lowered.
    pub fn should_be_lowered(self) -> bool {
        if matches!(self, FunctionKind::Dynamic | FunctionKind::Unreachable) {
            return false;
        }

        true
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub id: DefId,
    pub name: Path,
    pub kind: FunctionKind,
    pub parameters: Vec<Parameter>,
    pub type_params: TypeParameters,
    pub return_type: TypeRef,
    pub block: Option<Block>,
    pub location: Location,
}

impl Function {
    pub fn name_as_str(&self) -> String {
        format!("{:+}", self.name)
    }
}

#[derive(Debug, Clone, Eq)]
pub struct Parameter {
    pub index: usize,
    pub var: VariableId,
    pub name: Interned<String>,
    pub ty: TypeRef,
    pub vararg: bool,
    pub location: Location,
}

impl std::hash::Hash for Parameter {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
        self.var.hash(state);
        self.name.hash(state);
        self.ty.hash(state);
        self.vararg.hash(state);
    }
}

impl PartialEq for Parameter {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParameter {
    pub var: VariableId,
    pub name: String,
    pub constraints: Vec<TypeRef>,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub return_type: TypeRef,
}

impl Block {
    /// Determines whether all branches from the block return from the
    /// control flow.
    pub fn is_returning(&self) -> bool {
        // .all() returns `true` if the iterator is empty, which isn't correct.
        if self.statements.is_empty() {
            return false;
        }

        self.statements.iter().all(Statement::is_returning)
    }

    /// Determines whether the block has a non-void return value.
    pub fn has_return_value(&self) -> bool {
        !self.return_type.is_void()
    }
}

impl Default for Block {
    fn default() -> Self {
        Self {
            statements: Vec::new(),
            return_type: TypeRef::void(),
        }
    }
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub enum Statement {
    Variable(VariableDeclaration),
    Break(Break),
    Continue(Continue),
    Final(Final),
    Return(Return),
    InfiniteLoop(InfiniteLoop),
    IteratorLoop(IteratorLoop),
    Expression(Expression),
}

impl Statement {
    /// Determines whether the given statement or all branches within
    /// the statement branch away from the current control flow.
    pub fn is_returning(&self) -> bool {
        match self {
            Statement::InfiniteLoop(stmt) => stmt.is_returning(),
            Statement::IteratorLoop(stmt) => stmt.is_returning(),
            Statement::Return(_) | Statement::Continue(_) => true,
            Statement::Final(_) | Statement::Variable(_) | Statement::Break(_) => false,
            Statement::Expression(expr) => expr.is_returning(),
        }
    }
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub id: StatementId,
    pub var: VariableId,
    pub name: Interned<String>,
    pub value: Expression,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Break {
    pub id: StatementId,
    pub target: StatementId,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Continue {
    pub id: StatementId,
    pub target: StatementId,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Final {
    pub id: StatementId,
    pub value: Expression,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Return {
    pub id: StatementId,
    pub value: Option<Expression>,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
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

#[derive(Hash, Debug, Clone, PartialEq)]
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

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub ty: TypeRef,
}

impl Expression {
    pub fn location(&self) -> Location {
        match &self.kind {
            ExpressionKind::Assignment(e) => e.location,
            ExpressionKind::Binary(e) => e.location,
            ExpressionKind::Cast(e) => e.location,
            ExpressionKind::Construct(e) => e.location,
            ExpressionKind::Call(e) => e.location,
            ExpressionKind::If(e) => e.location,
            ExpressionKind::Is(e) => e.location,
            ExpressionKind::IntrinsicCall(e) => e.location,
            ExpressionKind::Literal(e) => e.location,
            ExpressionKind::Logical(e) => e.location,
            ExpressionKind::Member(e) => e.location,
            ExpressionKind::Scope(e) => e.location,
            ExpressionKind::Variable(e) => e.location,
            ExpressionKind::Switch(e) => e.location,
            ExpressionKind::Variant(e) => e.location,
        }
    }

    /// Determines whether the given expression or all branches within
    /// the expression branch away from the current control flow.
    pub fn is_returning(&self) -> bool {
        match &self.kind {
            ExpressionKind::If(stmt) => stmt.is_returning(),
            _ => false,
        }
    }
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Assignment(Box<Assignment>),
    Binary(Box<Binary>),
    Cast(Box<Cast>),
    Construct(Box<Construct>),
    Call(Box<Call>),
    If(If),
    IntrinsicCall(Box<IntrinsicCall>),
    Is(Box<Is>),
    Literal(Literal),
    Logical(Box<Logical>),
    Member(Box<Member>),
    Scope(Box<Scope>),
    Switch(Box<Switch>),
    Variable(Box<VariableReference>),
    Variant(Box<Variant>),
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Assignment {
    pub id: ExpressionId,
    pub target: Expression,
    pub value: Expression,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    And,
    Or,
    Xor,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Binary {
    pub id: ExpressionId,
    pub lhs: Expression,
    pub op: BinaryOperator,
    pub rhs: Expression,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Cast {
    pub id: ExpressionId,
    pub source: Expression,
    pub target: TypeRef,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Construct {
    pub id: ExpressionId,
    pub ty: TypeRef,
    pub fields: Vec<ConstructorField>,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct ConstructorField {
    pub name: Interned<String>,
    pub value: Expression,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Call {
    pub id: ExpressionId,
    pub function: DefId,
    pub arguments: Vec<Expression>,
    pub type_arguments: Vec<TypeRef>,
    pub return_type: TypeRef,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct If {
    pub id: ExpressionId,
    pub cases: Vec<Conditional>,
    pub return_type: Option<TypeRef>,
    pub location: Location,
}

impl If {
    /// Determines whether all branches from the statement return from the
    /// control flow.
    pub fn is_returning(&self) -> bool {
        // .all() returns `true` if the iterator is empty, which isn't correct.
        if self.cases.is_empty() {
            return false;
        }

        self.cases.iter().all(|branch| branch.block.is_returning())
    }

    /// Gets the `else` branch, if any is defined
    pub fn else_branch(&self) -> Option<&Conditional> {
        self.cases.iter().find(|case| case.condition.is_none())
    }
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Conditional {
    pub condition: Option<Expression>,
    pub block: Block,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct IntrinsicCall {
    pub id: ExpressionId,
    pub kind: IntrinsicKind,
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
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub enum IntrinsicKind {
    FloatEq { bits: u8 },
    FloatNe { bits: u8 },
    FloatGe { bits: u8 },
    FloatGt { bits: u8 },
    FloatLe { bits: u8 },
    FloatLt { bits: u8 },
    FloatAdd { bits: u8 },
    FloatSub { bits: u8 },
    FloatMul { bits: u8 },
    FloatDiv { bits: u8 },
    IntEq { bits: u8, signed: bool },
    IntNe { bits: u8, signed: bool },
    IntGe { bits: u8, signed: bool },
    IntGt { bits: u8, signed: bool },
    IntLe { bits: u8, signed: bool },
    IntLt { bits: u8, signed: bool },
    IntAdd { bits: u8, signed: bool },
    IntSub { bits: u8, signed: bool },
    IntMul { bits: u8, signed: bool },
    IntDiv { bits: u8, signed: bool },
    IntAnd { bits: u8, signed: bool },
    IntOr { bits: u8, signed: bool },
    IntXor { bits: u8, signed: bool },
    BooleanEq,
    BooleanNe,
    BooleanAnd,
    BooleanOr,
    Metadata { id: TypeMetadataId },
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Is {
    pub id: ExpressionId,
    pub target: Expression,
    pub pattern: Pattern,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Literal {
    pub id: ExpressionId,
    pub kind: LiteralKind,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, Copy, PartialEq)]
pub enum LiteralKind {
    Int(IntLiteral),
    Float(FloatLiteral),
    String(Interned<String>),
    Boolean(bool),
}

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub enum IntLiteral {
    I8(i64),
    U8(i64),
    I16(i64),
    U16(i64),
    I32(i64),
    U32(i64),
    I64(i64),
    U64(i64),
}

impl IntLiteral {
    pub fn signed(self) -> bool {
        match self {
            Self::U8(_) | Self::U16(_) | Self::U32(_) | Self::U64(_) => false,
            Self::I8(_) | Self::I16(_) | Self::I32(_) | Self::I64(_) => true,
        }
    }

    pub fn bits(self) -> u8 {
        match self {
            Self::I8(_) | Self::U8(_) => 8,
            Self::I16(_) | Self::U16(_) => 16,
            Self::I32(_) | Self::U32(_) => 32,
            Self::I64(_) | Self::U64(_) => 64,
        }
    }

    pub fn value(self) -> i64 {
        match self {
            Self::I8(value) => value,
            Self::U8(value) => value,
            Self::I16(value) => value,
            Self::U16(value) => value,
            Self::I32(value) => value,
            Self::U32(value) => value,
            Self::I64(value) => value,
            Self::U64(value) => value,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FloatLiteral {
    F32(f64),
    F64(f64),
}

impl FloatLiteral {
    pub fn bits(self) -> u8 {
        match self {
            Self::F32(_) => 32,
            Self::F64(_) => 64,
        }
    }

    pub fn value(self) -> f64 {
        match self {
            Self::F32(value) => value,
            Self::F64(value) => value,
        }
    }
}

impl std::hash::Hash for FloatLiteral {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::F32(val) | Self::F64(val) => val.to_bits().hash(state),
        }
    }
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOperator {
    And,
    Or,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Logical {
    pub id: ExpressionId,
    pub lhs: Expression,
    pub op: LogicalOperator,
    pub rhs: Expression,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Switch {
    pub id: ExpressionId,
    pub operand: Expression,
    pub operand_var: VariableId,
    pub entries: Vec<(SwitchConstantPattern, Expression)>,
    pub fallback: Expression,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub enum SwitchConstantPattern {
    Literal(SwitchConstantLiteral),
    Variable(VariableId),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SwitchConstantLiteral {
    Integer(i64),
    Float(f64),
    Boolean(bool),
}

impl std::hash::Hash for SwitchConstantLiteral {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Integer(lit) => lit.hash(state),
            Self::Float(lit) => lit.to_bits().hash(state),
            Self::Boolean(lit) => lit.hash(state),
        }
    }
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Member {
    pub id: ExpressionId,
    pub callee: Expression,
    pub field: Field,
    pub name: Interned<String>,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Scope {
    pub id: ExpressionId,
    pub body: Vec<Statement>,
    pub return_type: TypeRef,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct VariableReference {
    pub id: ExpressionId,
    pub reference: VariableId,
    pub source: VariableSource,
    pub name: Interned<String>,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub enum VariableSource {
    Parameter,
    Variable,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Variant {
    pub id: ExpressionId,
    pub index: u8,
    pub ty: TypeRef,
    pub name: Path,
    pub arguments: Vec<Expression>,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Pattern {
    pub id: PatternId,
    pub kind: PatternKind,
    pub location: Location,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub enum PatternKind {
    Literal(Literal),
    Variable(VariableId),
    Variant(VariantPattern),
    Wildcard,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct VariantPattern {
    pub index: u8,
    pub ty: TypeRef,
    pub name: Path,
    pub fields: Vec<Pattern>,
}
