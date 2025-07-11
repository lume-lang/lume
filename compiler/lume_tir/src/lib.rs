use indexmap::IndexMap;
use lume_span::{ExpressionId, Interned, StatementId};
use lume_types::{Property, TypeRef};

#[derive(Debug, Default)]
pub struct TypedIR {
    pub functions: IndexMap<FunctionId, Function>,
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
            Self::Namespace { name } | Self::Type { name, .. } | Self::Callable { name, .. } => name.as_str(),
        }
    }

    /// Gets the type arguments of the path segment.
    pub fn type_arguments(&self) -> &[TypeRef] {
        match self {
            Self::Namespace { .. } => &[],
            Self::Type { type_arguments, .. } | Self::Callable { type_arguments, .. } => type_arguments.as_slice(),
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
pub enum FunctionKind {
    Function,
    Method,
}

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunctionId(pub usize);

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct VariableId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub id: FunctionId,
    pub name: Path,
    pub parameters: Vec<Parameter>,
    pub return_type: TypeRef,
    pub block: Option<Block>,
}

#[derive(Debug, Clone, Eq)]
pub struct Parameter {
    pub index: usize,
    pub var: VariableId,
    pub name: Interned<String>,
    pub ty: TypeRef,
    pub vararg: bool,
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

#[derive(Hash, Default, Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
}

impl Block {
    /// Determines whether all branches from the block return from the
    /// control flow.
    pub fn is_returning(&self) -> bool {
        self.statements.iter().all(Statement::is_returning)
    }
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub enum Statement {
    Variable(VariableDeclaration),
    Break(Break),
    Continue(Continue),
    Return(Return),
    If(If),
    InfiniteLoop(InfiniteLoop),
    IteratorLoop(IteratorLoop),
    PredicateLoop(PredicateLoop),
    Expression(Expression),
}

impl Statement {
    /// Determines whether the given statement or all branches within
    /// the statement branch away from the current control flow.
    pub fn is_returning(&self) -> bool {
        match self {
            Statement::If(stmt) => stmt.is_returning(),
            Statement::InfiniteLoop(stmt) => stmt.is_returning(),
            Statement::IteratorLoop(stmt) => stmt.is_returning(),
            Statement::PredicateLoop(stmt) => stmt.is_returning(),
            Statement::Return(_) | Statement::Continue(_) => true,
            Statement::Variable(_) | Statement::Break(_) | Statement::Expression(_) => false,
        }
    }
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub id: StatementId,
    pub var: VariableId,
    pub name: Interned<String>,
    pub value: Expression,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Break {
    pub id: StatementId,
    pub target: StatementId,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Continue {
    pub id: StatementId,
    pub target: StatementId,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Return {
    pub id: StatementId,
    pub value: Option<Expression>,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct If {
    pub id: StatementId,
    pub cases: Vec<Conditional>,
}

impl If {
    /// Determines whether all branches from the statement return from the
    /// control flow.
    pub fn is_returning(&self) -> bool {
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
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct InfiniteLoop {
    pub id: StatementId,
    pub block: Block,
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
}

impl IteratorLoop {
    /// Determines whether the block returns from the control flow.
    pub fn is_returning(&self) -> bool {
        self.block.is_returning()
    }
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct PredicateLoop {
    pub id: StatementId,
    pub condition: Expression,
    pub block: Block,
}

impl PredicateLoop {
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

#[derive(Hash, Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Assignment(Box<Assignment>),
    Binary(Box<Binary>),
    Cast(Box<Cast>),
    Construct(Box<Construct>),
    Call(Box<Call>),
    IntrinsicCall(Box<IntrinsicCall>),
    Literal(Literal),
    Logical(Box<Logical>),
    Member(Box<Member>),
    Variable(Box<VariableReference>),
    Variant(Box<Variant>),
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Assignment {
    pub id: ExpressionId,
    pub target: Expression,
    pub value: Expression,
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
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Cast {
    pub id: ExpressionId,
    pub source: Expression,
    pub target: TypeRef,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Construct {
    pub id: ExpressionId,
    pub ty: TypeRef,
    pub fields: Vec<Field>,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Field {
    pub name: Interned<String>,
    pub value: Expression,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Call {
    pub id: ExpressionId,
    pub function: FunctionId,
    pub arguments: Vec<Expression>,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct IntrinsicCall {
    pub id: ExpressionId,
    pub kind: IntrinsicKind,
    pub arguments: Vec<Expression>,
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
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Literal {
    pub id: ExpressionId,
    pub kind: LiteralKind,
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FloatLiteral {
    F32(f64),
    F64(f64),
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
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Member {
    pub id: ExpressionId,
    pub callee: Expression,
    pub property: Property,
    pub name: Interned<String>,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct VariableReference {
    pub id: ExpressionId,
    pub reference: VariableId,
    pub source: VariableSource,
    pub name: Interned<String>,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub enum VariableSource {
    Parameter,
    Variable,
}

#[derive(Hash, Debug, Clone, PartialEq)]
pub struct Variant {
    pub id: ExpressionId,
    pub name: Path,
    pub ty: TypeRef,
}
