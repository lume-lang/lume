use indexmap::IndexMap;

#[derive(Default, Debug, Clone)]
pub struct ModuleMap {
    pub functions: Vec<Function>,
}

impl ModuleMap {
    /// Creates a new empty [`ModuleMap`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the next available function ID.
    ///
    /// # Panics
    ///
    /// Panics if the given ID is invalid or out of bounds.
    pub fn new_function_id(&self) -> FunctionId {
        FunctionId(self.functions.len())
    }

    /// Returns a reference to the function with the given ID.
    ///
    /// # Panics
    ///
    /// Panics if the given ID is invalid or out of bounds.
    pub fn function(&self, id: FunctionId) -> &Function {
        self.functions.get(id.0).unwrap()
    }

    /// Returns a mutable reference to the function with the given ID.
    ///
    /// # Panics
    ///
    /// Panics if the given ID is invalid or out of bounds.
    pub fn function_mut(&mut self, id: FunctionId) -> &mut Function {
        self.functions.get_mut(id.0).unwrap()
    }

    /// Allocates a new function and returns its ID.
    pub fn new_func(&mut self, name: String) -> FunctionId {
        let id = self.new_function_id();

        self.functions.push(Function::new(id, name));

        id
    }
}

impl std::fmt::Display for ModuleMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for func in &self.functions {
            write!(f, "{func}")?;
        }

        Ok(())
    }
}

#[derive(Hash, Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct FunctionId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub id: FunctionId,
    pub name: String,
    pub external: bool,

    pub parameters: Vec<Type>,
    pub return_type: Type,

    pub locals: Vec<Local>,
    pub blocks: Vec<BasicBlock>,
    pub variables: IndexMap<lume_span::StatementId, Local>,
    current_block: BasicBlockId,

    pub(crate) scope: Box<Scope>,
}

impl Function {
    pub fn new(id: FunctionId, name: String) -> Self {
        Function {
            id,
            name,
            locals: Vec::new(),
            blocks: Vec::new(),
            variables: IndexMap::new(),
            external: false,
            current_block: BasicBlockId(0),
            scope: Box::new(Scope::root_scope()),
            parameters: Vec::new(),
            return_type: Type::Void,
        }
    }

    /// Returns a reference to the current basic block.
    pub fn current_block(&self) -> &BasicBlock {
        self.block(self.current_block)
    }

    /// Returns a mutable reference to the current basic block.
    pub fn current_block_mut(&mut self) -> &mut BasicBlock {
        self.block_mut(self.current_block)
    }

    /// Sets the current basic block.
    pub fn set_current_block(&mut self, block: BasicBlockId) {
        self.current_block = block;
    }

    /// Returns a reference to the basic block with the given ID.
    ///
    /// # Panics
    ///
    /// Panics if the given ID is invalid or out of bounds.
    pub fn block(&self, id: BasicBlockId) -> &BasicBlock {
        self.blocks.get(id.0).unwrap()
    }

    /// Returns a mutable reference to the basic block with the given ID.
    ///
    /// # Panics
    ///
    /// Panics if the given ID is invalid or out of bounds.
    pub fn block_mut(&mut self, id: BasicBlockId) -> &mut BasicBlock {
        self.blocks.get_mut(id.0).unwrap()
    }

    /// Allocates a new basic block and returns its ID.
    pub fn new_block(&mut self) -> BasicBlockId {
        let id = BasicBlockId(self.blocks.len());

        self.blocks.push(BasicBlock::new(id));

        id
    }

    /// Allocates a new basic block, sets it as the active block, and returns its ID.
    pub fn new_active_block(&mut self) -> BasicBlockId {
        let block = self.new_block();

        self.set_current_block(block);

        block
    }

    /// Adds the given instruction to the basic block with the given ID.
    ///
    /// # Panics
    ///
    /// Panics if the given ID is invalid or out of bounds.
    pub fn add_instruction(&mut self, block: BasicBlockId, inst: Instruction) {
        self.block_mut(block).instructions.push(inst);
    }

    /// Returns a reference to the instruction with the given ID.
    ///
    /// # Panics
    ///
    /// Panics if the given ID is invalid or out of bounds.
    pub fn instruction(&self, block: BasicBlockId, id: InstructionId) -> &Instruction {
        self.block(block).instructions.get(id.0).unwrap()
    }

    /// Allocates a new instruction and adds it to the basic block with the given ID.
    ///
    /// # Panics
    ///
    /// Panics if the given ID is invalid or out of bounds.
    pub fn new_instruction(&mut self, block: BasicBlockId, kind: InstructionKind) -> InstructionId {
        let block = self.block_mut(block);
        let id = InstructionId(block.instructions.len());

        block.instructions.push(Instruction { id, kind });

        id
    }

    /// Allocates a new variable local and returns its ID.
    pub fn new_local(&mut self) -> Local {
        let id = Local::var(self.locals.len());

        self.locals.push(id);

        id
    }

    /// Declares a new local with the given declaration in the current block.
    pub fn declare(&mut self, decl: Declaration) -> Local {
        let local = self.new_local();

        self.current_block_mut().declare(local, decl)
    }

    /// Declares a new local with the given value in the current block.
    pub fn declare_value(&mut self, value: Value) -> Local {
        self.declare(Declaration::Value(value))
    }

    /// Enters a new scope with the given scope kind.
    pub fn enter_scope(&mut self, kind: ScopeKind) {
        let mut scope = Box::new(Scope { kind, parent: None });

        std::mem::swap(&mut self.scope, &mut scope);

        self.scope.parent = Some(scope);
    }

    /// Enters a new loop scope with the given body- and end-blocks.
    pub fn enter_loop_scope(&mut self, body: BasicBlockId, end: BasicBlockId) {
        self.enter_scope(ScopeKind::Loop { body, end });
    }

    /// Exits the current scope.
    ///
    /// # Panics
    ///
    /// Panics if the current scope is the root scope.
    pub fn exit_scope(&mut self) -> Box<Scope> {
        if let Some(mut scope) = self.scope.parent.take() {
            std::mem::swap(&mut scope, &mut self.scope);

            scope
        } else {
            panic!("attempted to exit from root scope")
        }
    }

    /// Finds the loop target of the current scope.
    pub fn loop_target(&self) -> Option<(BasicBlockId, BasicBlockId)> {
        let mut scope = Some(&self.scope);

        while let Some(current) = scope {
            if let ScopeKind::Loop { body, end } = &current.kind {
                return Some((*body, *end));
            }

            scope = current.parent.as_ref();
        }

        None
    }

    /// Finds the loop target of the current scope.
    ///
    /// # Panics
    ///
    /// If no loop target is found, this function will panic.
    pub fn expect_loop_target(&self) -> (BasicBlockId, BasicBlockId) {
        let Some(target) = self.loop_target() else {
            panic!("bug!: no loop target found");
        };

        target
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.external {
            writeln!(f, "declare extern fn {:?}", self.name)?;
            return writeln!(f);
        }

        writeln!(f, "{:?} {{", self.name)?;

        for block in &self.blocks {
            write!(f, "{block}")?;
        }

        writeln!(f, "}}")?;
        writeln!(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScopeKind {
    Regular,
    Loop { body: BasicBlockId, end: BasicBlockId },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    kind: ScopeKind,
    parent: Option<Box<Scope>>,
}

impl Scope {
    pub fn root_scope() -> Self {
        Scope {
            kind: ScopeKind::Regular,
            parent: None,
        }
    }

    pub fn loop_scope(body: BasicBlockId, end: BasicBlockId) -> Self {
        Scope {
            kind: ScopeKind::Loop { body, end },
            parent: None,
        }
    }
}

#[derive(Hash, Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct BasicBlockId(pub usize);

impl std::fmt::Display for BasicBlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "B{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlock {
    pub id: BasicBlockId,
    pub instructions: Vec<Instruction>,
    pub terminator: Option<Terminator>,
}

impl BasicBlock {
    pub fn new(id: BasicBlockId) -> Self {
        BasicBlock {
            id,
            instructions: Vec::new(),
            terminator: None,
        }
    }

    /// Returns a reference to the instruction with the given ID.
    ///
    /// # Panics
    ///
    /// Panics if the given ID is invalid or out of bounds.
    pub fn instruction(&self, id: InstructionId) -> &Instruction {
        self.instructions.get(id.0).unwrap()
    }

    /// Adds the given instruction to the block.
    ///
    /// # Panics
    ///
    /// Panics if the given ID is invalid or out of bounds.
    pub fn add_instruction(&mut self, inst: Instruction) {
        self.instructions.push(inst);
    }

    /// Allocates a new instruction and adds it to the block.
    ///
    /// # Panics
    ///
    /// Panics if the given ID is invalid or out of bounds.
    pub fn new_instruction(&mut self, kind: InstructionKind) -> InstructionId {
        let id = InstructionId(self.instructions.len());

        self.instructions.push(Instruction { id, kind });

        id
    }

    /// Determines if the block has a terminator.
    pub fn has_terminator(&self) -> bool {
        self.terminator.is_some()
    }

    /// Sets the terminator of the block if one has not been set already.
    pub fn set_terminator(&mut self, term: Terminator) {
        if !self.has_terminator() {
            self.terminator = Some(term);
        }
    }

    /// Sets the terminator of the block, whether one has been set already or not.
    pub fn set_terminator_full(&mut self, term: Terminator) {
        self.terminator = Some(term);
    }

    /// Declares a new local with the given declaration.
    pub fn declare(&mut self, local: Local, decl: Declaration) -> Local {
        self.new_instruction(InstructionKind::Declare { local, decl });

        local
    }

    /// Declares a new local with the given value.
    pub fn declare_value(&mut self, local: Local, value: Value) -> Local {
        self.declare(local, Declaration::Value(value))
    }

    /// Assigns a new value to an existing local.
    pub fn assign(&mut self, target: Local, value: Value) -> Local {
        self.new_instruction(InstructionKind::Store { target, value });

        target
    }

    /// Sets the terminator of the current block to an unconditional branch.
    pub fn branch(&mut self, block: BasicBlockId) {
        self.set_terminator(Terminator::Branch(block));
    }

    /// Sets the terminator of the current block to a conditional branch.
    pub fn conditional_branch(&mut self, cond: Local, then_block: BasicBlockId, else_block: BasicBlockId) {
        self.set_terminator(Terminator::ConditionalBranch {
            condition: cond,
            then_block,
            else_block,
        });
    }

    /// Returns the given value, if any is defined. Otherwise, returns `void`.
    pub fn return_any(&mut self, value: Option<Value>) {
        if let Some(value) = value {
            self.return_value(value);
        } else {
            self.return_void();
        }
    }

    /// Returns the `void` from the block.
    pub fn return_void(&mut self) {
        self.set_terminator(Terminator::Return(None));
    }

    /// Returns the the given value from the block.
    pub fn return_value(&mut self, value: Value) {
        self.set_terminator(Terminator::Return(Some(value)));
    }

    /// Returns the the given value from the block.
    pub fn unreachable(&mut self) {
        self.set_terminator(Terminator::Unreachable);
    }
}

impl std::fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.id)?;

        for stmt in &self.instructions {
            writeln!(f, "    {stmt}")?;
        }

        if let Some(terminator) = &self.terminator {
            writeln!(f, "    {terminator}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum Local {
    Variable(usize),
    Parameter(usize),
}

impl Local {
    pub fn var(id: usize) -> Self {
        Local::Variable(id)
    }

    pub fn param(id: usize) -> Self {
        Local::Parameter(id)
    }
}

impl std::fmt::Display for Local {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Local::Variable(id) | Local::Parameter(id) => write!(f, "#{id}"),
        }
    }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct InstructionId(usize);

impl std::fmt::Display for InstructionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "i.{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    pub id: InstructionId,
    pub kind: InstructionKind,
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InstructionKind {
    Declare { local: Local, decl: Declaration },
    Store { target: Local, value: Value },
}

impl std::fmt::Display for InstructionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Declare { local, decl } => write!(f, "{local} = {decl}"),
            Self::Store { target, value } => write!(f, "&{target} = {value}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Value(Value),
    IntCompare(IntComparison),
    FloatCompare(FloatComparison),
    Cast { operand: InstructionId, bits: u8 },
    Reference { id: Local },
}

impl std::fmt::Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Value(val) => val.fmt(f),
            Self::IntCompare(val) => val.fmt(f),
            Self::FloatCompare(val) => val.fmt(f),
            Self::Cast { operand, bits } => write!(f, "{operand} as i{bits}"),
            Self::Reference { id } => write!(f, "{id}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntComparison {
    Equal(Value, Value),
    NotEqual(Value, Value),
    UnsignedLessThan(Value, Value),
    UnsignedLessThanEqual(Value, Value),
    UnsignedGreaterThan(Value, Value),
    UnsignedGreaterThanEqual(Value, Value),
    SignedLessThan(Value, Value),
    SignedLessThanEqual(Value, Value),
    SignedGreaterThan(Value, Value),
    SignedGreaterThanEqual(Value, Value),
}

impl std::fmt::Display for IntComparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Equal(a, b) => write!(f, "{a} == {b}"),
            Self::NotEqual(a, b) => write!(f, "{a} != {b}"),
            Self::UnsignedLessThan(a, b) | Self::SignedLessThan(a, b) => write!(f, "{a} < {b}"),
            Self::UnsignedLessThanEqual(a, b) | Self::SignedLessThanEqual(a, b) => write!(f, "{a} <= {b}"),
            Self::UnsignedGreaterThan(a, b) | Self::SignedGreaterThan(a, b) => write!(f, "{a} > {b}"),
            Self::UnsignedGreaterThanEqual(a, b) | Self::SignedGreaterThanEqual(a, b) => write!(f, "{a} >= {b}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FloatComparison {
    Equal(Value, Value),
    NotEqual(Value, Value),
    LessThan(Value, Value),
    LessThanEqual(Value, Value),
    GreaterThan(Value, Value),
    GreaterThanEqual(Value, Value),
}

impl std::fmt::Display for FloatComparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Equal(a, b) => write!(f, "{a} == {b}"),
            Self::NotEqual(a, b) => write!(f, "{a} != {b}"),
            Self::LessThan(a, b) => write!(f, "{a} < {b}"),
            Self::LessThanEqual(a, b) => write!(f, "{a} <= {b}"),
            Self::GreaterThan(a, b) => write!(f, "{a} > {b}"),
            Self::GreaterThanEqual(a, b) => write!(f, "{a} >= {b}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Boolean { value: bool },
    Integer { bits: u8, signed: bool, value: i64 },
    Float { bits: u8, value: f64 },
    String { value: String },
    Reference { id: Local },
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Boolean { value } => write!(f, "{value}"),
            Self::Integer { bits, signed, value } => write!(f, "{value}_{}{bits}", if *signed { "i" } else { "u" }),
            Self::Float { bits, value } => write!(f, "{value}_f{bits}"),
            Self::String { value } => write!(f, "\"{value}\""),
            Self::Reference { id } => write!(f, "&{id}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Terminator {
    Return(Option<Value>),
    ConditionalBranch {
        condition: Local,
        then_block: BasicBlockId,
        else_block: BasicBlockId,
    },
    Branch(BasicBlockId),
    Unreachable,
}

impl std::fmt::Display for Terminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Return(value) => {
                if let Some(value) = value {
                    write!(f, "return {value}")
                } else {
                    write!(f, "return")
                }
            }
            Self::ConditionalBranch {
                condition,
                then_block,
                else_block,
            } => write!(f, "if {condition} goto {then_block} else {else_block}"),
            Self::Branch(block_id) => write!(f, "goto {block_id}"),
            Self::Unreachable => write!(f, "unreachable"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Struct { properties: Vec<Type> },
    Integer { bits: u8, signed: bool },
    Float { bits: u8 },
    Boolean,
    String { length: usize },
    Pointer,
    Void,
}
