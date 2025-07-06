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

#[derive(Debug, Clone)]
pub struct Function {
    pub id: FunctionId,
    pub name: String,
    pub external: bool,

    pub parameters: Vec<Type>,
    pub return_type: Type,

    pub registers: Registers,
    pub blocks: Vec<BasicBlock>,
    current_block: BasicBlockId,

    pub(crate) scope: Box<Scope>,
}

impl Function {
    pub fn new(id: FunctionId, name: String) -> Self {
        Function {
            id,
            name,
            registers: Registers::default(),
            blocks: Vec::new(),
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

    /// Allocates a new register and returns its ID.
    pub fn add_register(&mut self, ty: Type) -> RegisterId {
        self.registers.allocate(ty, self.current_block)
    }

    /// Declares a new local with the given declaration in the current block.
    pub fn declare(&mut self, ty: Type, decl: Declaration) -> RegisterId {
        let register = self.add_register(ty);

        self.current_block_mut().declare(register, decl);

        register
    }

    /// Declares a new local with the given value in the current block.
    pub fn declare_value(&mut self, ty: Type, value: Value) -> RegisterId {
        self.declare(ty, Declaration::Value(value))
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

    instructions: Vec<Instruction>,
    terminator: Option<Terminator>,
}

impl BasicBlock {
    pub fn new(id: BasicBlockId) -> Self {
        BasicBlock {
            id,
            instructions: Vec::new(),
            terminator: None,
        }
    }

    /// Gets the instructions of the block.
    pub fn instructions(&self) -> impl Iterator<Item = &Instruction> {
        self.instructions.iter()
    }

    /// Gets the terminator of the block if one has been set.
    pub fn terminator(&self) -> Option<&Terminator> {
        self.terminator.as_ref()
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

    /// Declares a new register with an initial value.
    pub fn declare(&mut self, register: RegisterId, decl: Declaration) {
        self.instructions.push(Instruction::Declare { register, decl });
    }

    /// Assigns a new value to an existing register.
    pub fn assign(&mut self, target: RegisterId, value: Value) {
        self.instructions.push(Instruction::Store { target, value });
    }

    /// Sets the terminator of the current block to an unconditional branch.
    pub fn branch(&mut self, block: BasicBlockId) {
        self.set_terminator(Terminator::Branch(block));
    }

    /// Sets the terminator of the current block to a conditional branch.
    pub fn conditional_branch(&mut self, cond: RegisterId, then_block: BasicBlockId, else_block: BasicBlockId) {
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
pub struct RegisterId(usize);

impl RegisterId {
    pub fn param(index: usize) -> Self {
        RegisterId(index)
    }
}

impl std::fmt::Display for RegisterId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct Register {
    pub ty: Type,
    pub block: BasicBlockId,
}

#[derive(Default, Debug, Clone)]
pub struct Registers {
    regs: Vec<Register>,
}

impl Registers {
    pub fn register(&self, id: RegisterId) -> &Register {
        &self.regs[id.0]
    }

    pub fn register_mut(&mut self, id: RegisterId) -> &mut Register {
        &mut self.regs[id.0]
    }

    pub fn allocate(&mut self, ty: Type, block: BasicBlockId) -> RegisterId {
        let id = RegisterId(self.regs.len());
        self.regs.push(Register { ty, block });
        id
    }

    pub fn iter(&self) -> impl Iterator<Item = (RegisterId, &Register)> {
        self.regs.iter().enumerate().map(|(i, r)| (RegisterId(i), r))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Declare { register: RegisterId, decl: Declaration },
    Store { target: RegisterId, value: Value },
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Declare { register, decl } => write!(f, "{register} = {decl}"),
            Self::Store { target, value } => write!(f, "&{target} = {value}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Value(Value),
    Cast { operand: RegisterId, bits: u8 },
    Intrinsic { name: Intrinsic, args: Vec<RegisterId> },
    Reference { id: RegisterId },
}

impl Declaration {
    pub fn is_pointer_type(&self) -> bool {
        match self {
            Self::Value(value) => value.is_pointer_type(),
            Self::Reference { .. } => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Value(val) => val.fmt(f),
            Self::Cast { operand, bits } => write!(f, "{operand} as i{bits}"),
            Self::Intrinsic { name, args } => write!(
                f,
                "{name}({})",
                args.iter().map(|arg| format!("{arg}")).collect::<Vec<_>>().join(", ")
            ),
            Self::Reference { id } => write!(f, "{id}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Intrinsic {
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
    BooleanEq,
    BooleanNe,
}

impl std::fmt::Display for Intrinsic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::FloatEq { .. } | Self::IntEq { .. } | Self::BooleanEq => write!(f, "=="),
            Self::FloatNe { .. } | Self::IntNe { .. } | Self::BooleanNe => write!(f, "!="),
            Self::FloatLe { .. } | Self::IntLe { .. } => write!(f, "<"),
            Self::FloatLt { .. } | Self::IntLt { .. } => write!(f, "<="),
            Self::FloatGe { .. } | Self::IntGe { .. } => write!(f, ">"),
            Self::FloatGt { .. } | Self::IntGt { .. } => write!(f, ">="),
            Self::FloatAdd { .. } | Self::IntAdd { .. } => write!(f, "+"),
            Self::FloatSub { .. } | Self::IntSub { .. } => write!(f, "-"),
            Self::FloatMul { .. } | Self::IntMul { .. } => write!(f, "*"),
            Self::FloatDiv { .. } | Self::IntDiv { .. } => write!(f, "/"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Boolean { value: bool },
    Integer { bits: u8, signed: bool, value: i64 },
    Float { bits: u8, value: f64 },
    String { value: String },
    Reference { id: RegisterId },
}

impl Value {
    pub fn is_pointer_type(&self) -> bool {
        matches!(self, Self::String { .. })
    }
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
        condition: RegisterId,
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

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Struct { properties: Vec<Type> },
    Integer { bits: u8, signed: bool },
    Float { bits: u8 },
    Boolean,
    String,
    Pointer,
    Void,
}
