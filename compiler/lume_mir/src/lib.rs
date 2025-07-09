use std::hash::Hash;

use indexmap::IndexMap;
use lume_infer::query::CallReference;

/// Represents a map of all functions within a compilation
/// module. Functions are identified by their unique ID,
/// which is referenced by later expressions, such as call sites.
#[derive(Default, Debug, Clone)]
pub struct ModuleMap {
    pub functions: IndexMap<FunctionId, Function>,
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
    pub fn new_function_id(&self, id: CallReference) -> FunctionId {
        FunctionId(id)
    }

    /// Returns a reference to the function with the given ID.
    ///
    /// # Panics
    ///
    /// Panics if the given ID is invalid or out of bounds.
    pub fn function(&self, id: FunctionId) -> &Function {
        self.functions.get(&id).unwrap()
    }

    /// Returns a mutable reference to the function with the given ID.
    ///
    /// # Panics
    ///
    /// Panics if the given ID is invalid or out of bounds.
    pub fn function_mut(&mut self, id: FunctionId) -> &mut Function {
        self.functions.get_mut(&id).unwrap()
    }

    /// Returns a reference to the function with the given name.
    ///
    /// # Panics
    ///
    /// Panics if the given name is invalid or missing.
    pub fn find_function(&self, name: CallReference) -> &Function {
        let id = self.new_function_id(name);

        self.function(id)
    }
}

impl std::fmt::Display for ModuleMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for func in self.functions.values() {
            write!(f, "{func}")?;
        }

        Ok(())
    }
}

/// Unique identifier for a function within a module.
///
/// [`FunctionId`]s refer to the specific HIR function which
/// is being referred to, so it can be used to optimize call
/// site expressions.
#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionId(CallReference);

impl std::fmt::Display for FunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            CallReference::Function(id) => write!(f, "F{:?}", id.0),
            CallReference::Method(id) => write!(f, "F{:?}", id.0),
        }
    }
}

/// Defines a function signature, such as parameter types and return type,
/// as well as any declared modifiers such as `external` or `inline`.
#[derive(Debug, Clone)]
pub struct Signature {
    /// Defines whether the function is externally defined or not.
    ///
    /// External functions can be defined outside of the current module,
    /// either in another module or in an external library.
    pub external: bool,

    /// Defines an ordered mapping of parameter types.
    ///
    /// If the parent function definition refers to an instance method,
    /// the first parameter will be the `self` parameter.
    pub parameters: Vec<Type>,
    pub return_type: Type,
}

impl std::fmt::Display for Signature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}) -> {}",
            self.parameters
                .iter()
                .enumerate()
                .map(|(idx, param)| format!("{param} #{idx}"))
                .collect::<Vec<String>>()
                .join(", "),
            self.return_type
        )
    }
}

impl Default for Signature {
    fn default() -> Self {
        Signature {
            external: false,
            parameters: Vec::new(),
            return_type: Type::Void,
        }
    }
}

/// Defines a function which is declared within the MIR module map.
///
/// Even though they're called "functions" in the MIR map, both
/// functions and methods are represented by this struct.
#[derive(Debug, Clone)]
pub struct Function {
    pub id: FunctionId,
    pub name: String,
    pub signature: Signature,

    pub registers: Registers,
    pub blocks: Vec<BasicBlock>,
    current_block: BasicBlockId,

    scope: Box<Scope>,
}

impl Function {
    pub fn new(id: FunctionId, name: String) -> Self {
        Function {
            id,
            name,
            registers: Registers::default(),
            blocks: Vec::new(),
            signature: Signature::default(),
            current_block: BasicBlockId(0),
            scope: Box::new(Scope::root_scope()),
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

    /// Allocates a new register with the given type and returns its ID.
    pub fn add_register(&mut self, ty: Type) -> RegisterId {
        self.registers.allocate(ty, self.current_block)
    }

    /// Declares a new local with the given declaration in the current block.
    pub fn declare(&mut self, ty: Type, decl: Declaration) -> RegisterId {
        let is_ref_type = ty.is_reference_type();

        if let Declaration::Operand(op) = decl {
            match op {
                Operand::Load { id } => id,
                _ if is_ref_type => {
                    let ptr = self.add_register(ty.clone());
                    self.current_block_mut().allocate_heap(ptr, ty);
                    self.current_block_mut().store(ptr, op);

                    ptr
                }
                _ => {
                    let ptr = self.add_register(ty.clone());
                    self.current_block_mut().allocate_stack(ptr, ty);
                    self.current_block_mut().store(ptr, op);

                    ptr
                }
            }
        } else {
            let ptr = self.add_register(ty.clone());
            self.current_block_mut().declare(ptr, decl);

            ptr
        }
    }

    /// Declares a new local with the given value in the current block.
    pub fn declare_value(&mut self, ty: Type, value: Operand) -> RegisterId {
        self.declare(ty, Declaration::Operand(value))
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
        if self.signature.external {
            writeln!(f, "@{} extern fn {:?} {}", self.id, self.name, self.signature)?;
            return writeln!(f);
        }

        writeln!(f, "@{} fn {:?} {} {{", self.id, self.name, self.signature)?;

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

/// Represents a basic block in the control flow graph.
#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlock {
    pub id: BasicBlockId,

    /// Defines all non-terminator instructions in the block.
    instructions: Vec<Instruction>,

    /// Defines the terminator of the block.
    ///
    /// Even though the terminator is optional, it is required for the block to be valid.
    /// If a block does not have a terminator, it will default to returning void.
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

    /// Declares a new stack-allocated register with the given value.
    pub fn declare(&mut self, register: RegisterId, decl: Declaration) {
        self.instructions.push(Instruction::Let { register, decl });
    }

    /// Declares a new stack-allocated register with the given type.
    pub fn allocate_stack(&mut self, register: RegisterId, ty: Type) {
        self.instructions.push(Instruction::StackAllocate { register, ty });
    }

    /// Declares a new heap-allocated register with the given type.
    pub fn allocate_heap(&mut self, register: RegisterId, ty: Type) {
        self.instructions.push(Instruction::HeapAllocate { register, ty });
    }

    /// Stores a value in an existing register.
    pub fn store(&mut self, target: RegisterId, value: Operand) {
        self.instructions.push(Instruction::Store { target, value });
    }

    /// Stores a value in a field of an existing register.
    pub fn store_field(&mut self, target: RegisterId, idx: usize, value: Operand) {
        self.instructions.push(Instruction::StoreField { target, idx, value });
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
    pub fn return_any(&mut self, value: Option<Operand>) {
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
    pub fn return_value(&mut self, value: Operand) {
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

    pub fn as_usize(&self) -> usize {
        self.0
    }
}

impl std::fmt::Display for RegisterId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

/// Defines a register within a block, which can hold a value of a specific type.
///
/// Registers cannot be altered after they are created, as they follow
/// the SSA (Single Static Assignment) principle. If register needs to be updated,
/// it should define an allocation which can be used to store the new value.
#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct Register {
    pub id: RegisterId,

    /// Defines the type of the register.
    pub ty: Type,

    /// Defines which block the register belongs to.
    pub block: Option<BasicBlockId>,
}

#[derive(Default, Debug, Clone)]
pub struct Registers {
    regs: Vec<Register>,
}

impl Registers {
    /// Gets a reference to the [`Register`] with the given ID.
    pub fn register(&self, id: RegisterId) -> &Register {
        &self.regs[id.0]
    }

    /// Gets a mutable reference to the [`Register`] with the given ID.
    pub fn register_mut(&mut self, id: RegisterId) -> &mut Register {
        &mut self.regs[id.0]
    }

    /// Gets a reference to the type of the [`Register`] with the given ID.
    pub fn register_ty(&self, id: RegisterId) -> &Type {
        &self.register(id).ty
    }

    /// Allocates a new register with the given type and block.
    pub fn allocate(&mut self, ty: Type, block: BasicBlockId) -> RegisterId {
        let id = RegisterId(self.regs.len());
        self.regs.push(Register {
            id,
            ty,
            block: Some(block),
        });

        id
    }

    /// Allocates a new parameter register with the given type.
    pub fn allocate_param(&mut self, ty: Type) -> RegisterId {
        let id = RegisterId(self.regs.len());
        self.regs.push(Register { id, ty, block: None });

        id
    }

    /// Iterates over all registers.
    pub fn iter(&self) -> impl Iterator<Item = (RegisterId, &Register)> {
        self.regs.iter().enumerate().map(|(i, r)| (RegisterId(i), r))
    }
}

/// Represents a standalone instruction within a basic block.
///
/// Instructions themselves cannot be referenced by other instructions - only registers
/// they declare can be referenced.
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    /// Declares an SSA register within the current function.
    Let { register: RegisterId, decl: Declaration },

    /// Declares a stack-allocated register within the current function.
    StackAllocate { register: RegisterId, ty: Type },

    /// Declares a heap-allocated register within the current function.
    HeapAllocate { register: RegisterId, ty: Type },

    /// Stores the value into the target register.
    Store { target: RegisterId, value: Operand },

    /// Stores the value into the field of an target register.
    StoreField {
        target: RegisterId,
        idx: usize,
        value: Operand,
    },
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Let { register, decl } => write!(f, "let {register} = {decl}"),
            Self::StackAllocate { register, ty } => write!(f, "{register} = alloc {ty}"),
            Self::HeapAllocate { register, ty } => write!(f, "{register} = malloc {ty}"),
            Self::Store { target, value } => write!(f, "*{target} = {value}"),
            Self::StoreField { target, idx, value } => write!(f, "*{target}[{idx}] = {value}"),
        }
    }
}

/// Represents the right-hand side of a declaration instruction.
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    /// Represents an operand value.
    Operand(Operand),

    /// Represents an inline cast from a target register to another type.
    Cast { operand: RegisterId, bits: u8 },

    /// Defines a call to an intrinsic function.
    Intrinsic { name: Intrinsic, args: Vec<Operand> },

    /// Represents a reference to an existing register.
    Reference { id: RegisterId },

    /// Represents a memory load from an existing register, holding a pointer.
    Load { id: RegisterId },

    /// Represents a call to a function.
    Call { func_id: FunctionId, args: Vec<Operand> },
}

impl std::fmt::Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Operand(op) => op.fmt(f),
            Self::Cast { operand, bits } => write!(f, "{operand} as i{bits}"),
            Self::Intrinsic { name, args } => write!(
                f,
                "{name}({})",
                args.iter().map(|arg| format!("{arg}")).collect::<Vec<_>>().join(", ")
            ),
            Self::Reference { id } => write!(f, "{id}"),
            Self::Load { id } => write!(f, "&{id}"),
            Self::Call { func_id, args } => write!(
                f,
                "(call {func_id})({})",
                args.iter().map(|arg| format!("{arg}")).collect::<Vec<_>>().join(", ")
            ),
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
    IntAnd { bits: u8, signed: bool },
    IntOr { bits: u8, signed: bool },
    IntXor { bits: u8, signed: bool },
    BooleanEq,
    BooleanNe,
    BooleanAnd,
    BooleanOr,
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
            Self::IntAnd { .. } => write!(f, "&"),
            Self::IntOr { .. } => write!(f, "|"),
            Self::IntXor { .. } => write!(f, "^"),
            Self::BooleanAnd { .. } => write!(f, "&&"),
            Self::BooleanOr { .. } => write!(f, "||"),
        }
    }
}

/// Represents an operand to a call expression.
///
/// Not all values can be used as operands, which means they
/// must be declared as a stack- or heap-allocated register.
#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    /// Represents a literal boolean value.
    Boolean { value: bool },

    /// Represents a literal integer value.
    Integer { bits: u8, signed: bool, value: i64 },

    /// Represents a literal floating-point value.
    Float { bits: u8, value: f64 },

    /// Represents a literal string value.
    String { value: String },

    /// Represents a loaded value from an existing register.
    Load { id: RegisterId },

    /// Represents a loaded value from an existing register.
    LoadField { target: RegisterId, field: usize },

    /// Represents a reference to an existing register.
    Reference { id: RegisterId },
}

impl Operand {
    /// Gets the bitsize of the operand.
    #[expect(clippy::cast_possible_truncation, clippy::missing_panics_doc)]
    pub fn bitsize(&self) -> u8 {
        match self {
            Self::Boolean { .. } => 1,
            Self::Integer { bits, .. } | Self::Float { bits, .. } => *bits,
            Self::Reference { .. } | Self::String { .. } => std::mem::size_of::<*const u32>() as u8 * 8,
            Self::Load { .. } | Self::LoadField { .. } => panic!("cannot get bitsize of load operand"),
        }
    }
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Boolean { value } => write!(f, "{value}"),
            Self::Integer { bits, signed, value } => write!(f, "{value}_{}{bits}", if *signed { "i" } else { "u" }),
            Self::Float { bits, value } => write!(f, "{value}_f{bits}"),
            Self::Reference { id } => write!(f, "{id}"),
            Self::Load { id } => write!(f, "*{id}"),
            Self::LoadField { target, field } => write!(f, "&{target}.{field}"),
            Self::String { value } => write!(f, "\"{value}\""),
        }
    }
}

/// Represents a terminator of a block, which defines how control flow is
/// transferred.
#[derive(Debug, Clone, PartialEq)]
pub enum Terminator {
    /// Returns the given value, if any, from the current function.
    ///
    /// If no value is provided, the function returns `void`.
    Return(Option<Operand>),

    /// Unconditionally transfers control flow to the given block.
    Branch(BasicBlockId),

    /// Conditionally transfers control flow to one of the given blocks.
    ConditionalBranch {
        condition: RegisterId,
        then_block: BasicBlockId,
        else_block: BasicBlockId,
    },

    /// Defines the terminator as being unreachable, which may happen when
    /// calling `noret` functions or panic.
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

/// Defines a type within the MIR.
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// Represents a struct type with zero-or-more properties.
    Struct { properties: Vec<Type> },

    /// Defines an integer type with a specified number of bits and signedness.
    Integer { bits: u8, signed: bool },

    /// Defines a floating-point type with a specified number of bits.
    Float { bits: u8 },

    /// Defines a boolean type.
    Boolean,

    /// Defines a string type.
    ///
    /// While the [`Type::String`] type is a separate type, it is most often
    /// lowering into a pointer to a [`Type::Integer`] type.
    String,

    /// Defines a pointer type.
    Pointer,

    /// Defines a void type.
    Void,
}

impl Type {
    pub fn is_reference_type(&self) -> bool {
        matches!(self, Type::Struct { .. } | Type::String | Type::Pointer)
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Struct { properties } => write!(
                f,
                "{{{}}}",
                properties
                    .iter()
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Integer { bits, signed } => write!(f, "{}{bits}", if *signed { "i" } else { "u" }),
            Self::Float { bits } => write!(f, "f{bits}"),
            Self::Boolean => write!(f, "bool"),
            Self::String => write!(f, "string"),
            Self::Pointer => write!(f, "ptr"),
            Self::Void => write!(f, "void"),
        }
    }
}
