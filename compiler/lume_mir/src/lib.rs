use std::hash::Hash;

use indexmap::IndexMap;
use lume_span::Interned;
use lume_type_metadata::{StaticMetadata, TypeMetadata};

/// Represents a map of all functions within a compilation
/// module. Functions are identified by their unique ID,
/// which is referenced by later expressions, such as call sites.
#[derive(Default, Debug, Clone)]
pub struct ModuleMap {
    pub metadata: StaticMetadata,
    pub functions: IndexMap<FunctionId, Function>,
}

impl ModuleMap {
    /// Creates a new empty [`ModuleMap`].
    pub fn new(metadata: StaticMetadata) -> Self {
        Self {
            metadata,
            ..Default::default()
        }
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
/// [`FunctionId`]s refer to the specific MIR function which
/// is being referred to, so it can be used to optimize call
/// site expressions.
#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionId(pub usize);

impl std::fmt::Display for FunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "F{:?}", self.0)
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
            return_type: Type::void(),
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

        if let Declaration::Operand(op) = &decl {
            match op {
                Operand::Load { id } => *id,
                _ if is_ref_type => {
                    let ptr = self.add_register(ty.clone());
                    self.current_block_mut().allocate(ptr, ty);
                    self.current_block_mut().store(ptr, op.clone());

                    ptr
                }
                _ => {
                    let ptr = self.add_register(ty.clone());
                    self.current_block_mut().declare(ptr, decl);

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

    /// Defines all the input registers, which the block takes
    /// as input from predecessor blocks.
    pub parameters: Vec<RegisterId>,

    /// Defines all non-terminator instructions in the block.
    instructions: Vec<Instruction>,

    /// Defines the terminator of the block.
    ///
    /// Even though the terminator is optional, it is required for the block to be valid.
    /// If a block does not have a terminator, it will default to returning void.
    terminator: Option<Terminator>,

    /// Gets all the predecessor blocks, which branch to this block.
    predecessors: Vec<BasicBlockId>,

    /// Gets all the successor blocks, which this block branches to.
    successors: Vec<BasicBlockId>,
}

impl BasicBlock {
    pub fn new(id: BasicBlockId) -> Self {
        BasicBlock {
            id,
            parameters: Vec::new(),
            instructions: Vec::new(),
            terminator: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        }
    }

    /// Gets the instructions of the block.
    pub fn instructions(&self) -> impl Iterator<Item = &Instruction> {
        self.instructions.iter()
    }

    /// Gets the instructions of the block.
    pub fn instructions_mut(&mut self) -> impl Iterator<Item = &mut Instruction> {
        self.instructions.iter_mut()
    }

    /// Gets the terminator of the block if one has been set.
    pub fn terminator(&self) -> Option<&Terminator> {
        self.terminator.as_ref()
    }

    /// Gets the terminator of the block if one has been set.
    pub fn terminator_mut(&mut self) -> Option<&mut Terminator> {
        self.terminator.as_mut()
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

    /// Gets an iterator for all the predecessors of the current block.
    pub fn predecessors(&self) -> impl Iterator<Item = BasicBlockId> {
        self.predecessors.iter().copied()
    }

    /// Gets an iterator for all the successors of the current block.
    pub fn successors(&self) -> impl Iterator<Item = BasicBlockId> {
        self.successors.iter().copied()
    }

    /// Pushes the given block to be a predecessor to the current block.
    pub fn push_predecessor(&mut self, block: BasicBlockId) {
        self.predecessors.reserve_exact(1);
        self.predecessors.push(block);
    }

    /// Pushes the given block to be a successor to the current block.
    pub fn push_successor(&mut self, block: BasicBlockId) {
        self.successors.reserve_exact(1);
        self.successors.push(block);
    }

    /// Declares a new stack-allocated register with the given value.
    pub fn declare(&mut self, register: RegisterId, decl: Declaration) {
        self.instructions.push(Instruction::Let { register, decl });
    }

    /// Assigns a new value to an existing register.
    pub fn assign(&mut self, target: RegisterId, value: Operand) {
        self.instructions.push(Instruction::Assign { target, value });
    }

    /// Declares a new heap-allocated register with the given type.
    pub fn allocate(&mut self, register: RegisterId, ty: Type) {
        self.instructions.push(Instruction::Allocate { register, ty });
    }

    /// Stores a value in an existing register.
    pub fn store(&mut self, target: RegisterId, value: Operand) {
        self.instructions.push(Instruction::Store { target, value });
    }

    /// Stores a value in a field of an existing register.
    pub fn store_field(&mut self, target: RegisterId, offset: usize, value: Operand) {
        self.instructions
            .push(Instruction::StoreField { target, offset, value });
    }

    /// Sets the terminator of the current block to an unconditional branch.
    pub fn branch(&mut self, block: BasicBlockId) {
        self.set_terminator(Terminator::Branch(BlockBranchSite::new(block)));
    }

    /// Sets the terminator of the current block to a conditional branch.
    pub fn conditional_branch(&mut self, cond: RegisterId, then_block: BasicBlockId, else_block: BasicBlockId) {
        self.set_terminator(Terminator::ConditionalBranch {
            condition: cond,
            then_block: BlockBranchSite::new(then_block),
            else_block: BlockBranchSite::new(else_block),
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
        write!(f, "{}", self.id)?;

        if !self.parameters.is_empty() {
            write!(
                f,
                "({})",
                self.parameters
                    .iter()
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
        }

        writeln!(
            f,
            ":  {}",
            self.predecessors
                .iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )?;

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
    pub fn new(index: usize) -> Self {
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
    pub fn iter(&self) -> impl Iterator<Item = &Register> {
        self.regs.iter()
    }

    /// Iterates over all registers.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Register> {
        self.regs.iter_mut()
    }

    /// Replaces all the existing registers within the function.
    pub fn replace_all(&mut self, replacement: impl Iterator<Item = Register>) {
        self.regs = replacement.collect();
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

    /// Assigns the value into the target register.
    Assign { target: RegisterId, value: Operand },

    /// Declares a heap-allocated register within the current function.
    Allocate { register: RegisterId, ty: Type },

    /// Stores the value into the target register.
    Store { target: RegisterId, value: Operand },

    /// Stores the value into the field of an target register.
    StoreField {
        target: RegisterId,
        offset: usize,
        value: Operand,
    },
}

impl Instruction {
    pub fn register_def(&self) -> Option<RegisterId> {
        match self {
            Self::Let { register, .. } | Self::Allocate { register, .. } => Some(*register),
            Self::Assign { .. } | Self::Store { .. } | Self::StoreField { .. } => None,
        }
    }

    pub fn register_refs(&self) -> Vec<RegisterId> {
        match self {
            Self::Let { decl, .. } => decl.register_refs(),
            Self::Assign { target, value } => {
                let mut refs = vec![*target];
                refs.extend(value.register_refs());

                refs
            }
            Self::Allocate { .. } => Vec::new(),
            Self::Store { target, .. } | Self::StoreField { target, .. } => vec![*target],
        }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Let { register, decl } => write!(f, "let {register} = {decl}"),
            Self::Assign { target, value } => write!(f, "{target} = {value}"),
            Self::Allocate { register, ty } => write!(f, "{register} = alloc {ty}"),
            Self::Store { target, value } => write!(f, "*{target} = {value}"),
            Self::StoreField { target, offset, value } => write!(f, "*{target}[+x{offset}] = {value}"),
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

impl Declaration {
    pub fn register_refs(&self) -> Vec<RegisterId> {
        match self {
            Self::Operand(op) => op.register_refs(),
            Self::Cast { operand, .. } => vec![*operand],
            Self::Intrinsic { args, .. } | Self::Call { args, .. } => {
                args.iter().flat_map(Operand::register_refs).collect()
            }
            Self::Reference { id } | Self::Load { id } => vec![*id],
        }
    }
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
    Metadata { metadata: TypeMetadata },
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
            Self::Metadata { metadata } => write!(f, "metadata {}", metadata.full_name),
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
    String { value: Interned<String> },

    /// Represents a loaded value from an existing register.
    Load { id: RegisterId },

    /// Represents a loaded value from an existing register.
    LoadField {
        target: RegisterId,
        offset: usize,
        index: usize,
    },

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

    pub fn register_refs(&self) -> Vec<RegisterId> {
        match self {
            Self::Load { id } | Self::Reference { id } => {
                vec![*id]
            }
            Self::LoadField { target, .. } => {
                vec![*target]
            }
            Self::Boolean { .. } | Self::Integer { .. } | Self::Float { .. } | Self::String { .. } => Vec::new(),
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
            Self::LoadField { target, offset, .. } => write!(f, "&{target}[+0x{offset}]"),
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
    Branch(BlockBranchSite),

    /// Conditionally transfers control flow to one of the given blocks.
    ConditionalBranch {
        condition: RegisterId,
        then_block: BlockBranchSite,
        else_block: BlockBranchSite,
    },

    /// Defines the terminator as being unreachable, which may happen when
    /// calling `noret` functions or panic.
    Unreachable,
}

impl Terminator {
    pub fn register_refs(&self) -> Vec<RegisterId> {
        match self {
            Self::Return(operand) => {
                if let Some(operand) = operand {
                    operand.register_refs()
                } else {
                    Vec::new()
                }
            }
            Self::ConditionalBranch {
                condition,
                then_block,
                else_block,
            } => {
                let mut refs = vec![*condition];
                refs.extend(&then_block.arguments);
                refs.extend(&else_block.arguments);

                refs
            }
            Self::Branch(site) => site.arguments.clone(),
            Self::Unreachable => Vec::new(),
        }
    }
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

/// Represents a call site for a branch instruction, branching to a block
/// with a set of arguments.
#[derive(Debug, Clone, PartialEq)]
pub struct BlockBranchSite {
    pub block: BasicBlockId,
    pub arguments: Vec<RegisterId>,
}

impl BlockBranchSite {
    pub fn new(block: BasicBlockId) -> Self {
        Self {
            block,
            arguments: Vec::new(),
        }
    }

    pub fn arg_operands(&self) -> impl Iterator<Item = Operand> {
        self.arguments.iter().map(|arg| Operand::Reference { id: *arg })
    }
}

impl std::fmt::Display for BlockBranchSite {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.block)?;

        if !self.arguments.is_empty() {
            write!(
                f,
                "({})",
                self.arguments
                    .iter()
                    .map(|arg| format!("{arg}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
        }

        Ok(())
    }
}

pub type TypeId = lume_types::TypeId;

pub type TypeRef = lume_types::TypeRef;

/// Defines a type within the MIR.
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub id: TypeRef,
    pub kind: TypeKind,
}

impl Type {
    pub fn void() -> Self {
        Self {
            id: TypeRef::void(),
            kind: TypeKind::Void,
        }
    }

    pub fn boolean() -> Self {
        Self {
            id: TypeRef::bool(),
            kind: TypeKind::Boolean,
        }
    }

    pub fn string() -> Self {
        Self {
            id: TypeRef::string(),
            kind: TypeKind::String,
        }
    }

    pub fn pointer(elemental: Type) -> Self {
        Self {
            id: TypeRef::pointer(elemental.id.clone()),
            kind: TypeKind::Pointer {
                elemental: Box::new(elemental),
            },
        }
    }

    pub fn integer(bits: u8, signed: bool) -> Self {
        match (bits, signed) {
            (8, true) => Self::i8(),
            (8, false) => Self::u8(),
            (16, true) => Self::i16(),
            (16, false) => Self::u16(),
            (32, true) => Self::i32(),
            (32, false) => Self::u32(),
            (64, true) => Self::i64(),
            (64, false) => Self::u64(),
            _ => unreachable!(),
        }
    }

    pub fn i8() -> Self {
        Self {
            id: TypeRef::i8(),
            kind: TypeKind::Integer { bits: 8, signed: true },
        }
    }

    pub fn u8() -> Self {
        Self {
            id: TypeRef::u8(),
            kind: TypeKind::Integer { bits: 8, signed: false },
        }
    }

    pub fn i16() -> Self {
        Self {
            id: TypeRef::i16(),
            kind: TypeKind::Integer { bits: 16, signed: true },
        }
    }

    pub fn u16() -> Self {
        Self {
            id: TypeRef::u16(),
            kind: TypeKind::Integer {
                bits: 16,
                signed: false,
            },
        }
    }

    pub fn i32() -> Self {
        Self {
            id: TypeRef::i32(),
            kind: TypeKind::Integer { bits: 32, signed: true },
        }
    }

    pub fn u32() -> Self {
        Self {
            id: TypeRef::u32(),
            kind: TypeKind::Integer {
                bits: 32,
                signed: false,
            },
        }
    }

    pub fn i64() -> Self {
        Self {
            id: TypeRef::i64(),
            kind: TypeKind::Integer { bits: 64, signed: true },
        }
    }

    pub fn u64() -> Self {
        Self {
            id: TypeRef::u64(),
            kind: TypeKind::Integer {
                bits: 64,
                signed: false,
            },
        }
    }

    pub fn float(bits: u8) -> Self {
        match bits {
            32 => Self::f32(),
            64 => Self::f64(),
            _ => unreachable!(),
        }
    }

    pub fn f32() -> Self {
        Self {
            id: TypeRef::f32(),
            kind: TypeKind::Float { bits: 32 },
        }
    }

    pub fn f64() -> Self {
        Self {
            id: TypeRef::f64(),
            kind: TypeKind::Float { bits: 64 },
        }
    }

    pub fn structure(id: TypeRef, props: Vec<Type>) -> Self {
        Self {
            id,
            kind: TypeKind::Struct { properties: props },
        }
    }

    pub fn union(id: TypeRef, cases: Vec<Type>) -> Self {
        Self {
            id,
            kind: TypeKind::Union { cases },
        }
    }

    pub fn is_reference_type(&self) -> bool {
        self.kind.is_reference_type()
    }

    pub fn bytesize(&self) -> usize {
        match &self.kind {
            TypeKind::Struct { properties } => properties.iter().map(Type::bytesize).sum(),
            TypeKind::Union { cases } => {
                let discriminator_size = Self::i8().bytesize();
                let max_case_size = cases.iter().map(Type::bytesize).max().unwrap_or_default();

                discriminator_size + max_case_size
            }
            TypeKind::Integer { bits, .. } | TypeKind::Float { bits } => (*bits / 8) as usize,
            TypeKind::Boolean => 1,
            TypeKind::String | TypeKind::Pointer { .. } | TypeKind::Metadata { .. } => {
                std::mem::size_of::<*const u32>()
            }
            TypeKind::Void => 0,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    /// Represents a struct type with zero-or-more properties.
    Struct { properties: Vec<Type> },

    /// Represents a union type with zero-or-more cases.
    Union { cases: Vec<Type> },

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
    Pointer { elemental: Box<Type> },

    /// Defines a metadata type.
    Metadata { inner: TypeMetadata },

    /// Defines a void type.
    Void,
}

impl TypeKind {
    pub fn is_reference_type(&self) -> bool {
        matches!(self, Self::Struct { .. } | Self::String | Self::Pointer { .. })
    }
}

impl std::fmt::Display for TypeKind {
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
            Self::Union { cases } => write!(
                f,
                "(u8, {})",
                cases
                    .iter()
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Integer { bits, signed } => write!(f, "{}{bits}", if *signed { "i" } else { "u" }),
            Self::Float { bits } => write!(f, "f{bits}"),
            Self::Boolean => write!(f, "bool"),
            Self::String => write!(f, "string"),
            Self::Pointer { elemental } => write!(f, "ptr {elemental}"),
            Self::Metadata { inner } => write!(f, "metadata {}", inner.full_name),
            Self::Void => write!(f, "void"),
        }
    }
}
