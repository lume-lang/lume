use std::hash::Hash;

use indexmap::{IndexMap, IndexSet};
use lume_session::Package;
use lume_span::source::Location;
use lume_span::{Interned, NodeId};
use lume_type_metadata::{StaticMetadata, TypeMetadata};
use lume_types::TypeRef;
use serde::{Deserialize, Serialize};

pub const POINTER_SIZE: usize = std::mem::size_of::<*const u32>();

/// Represents a map of all functions within a compilation
/// module. Functions are identified by their unique ID,
/// which is referenced by later expressions, such as call sites.
#[derive(Serialize, Deserialize, Default, Debug, Clone)]
pub struct ModuleMap {
    pub package: Package,
    pub metadata: StaticMetadata,
    pub functions: IndexMap<NodeId, Function>,
}

impl ModuleMap {
    /// Creates a new empty [`ModuleMap`].
    pub fn new(package: Package, metadata: StaticMetadata) -> Self {
        Self {
            package,
            metadata,
            ..Default::default()
        }
    }

    /// Returns a reference to the function with the given ID.
    ///
    /// # Panics
    ///
    /// Panics if the given ID is invalid or out of bounds.
    pub fn function(&self, id: NodeId) -> &Function {
        self.functions.get(&id).unwrap()
    }

    /// Returns a mutable reference to the function with the given ID.
    ///
    /// # Panics
    ///
    /// Panics if the given ID is invalid or out of bounds.
    pub fn function_mut(&mut self, id: NodeId) -> &mut Function {
        self.functions.get_mut(&id).unwrap()
    }

    /// Merges the current [`ModuleMap`] into the other given map.
    pub fn merge_into(self, dest: &mut ModuleMap) {
        self.metadata.merge_into(&mut dest.metadata);

        for (id, func) in self.functions {
            if !dest.functions.contains_key(&id) {
                dest.functions.insert(id, func);
            }
        }
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

/// Defines a function signature, such as parameter types and return type,
/// as well as any declared modifiers such as `external` or `inline`.
#[derive(Serialize, Deserialize, Default, Debug, Clone, PartialEq, Eq)]
pub struct Signature {
    /// Defines whether the function is externally defined or not.
    ///
    /// External functions can be defined outside of the current module,
    /// either in another module or in an external library.
    pub external: bool,

    /// Defines whether the function supports a variable amount of
    /// arguments.
    pub vararg: bool,

    /// Defines an ordered mapping of parameter types.
    ///
    /// If the parent function definition refers to an instance method,
    /// the first parameter will be the `self` parameter.
    pub parameters: Vec<Parameter>,
    pub return_type: Type,

    /// Determines whether the current function is a drop-method.
    pub is_dropper: bool,
}

impl std::fmt::Display for Signature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}) -> {}",
            self.parameters
                .iter()
                .enumerate()
                .map(|(idx, param)| {
                    let is_last = idx == self.parameters.len() - 1;

                    format!("{}{param} #{idx}", if is_last && self.vararg { "..." } else { "" })
                })
                .collect::<Vec<String>>()
                .join(", "),
            self.return_type
        )
    }
}

/// Defines a parameter in a function signature.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub name: Interned<String>,
    pub ty: Type,
    pub type_ref: TypeRef,
    pub location: Location,
}

impl std::fmt::Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}: {}", self.name, self.ty))
    }
}

/// Defines a function which is declared within the MIR module map.
///
/// Even though they're called "functions" in the MIR map, both
/// functions and methods are represented by this struct.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Function {
    pub id: NodeId,
    pub name: String,
    pub signature: Signature,

    pub registers: Registers,
    pub slots: IndexMap<SlotId, Type>,
    pub blocks: IndexMap<BasicBlockId, BasicBlock>,
    current_block: BasicBlockId,

    #[serde(skip)]
    scope: Box<Scope>,

    pub location: Location,
}

impl Function {
    pub fn new(id: NodeId, name: String, location: Location) -> Self {
        Function {
            id,
            name,
            registers: Registers::default(),
            slots: IndexMap::new(),
            blocks: IndexMap::new(),
            signature: Signature::default(),
            current_block: BasicBlockId(0),
            scope: Box::new(Scope::root_scope()),
            location,
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
        self.blocks.get(&id).unwrap()
    }

    /// Returns a mutable reference to the basic block with the given ID.
    ///
    /// # Panics
    ///
    /// Panics if the given ID is invalid or out of bounds.
    pub fn block_mut(&mut self, id: BasicBlockId) -> &mut BasicBlock {
        self.blocks.get_mut(&id).unwrap()
    }

    /// Allocates a new basic block and returns its ID.
    pub fn new_block(&mut self) -> BasicBlockId {
        let id = BasicBlockId(self.blocks.len());

        self.blocks.insert(id, BasicBlock::new(id));

        id
    }

    /// Allocates a new basic block, sets it as the active block, and returns
    /// its ID.
    pub fn new_active_block(&mut self) -> BasicBlockId {
        let block = self.new_block();

        self.set_current_block(block);

        block
    }

    /// Adds a new register with the given type to the block with the given ID.
    ///
    /// # Panics
    ///
    /// Panics if the given ID is invalid or out of bounds.
    pub fn add_block_parameter(&mut self, block: BasicBlockId, ty: Type) -> RegisterId {
        let reg = self.add_register_in(block, ty);
        self.block_mut(block).parameters.push(reg);

        reg
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
        self.add_register_in(self.current_block, ty)
    }

    /// Allocates a new register with the given type and returns its ID.
    pub fn add_register_in(&mut self, block: BasicBlockId, ty: Type) -> RegisterId {
        self.registers.allocate(ty, block)
    }

    /// Allocates a new slot with the given type and returns its ID.
    pub fn add_slot(&mut self, ty: Type) -> SlotId {
        let slot = SlotId::new(self.slots.len());
        self.slots.insert(slot, ty);

        slot
    }

    /// Declares a new local with the given declaration in the current block.
    pub fn declare(&mut self, ty: Type, decl: Declaration) -> RegisterId {
        if let DeclarationKind::Operand(op) = decl.kind.as_ref()
            && let OperandKind::Load { id } | OperandKind::Reference { id } = &op.kind
        {
            *id
        } else {
            self.declare_raw(ty, decl)
        }
    }

    /// Declares a new local with the given value in the current block.
    pub fn declare_value(&mut self, ty: Type, value: Operand) -> RegisterId {
        self.declare(ty, Declaration {
            location: value.location.clone(),
            kind: Box::new(DeclarationKind::Operand(value)),
        })
    }

    /// Declares a new local with the given declaration in the current block.
    pub fn declare_raw(&mut self, ty: Type, decl: Declaration) -> RegisterId {
        let loc = decl.location.clone();

        let ptr = self.add_register(ty.clone());
        self.current_block_mut().declare(ptr, decl, ty, loc);

        ptr
    }

    /// Declares a new local with the given declaration in the current block.
    pub fn declare_value_raw(&mut self, ty: Type, value: Operand) -> RegisterId {
        self.declare_raw(ty, Declaration {
            location: value.location.clone(),
            kind: Box::new(DeclarationKind::Operand(value)),
        })
    }

    /// Creates a new stack-allocated slot within the function.
    pub fn alloc_slot(&mut self, ty: Type, loc: Location) -> SlotId {
        let slot = self.add_slot(ty.clone());
        self.current_block_mut().create_slot(slot, ty, loc);

        slot
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

        for block in self.blocks.values() {
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

impl Default for Scope {
    fn default() -> Self {
        Self::root_scope()
    }
}

#[derive(Serialize, Deserialize, Hash, Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct BasicBlockId(pub usize);

impl std::fmt::Display for BasicBlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "B{}", self.0)
    }
}

/// Defines how a block is used in the context of the function.
#[derive(Serialize, Deserialize, Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlockUsage {
    /// The block is used as a regular block, which can be jumped to and from.
    #[default]
    Regular,

    /// The block is unused, either because it was optimized away or because it
    /// was never used in the original MIR.
    Unused,
}

/// Represents a basic block in the control flow graph.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct BasicBlock {
    pub id: BasicBlockId,

    /// Defines all the input registers, which the block takes
    /// as input from predecessor blocks.
    pub parameters: Vec<RegisterId>,

    /// Defines all non-terminator instructions in the block.
    pub instructions: Vec<Instruction>,

    /// Defines the terminator of the block.
    ///
    /// Even though the terminator is optional, it is required for the block to
    /// be valid. If a block does not have a terminator, it will default to
    /// returning void.
    terminator: Option<Terminator>,

    /// Gets all the predecessor blocks, which branch to this block.
    predecessors: IndexSet<BasicBlockId>,

    /// Gets all the successor blocks, which this block branches to.
    successors: IndexSet<BasicBlockId>,

    /// Defines all the input registers, which are the result of phi
    /// node instructions.
    phi_registers: IndexMap<RegisterId, RegisterId>,

    /// Defines the usage of the block.
    usage: BlockUsage,
}

impl BasicBlock {
    pub fn new(id: BasicBlockId) -> Self {
        BasicBlock {
            id,
            parameters: Vec::new(),
            instructions: Vec::new(),
            terminator: None,
            predecessors: IndexSet::new(),
            successors: IndexSet::new(),
            phi_registers: IndexMap::new(),
            usage: BlockUsage::default(),
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

    /// Sets the terminator of the block, whether one has been set already or
    /// not.
    pub fn set_terminator_full(&mut self, term: Terminator) {
        self.terminator = Some(term);
    }

    /// Sets the usage of the block.
    pub fn set_usage(&mut self, usage: BlockUsage) {
        self.usage = usage;
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
        self.predecessors.insert(block);
    }

    /// Removes the given block to be a predecessor to the current block.
    pub fn remove_predecessor(&mut self, block: BasicBlockId) {
        self.predecessors.shift_remove(&block);
    }

    /// Pushes the given block to be a successor to the current block.
    pub fn push_successor(&mut self, block: BasicBlockId) {
        self.successors.reserve_exact(1);
        self.successors.insert(block);
    }

    /// Gets the phi registers of the block.
    pub fn phi_registers(&self) -> impl Iterator<Item = (RegisterId, RegisterId)> {
        self.phi_registers.iter().map(|(k, v)| (*k, *v))
    }

    /// Gets whether the given register is a destination register for a phi
    /// instruction.
    pub fn is_register_phi_dest(&self, reg: RegisterId) -> bool {
        self.phi_registers.contains_key(&reg)
    }

    /// Pushes the given register onto the block as a phi destination register.
    pub fn push_phi_register(&mut self, src: RegisterId, dst: RegisterId) {
        self.phi_registers.insert(dst, src);
    }

    /// Attempts to resolve the source of a phi node from the given destination
    /// node.
    pub fn resolve_phi_source(&self, dst: RegisterId) -> Option<RegisterId> {
        self.phi_registers().find(|(phi, _)| *phi == dst).map(|(_, src)| src)
    }

    /// Declares a new stack-allocated register with the given value.
    pub fn declare(&mut self, register: RegisterId, decl: Declaration, ty: Type, loc: Location) {
        self.instructions.push(Instruction {
            kind: InstructionKind::Let { register, decl, ty },
            location: loc,
        });
    }

    /// Assigns a new value to an existing register.
    pub fn assign(&mut self, target: RegisterId, value: Operand, loc: Location) {
        self.instructions.push(Instruction {
            kind: InstructionKind::Assign { target, value },
            location: loc,
        });
    }

    /// Declares a new stack-allocated slot with the given value.
    pub fn create_slot(&mut self, slot: SlotId, ty: Type, loc: Location) {
        self.instructions.push(Instruction {
            kind: InstructionKind::CreateSlot { slot, ty },
            location: loc,
        });
    }

    /// Declares a new heap-allocated register with the given type.
    pub fn allocate(&mut self, register: RegisterId, ty: Type, metadata: RegisterId, loc: Location) {
        self.instructions.push(Instruction {
            kind: InstructionKind::Allocate { register, ty, metadata },
            location: loc,
        });
    }

    /// Stores a value in an existing register.
    pub fn store(&mut self, target: RegisterId, value: Operand, loc: Location) {
        self.instructions.push(Instruction {
            kind: InstructionKind::Store { target, value },
            location: loc,
        });
    }

    /// Stores a value in an existing slot.
    pub fn store_slot(&mut self, target: SlotId, offset: usize, value: Operand, loc: Location) {
        self.instructions.push(Instruction {
            kind: InstructionKind::StoreSlot { target, value, offset },
            location: loc,
        });
    }

    /// Stores a value in a field of an existing register.
    pub fn store_field(&mut self, target: RegisterId, offset: usize, value: Operand, location: Location) {
        self.instructions.push(Instruction {
            kind: InstructionKind::StoreField { target, offset, value },
            location,
        });
    }

    /// Sets the terminator of the current block to an unconditional branch.
    pub fn branch(&mut self, block: BasicBlockId, location: Location) {
        self.set_terminator(Terminator {
            kind: TerminatorKind::Branch(BlockBranchSite::new(block)),
            location,
        });
    }

    /// Sets the terminator of the current block to an unconditional branch.
    pub fn branch_with(&mut self, block: BasicBlockId, args: &[RegisterId], location: Location) {
        self.set_terminator(Terminator {
            kind: TerminatorKind::Branch(BlockBranchSite {
                block,
                arguments: args.iter().map(|reg| Operand::reference_of(*reg)).collect(),
            }),
            location,
        });
    }

    /// Sets the terminator of the current block to a conditional branch.
    pub fn conditional_branch(
        &mut self,
        cond: RegisterId,
        then_block: BasicBlockId,
        else_block: BasicBlockId,
        location: Location,
    ) {
        self.set_terminator(Terminator {
            kind: TerminatorKind::ConditionalBranch {
                condition: cond,
                then_block: BlockBranchSite::new(then_block),
                else_block: BlockBranchSite::new(else_block),
            },
            location,
        });
    }

    /// Sets the terminator of the current block to a conditional branch.
    pub fn conditional_branch_with(
        &mut self,
        cond: RegisterId,
        then_block: BasicBlockId,
        then_block_args: &[RegisterId],
        else_block: BasicBlockId,
        else_block_args: &[RegisterId],
        location: Location,
    ) {
        self.set_terminator(Terminator {
            kind: TerminatorKind::ConditionalBranch {
                condition: cond,
                then_block: BlockBranchSite {
                    block: then_block,
                    arguments: then_block_args.iter().map(|reg| Operand::reference_of(*reg)).collect(),
                },
                else_block: BlockBranchSite {
                    block: else_block,
                    arguments: else_block_args.iter().map(|reg| Operand::reference_of(*reg)).collect(),
                },
            },
            location,
        });
    }

    /// Returns the given value, if any is defined. Otherwise, returns `void`.
    pub fn return_any(&mut self, value: Option<Operand>, location: Location) {
        if let Some(value) = value {
            self.return_value(value, location);
        } else {
            self.return_void(location);
        }
    }

    /// Returns the `void` from the block.
    pub fn return_void(&mut self, location: Location) {
        self.set_terminator(Terminator {
            kind: TerminatorKind::Return(None),
            location,
        });
    }

    /// Returns the the given value from the block.
    pub fn return_value(&mut self, value: Operand, location: Location) {
        self.set_terminator(Terminator {
            kind: TerminatorKind::Return(Some(value)),
            location,
        });
    }

    /// Returns the the given value from the block.
    pub fn unreachable(&mut self, location: Location) {
        self.set_terminator(Terminator {
            kind: TerminatorKind::Unreachable,
            location,
        });
    }
}

impl std::fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.usage == BlockUsage::Unused {
            return Ok(());
        }

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

#[derive(Serialize, Deserialize, Default, Debug, Hash, Clone, Copy, PartialEq, Eq)]
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

/// Defines a register within a block, which can hold a value of a specific
/// type.
///
/// Registers cannot be altered after they are created, as they follow
/// the SSA (Single Static Assignment) principle. If register needs to be
/// updated, it should define an allocation which can be used to store the new
/// value.
#[derive(Serialize, Deserialize, Default, Debug, Hash, Clone, PartialEq, Eq)]
pub struct Register {
    pub id: RegisterId,

    /// Defines the type of the register.
    pub ty: Type,

    /// Defines which block the register belongs to.
    pub block: Option<BasicBlockId>,
}

#[derive(Serialize, Deserialize, Default, Debug, Clone)]
pub struct Registers {
    regs: Vec<Register>,
}

impl Registers {
    /// Gets a reference to the [`Register`] with the given ID.
    #[track_caller]
    pub fn next_id(&self) -> RegisterId {
        let next = self.regs.iter().map(|reg| reg.id.as_usize() + 1).max().unwrap_or(0);

        RegisterId(next)
    }

    /// Gets a reference to the [`Register`] with the given ID.
    #[track_caller]
    pub fn register(&self, id: RegisterId) -> &Register {
        &self.regs[id.0]
    }

    /// Gets a mutable reference to the [`Register`] with the given ID.
    #[track_caller]
    pub fn register_mut(&mut self, id: RegisterId) -> &mut Register {
        &mut self.regs[id.0]
    }

    /// Gets a reference to the type of the [`Register`] with the given ID.
    #[track_caller]
    pub fn register_ty(&self, id: RegisterId) -> &Type {
        &self.register(id).ty
    }

    /// Allocates a new register with the given type and block.
    #[tracing::instrument(level = "TRACE", skip_all, fields(%ty, %block), ret)]
    pub fn allocate(&mut self, ty: Type, block: BasicBlockId) -> RegisterId {
        let id = self.next_id();
        self.regs.push(Register {
            id,
            ty,
            block: Some(block),
        });

        id
    }

    /// Allocates a new parameter register with the given type.
    #[tracing::instrument(level = "TRACE" skip_all, fields(%ty), ret)]
    pub fn allocate_param(&mut self, ty: Type) -> RegisterId {
        let id = self.next_id();
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

    /// Iterates over all parameter registers.
    pub fn iter_params(&self) -> impl Iterator<Item = &Register> {
        self.regs.iter().filter(|reg| reg.block.is_none())
    }

    /// Replaces all the existing registers within the function.
    pub fn replace_all(&mut self, replacement: impl Iterator<Item = Register>) {
        self.regs = replacement.collect();
    }
}

#[derive(Serialize, Deserialize, Default, Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct SlotId(usize);

impl SlotId {
    pub fn new(index: usize) -> Self {
        SlotId(index)
    }

    pub fn as_usize(&self) -> usize {
        self.0
    }
}

impl std::fmt::Display for SlotId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.0)
    }
}

/// Represents a standalone instruction within a basic block.
///
/// Instructions themselves cannot be referenced by other instructions - only
/// registers they declare can be referenced.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub location: Location,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum InstructionKind {
    /// Declares an SSA register within the current function.
    Let {
        register: RegisterId,
        decl: Declaration,
        ty: Type,
    },

    /// Assigns the value into the target register.
    Assign { target: RegisterId, value: Operand },

    /// Declares a stack-allocated slot within the current function.
    CreateSlot { slot: SlotId, ty: Type },

    /// Declares a heap-allocated register within the current function.
    Allocate {
        register: RegisterId,
        ty: Type,
        metadata: RegisterId,
    },

    /// Stores the value into the target register.
    Store { target: RegisterId, value: Operand },

    /// Stores the value into the target slot.
    StoreSlot {
        target: SlotId,
        value: Operand,
        offset: usize,
    },

    /// Stores the value into the field of an target register.
    StoreField {
        target: RegisterId,
        offset: usize,
        value: Operand,
    },

    /// Marks the given register as a GC reference
    ObjectRegister { register: RegisterId },
}

impl Instruction {
    pub fn register_def(&self) -> Option<RegisterId> {
        match &self.kind {
            InstructionKind::Let { register, .. } | InstructionKind::Allocate { register, .. } => Some(*register),
            InstructionKind::Assign { .. }
            | InstructionKind::CreateSlot { .. }
            | InstructionKind::Store { .. }
            | InstructionKind::StoreSlot { .. }
            | InstructionKind::StoreField { .. }
            | InstructionKind::ObjectRegister { .. } => None,
        }
    }

    pub fn register_refs(&self) -> Vec<RegisterId> {
        match &self.kind {
            InstructionKind::Let { decl, .. } => decl.register_refs(),
            InstructionKind::Assign { target, value }
            | InstructionKind::Store { target, value }
            | InstructionKind::StoreField { target, value, .. } => {
                let mut refs = vec![*target];
                refs.extend(value.register_refs());

                refs
            }
            InstructionKind::ObjectRegister { register } => vec![*register],
            InstructionKind::CreateSlot { .. }
            | InstructionKind::Allocate { .. }
            | InstructionKind::StoreSlot { .. } => Vec::new(),
        }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            InstructionKind::Let { register, decl, ty } => write!(f, "let {register}: {ty} = {decl}"),
            InstructionKind::Assign { target, value } => write!(f, "{target} = {value}"),
            InstructionKind::CreateSlot { slot, ty } => write!(f, "{slot} = slot ({} bytes)", ty.bytesize()),
            InstructionKind::Allocate { register, ty, .. } => write!(f, "{register} = alloc {ty}"),
            InstructionKind::Store { target, value } => write!(f, "*{target} = {value}"),
            InstructionKind::StoreSlot { target, value, offset } => write!(f, "*{target}[+x{offset:X}] = {value}"),
            InstructionKind::StoreField { target, offset, value } => write!(f, "*{target}[+x{offset:X}] = {value}"),
            InstructionKind::ObjectRegister { register } => write!(f, "mark object({register})"),
        }
    }
}

/// Represents the right-hand side of a declaration instruction.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Declaration {
    pub kind: Box<DeclarationKind>,
    pub location: Location,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum DeclarationKind {
    /// Represents an operand value.
    Operand(Operand),

    /// Represents an inline cast from a target register to another type.
    Cast { operand: RegisterId, bits: u8 },

    /// Defines a call to an intrinsic function.
    Intrinsic { name: Intrinsic, args: Vec<Operand> },

    /// Represents a call to a function.
    Call { func_id: NodeId, args: Vec<Operand> },

    /// Represents an indirect call to a function.
    IndirectCall {
        ptr: RegisterId,
        signature: Signature,
        args: Vec<Operand>,
    },
}

impl Declaration {
    pub fn register_refs(&self) -> Vec<RegisterId> {
        match self.kind.as_ref() {
            DeclarationKind::Operand(op) => op.register_refs(),
            DeclarationKind::Cast { operand, .. } => vec![*operand],
            DeclarationKind::Intrinsic { args, .. } | DeclarationKind::Call { args, .. } => {
                args.iter().flat_map(Operand::register_refs).collect()
            }
            DeclarationKind::IndirectCall { ptr, args, .. } => {
                let mut args = args.iter().flat_map(Operand::register_refs).collect::<Vec<_>>();
                args.push(*ptr);

                args
            }
        }
    }
}

impl std::fmt::Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind.as_ref() {
            DeclarationKind::Operand(op) => op.fmt(f),
            DeclarationKind::Cast { operand, bits } => write!(f, "{operand} as i{bits}"),
            DeclarationKind::Intrinsic { name, args } => write!(
                f,
                "{name}({})",
                args.iter().map(|arg| format!("{arg}")).collect::<Vec<_>>().join(", ")
            ),
            DeclarationKind::Call { func_id, args } => write!(
                f,
                "(call {func_id})({})",
                args.iter().map(|arg| format!("{arg}")).collect::<Vec<_>>().join(", ")
            ),
            DeclarationKind::IndirectCall { ptr, args, .. } => write!(
                f,
                "(call {ptr})({})",
                args.iter().map(|arg| format!("{arg}")).collect::<Vec<_>>().join(", ")
            ),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
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
    Metadata { metadata: Box<TypeMetadata> },
}

impl std::fmt::Display for Intrinsic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::FloatEq { .. } | Self::IntEq { .. } | Self::BooleanEq => write!(f, "=="),
            Self::FloatNe { .. } | Self::IntNe { .. } | Self::BooleanNe => write!(f, "!="),
            Self::FloatLe { .. } | Self::IntLe { .. } => write!(f, "<<"),
            Self::FloatLt { .. } | Self::IntLt { .. } => write!(f, "<"),
            Self::FloatGe { .. } | Self::IntGe { .. } => write!(f, ">="),
            Self::FloatGt { .. } | Self::IntGt { .. } => write!(f, ">"),
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
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Operand {
    pub kind: OperandKind,
    pub location: Location,
}

impl Operand {
    pub fn reference_of(register: RegisterId) -> Self {
        Self {
            kind: OperandKind::Reference { id: register },
            location: Location::empty(),
        }
    }

    pub fn is_reference_of(&self, register: RegisterId) -> bool {
        if let OperandKind::Reference { id } = &self.kind
            && *id == register
        {
            true
        } else {
            false
        }
    }

    pub fn integer(bits: u8, signed: bool, value: i64) -> Self {
        Self {
            kind: OperandKind::Integer { bits, signed, value },
            location: Location::empty(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum OperandKind {
    /// Represents a literal boolean value.
    Boolean { value: bool },

    /// Represents a literal integer value.
    Integer { bits: u8, signed: bool, value: i64 },

    /// Represents a literal floating-point value.
    Float { bits: u8, value: f64 },

    /// Represents a literal string value.
    String { value: Interned<String> },

    /// Represents a bitcast register of another type.
    Bitcast { source: RegisterId, target: Type },

    /// Represents a loaded value from an existing register.
    Load { id: RegisterId },

    /// Represents a loaded value from an existing register.
    LoadField {
        target: RegisterId,
        offset: usize,
        index: usize,
        field_type: Type,
    },

    /// Represents a loaded value from an existing slot.
    LoadSlot {
        target: SlotId,
        offset: usize,
        loaded_type: Type,
    },

    /// Represents an address to an existing stack slot.
    SlotAddress { id: SlotId, offset: usize },

    /// Represents a reference to an existing register.
    Reference { id: RegisterId },
}

impl Operand {
    /// Gets the bitsize of the operand.
    #[expect(clippy::cast_possible_truncation, clippy::missing_panics_doc)]
    pub fn bitsize(&self) -> u8 {
        match &self.kind {
            OperandKind::Boolean { .. } => 1,
            OperandKind::Integer { bits, .. } | OperandKind::Float { bits, .. } => *bits,
            OperandKind::Reference { .. } | OperandKind::String { .. } => std::mem::size_of::<*const u32>() as u8 * 8,
            OperandKind::Bitcast { .. } => panic!("cannot get bitsize of bitcast operand"),
            OperandKind::Load { .. }
            | OperandKind::LoadField { .. }
            | OperandKind::LoadSlot { .. }
            | OperandKind::SlotAddress { .. } => {
                panic!("cannot get bitsize of load operand")
            }
        }
    }

    pub fn register_refs(&self) -> Vec<RegisterId> {
        match &self.kind {
            OperandKind::Load { id } | OperandKind::Reference { id } => {
                vec![*id]
            }
            OperandKind::LoadField { target, .. } => {
                vec![*target]
            }
            OperandKind::Bitcast { source, .. } => {
                vec![*source]
            }
            OperandKind::Boolean { .. }
            | OperandKind::Integer { .. }
            | OperandKind::Float { .. }
            | OperandKind::String { .. }
            | OperandKind::LoadSlot { .. }
            | OperandKind::SlotAddress { .. } => Vec::new(),
        }
    }

    /// Determines whether the operand references the given register within it.
    pub fn references_register(&self, register: RegisterId) -> bool {
        match &self.kind {
            OperandKind::Boolean { .. }
            | OperandKind::Integer { .. }
            | OperandKind::Float { .. }
            | OperandKind::String { .. }
            | OperandKind::Bitcast { .. }
            | OperandKind::LoadSlot { .. }
            | OperandKind::SlotAddress { .. } => false,
            OperandKind::Reference { id } | OperandKind::Load { id } => *id == register,
            OperandKind::LoadField { target, .. } => *target == register,
        }
    }

    /// Determines whether the operand stores the given register within it.
    pub fn stores_register(&self, register: RegisterId) -> bool {
        match &self.kind {
            OperandKind::Boolean { .. }
            | OperandKind::Integer { .. }
            | OperandKind::Float { .. }
            | OperandKind::String { .. }
            | OperandKind::Bitcast { .. }
            | OperandKind::LoadSlot { .. }
            | OperandKind::SlotAddress { .. } => false,
            OperandKind::Reference { id } | OperandKind::Load { id } => *id == register,
            OperandKind::LoadField { target, field_type, .. } => *target == register && field_type.is_reference_type(),
        }
    }
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            OperandKind::Boolean { value } => write!(f, "{value}"),
            OperandKind::Integer { bits, signed, value } => {
                write!(f, "{value}_{}{bits}", if *signed { "i" } else { "u" })
            }
            OperandKind::Float { bits, value } => write!(f, "{value}_f{bits}"),
            OperandKind::Bitcast { source, target } => write!(f, "{source} as {target}"),
            OperandKind::Reference { id } => write!(f, "{id}"),
            OperandKind::Load { id } => write!(f, "*{id}"),
            OperandKind::LoadField { target, offset, .. } => write!(f, "*{target}[+x{offset:X}]"),
            OperandKind::LoadSlot { target, offset, .. } => write!(f, "*{target}[+x{offset:X}]"),
            OperandKind::SlotAddress { id, offset } => write!(f, "{id}[+x{offset:X}]"),
            OperandKind::String { value } => write!(f, "\"{value}\""),
        }
    }
}

/// Represents a terminator of a block, which defines how control flow is
/// transferred.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Terminator {
    pub kind: TerminatorKind,
    pub location: Location,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum TerminatorKind {
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

    /// Switch table with constant integer patterns.
    Switch {
        operand: RegisterId,
        arms: Vec<(i64, BlockBranchSite)>,
        fallback: BlockBranchSite,
    },

    /// Defines the terminator as being unreachable, which may happen when
    /// calling `noret` functions or panic.
    Unreachable,
}

impl Terminator {
    pub fn register_refs(&self) -> Vec<RegisterId> {
        match &self.kind {
            TerminatorKind::Return(operand) => {
                if let Some(operand) = operand {
                    operand.register_refs()
                } else {
                    Vec::new()
                }
            }
            TerminatorKind::ConditionalBranch {
                condition,
                then_block,
                else_block,
            } => {
                let mut refs = vec![*condition];

                for arg in &then_block.arguments {
                    refs.extend(arg.register_refs());
                }

                for arg in &else_block.arguments {
                    refs.extend(arg.register_refs());
                }

                refs
            }
            TerminatorKind::Switch {
                operand,
                arms,
                fallback,
            } => {
                let mut refs = vec![*operand];
                for (_, arm) in arms {
                    for arg in &arm.arguments {
                        refs.extend(arg.register_refs());
                    }
                }

                for arg in &fallback.arguments {
                    refs.extend(arg.register_refs());
                }

                refs
            }
            TerminatorKind::Branch(site) => {
                let mut refs = Vec::new();
                for arg in &site.arguments {
                    refs.extend(arg.register_refs());
                }

                refs
            }
            TerminatorKind::Unreachable => Vec::new(),
        }
    }
}

impl std::fmt::Display for Terminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TerminatorKind::Return(value) => {
                if let Some(value) = value {
                    write!(f, "return {value}")
                } else {
                    write!(f, "return")
                }
            }
            TerminatorKind::ConditionalBranch {
                condition,
                then_block,
                else_block,
            } => write!(f, "if {condition} goto {then_block} else {else_block}"),
            TerminatorKind::Switch {
                operand,
                arms,
                fallback,
            } => {
                writeln!(f, "switch {operand} [")?;

                for arm in arms {
                    writeln!(f, "      {} => {}", arm.0, arm.1)?;
                }

                writeln!(f, "      _ => {fallback}")?;
                write!(f, "    ]")
            }
            TerminatorKind::Branch(block_id) => write!(f, "goto {block_id}"),
            TerminatorKind::Unreachable => write!(f, "unreachable"),
        }
    }
}

/// Represents a call site for a branch instruction, branching to a block
/// with a set of arguments.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct BlockBranchSite {
    pub block: BasicBlockId,
    pub arguments: Vec<Operand>,
}

impl BlockBranchSite {
    pub fn new(block: BasicBlockId) -> Self {
        Self {
            block,
            arguments: Vec::new(),
        }
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

/// Defines a type within the MIR.
#[derive(Serialize, Deserialize, Hash, Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub kind: TypeKind,
    pub is_generic: bool,
}

impl Type {
    pub fn void() -> Self {
        Self {
            kind: TypeKind::Void,
            is_generic: false,
        }
    }

    pub fn boolean() -> Self {
        Self {
            kind: TypeKind::Boolean,
            is_generic: false,
        }
    }

    pub fn string() -> Self {
        Self {
            kind: TypeKind::String,
            is_generic: false,
        }
    }

    pub fn pointer(elemental: Type) -> Self {
        Self {
            kind: TypeKind::Pointer {
                elemental: Box::new(elemental),
            },
            is_generic: false,
        }
    }

    pub fn type_param() -> Self {
        Self {
            kind: TypeKind::Pointer {
                elemental: Box::new(Type::void()),
            },
            is_generic: true,
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
            kind: TypeKind::Integer { bits: 8, signed: true },
            is_generic: false,
        }
    }

    pub fn u8() -> Self {
        Self {
            kind: TypeKind::Integer { bits: 8, signed: false },
            is_generic: false,
        }
    }

    pub fn i16() -> Self {
        Self {
            kind: TypeKind::Integer { bits: 16, signed: true },
            is_generic: false,
        }
    }

    pub fn u16() -> Self {
        Self {
            kind: TypeKind::Integer {
                bits: 16,
                signed: false,
            },
            is_generic: false,
        }
    }

    pub fn i32() -> Self {
        Self {
            kind: TypeKind::Integer { bits: 32, signed: true },
            is_generic: false,
        }
    }

    pub fn u32() -> Self {
        Self {
            kind: TypeKind::Integer {
                bits: 32,
                signed: false,
            },
            is_generic: false,
        }
    }

    pub fn i64() -> Self {
        Self {
            kind: TypeKind::Integer { bits: 64, signed: true },
            is_generic: false,
        }
    }

    pub fn u64() -> Self {
        Self {
            kind: TypeKind::Integer {
                bits: 64,
                signed: false,
            },
            is_generic: false,
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
            kind: TypeKind::Float { bits: 32 },
            is_generic: false,
        }
    }

    pub fn f64() -> Self {
        Self {
            kind: TypeKind::Float { bits: 64 },
            is_generic: false,
        }
    }

    pub fn structure(name: String, fields: Vec<Type>) -> Self {
        Self {
            kind: TypeKind::Struct { name, fields },
            is_generic: false,
        }
    }

    pub fn union(cases: Vec<Type>) -> Self {
        Self {
            kind: TypeKind::Union { cases },
            is_generic: false,
        }
    }

    pub fn tuple(items: Vec<Type>) -> Self {
        Self {
            kind: TypeKind::Tuple { items },
            is_generic: false,
        }
    }

    pub fn is_void(&self) -> bool {
        self.kind == TypeKind::Void
    }

    pub fn is_reference_type(&self) -> bool {
        self.kind.is_reference_type()
    }

    pub fn is_metadata_type(&self) -> bool {
        matches!(self.kind, TypeKind::Metadata { .. })
    }

    pub fn requires_stack_map(&self) -> bool {
        // void-pointers are only really used for type arguments, which do not
        // require a stack map.
        if let TypeKind::Pointer { elemental } = &self.kind
            && let TypeKind::Void = &elemental.kind
        {
            return true;
        }

        // All other types of pointers, it depends on the inner type.
        if let TypeKind::Pointer { elemental } = &self.kind {
            return elemental.requires_stack_map();
        }

        // Structs should have a stack map, but type metadata should not.
        if let TypeKind::Struct { name, .. } = &self.kind
            && name == "std::Type"
        {
            return false;
        }

        matches!(&self.kind, TypeKind::Struct { .. } | TypeKind::String)
    }

    pub fn is_signed(&self) -> bool {
        if let TypeKind::Integer { signed, .. } = &self.kind {
            *signed
        } else {
            false
        }
    }

    pub fn bytesize(&self) -> usize {
        match &self.kind {
            TypeKind::Struct { fields, .. } => {
                let metadata_ptr_size = POINTER_SIZE;
                let fields_sum_size: usize = fields.iter().map(Type::bytesize).sum();

                metadata_ptr_size + fields_sum_size
            }
            TypeKind::Union { cases } => {
                let metadata_ptr_size = POINTER_SIZE;
                let discriminator_size = Self::i8().bytesize();
                let max_case_size = cases.iter().map(Type::bytesize).max().unwrap_or_default();

                metadata_ptr_size + discriminator_size + max_case_size
            }
            TypeKind::Tuple { items } => items.iter().map(Type::bytesize).sum(),
            TypeKind::Integer { bits, .. } | TypeKind::Float { bits } => (*bits / 8) as usize,
            TypeKind::Boolean => 1,
            TypeKind::String | TypeKind::Pointer { .. } | TypeKind::Metadata { .. } => POINTER_SIZE,
            TypeKind::Void => 0,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl Default for Type {
    fn default() -> Self {
        Self::void()
    }
}

#[derive(Serialize, Deserialize, Hash, Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    /// Represents a struct type with zero-or-more fields.
    Struct { name: String, fields: Vec<Type> },

    /// Represents a union type with zero-or-more cases.
    Union { cases: Vec<Type> },

    /// Represents an unnamed tuple type with zero-or-more items.
    Tuple { items: Vec<Type> },

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
    Metadata { inner: Box<TypeMetadata> },

    /// Defines a void type.
    Void,
}

impl TypeKind {
    pub fn is_reference_type(&self) -> bool {
        matches!(
            self,
            Self::Struct { .. } | Self::String | Self::Pointer { .. } | Self::Metadata { .. }
        )
    }
}

impl std::fmt::Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Struct { name, .. } => write!(f, "{name}"),
            Self::Union { cases } => write!(
                f,
                "(u8, {})",
                cases
                    .iter()
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Tuple { items } => write!(
                f,
                "({})",
                items
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
