use std::collections::HashSet;

use indexmap::{IndexMap, IndexSet};
use lume_mir::*;
use lume_span::Location;

use crate::FunctionTransformer;

impl FunctionTransformer<'_, '_> {
    pub(crate) fn run_passes(&mut self) {
        DefineBlockEdges.execute(&mut self.func);
        RemoveOrphanBlocks.execute(&mut self.func);
        DefineBlockParameters::default().execute(&mut self.func);
        PassBlockArguments::execute(&mut self.func);
        ConvertAssignmentExpressions::default().execute(&mut self.func);
        RenameSsaVariables::default().execute(&mut self.func);
        MarkObjectReferences::default().execute(&mut self.func);
    }
}

/// Detects edges between all blocks within a function, which is used
/// for later passes.
///
/// After edge detection has finished, the function can effectively be
/// remapped into a directed graph of blocks, where each node corresponds
/// to a block and each directed edge corresponds to a call between blocks A and B.
#[derive(Default, Debug)]
struct DefineBlockEdges;

impl DefineBlockEdges {
    pub fn execute(&self, func: &mut Function) {
        let mut edges = IndexSet::<(BasicBlockId, BasicBlockId)>::new();

        for block in func.blocks.values() {
            let terminator = block.terminator().unwrap();

            match &terminator.kind {
                lume_mir::TerminatorKind::Branch(term) => {
                    edges.insert((block.id, term.block));
                }
                lume_mir::TerminatorKind::ConditionalBranch {
                    then_block, else_block, ..
                } => {
                    edges.insert((block.id, then_block.block));
                    edges.insert((block.id, else_block.block));
                }
                lume_mir::TerminatorKind::Switch { arms, fallback, .. } => {
                    for (_, arm) in arms {
                        edges.insert((block.id, arm.block));
                    }

                    edges.insert((block.id, fallback.block));
                }
                lume_mir::TerminatorKind::Return(_) | lume_mir::TerminatorKind::Unreachable => {}
            }
        }

        for (succ, pred) in edges {
            func.block_mut(pred).push_predecessor(succ);
            func.block_mut(succ).push_successor(pred);
        }
    }
}

/// Removes all blocks, except the entry block, which have no predecessors, since
/// they only serve to clobber future passes and codegen.
///
/// The pass also removes any orphan blocks as predecessor of any successor blocks.
#[derive(Default, Debug)]
struct RemoveOrphanBlocks;

impl RemoveOrphanBlocks {
    pub fn execute(&self, func: &mut Function) {
        let mut orphan_blocks = Vec::new();

        for block in func.blocks.values() {
            // We ignore the entry block, since it'll never have any predecessors.
            if block.id.0 == 0 {
                continue;
            }

            if block.predecessors().count() == 0 {
                orphan_blocks.push(block.id);
            }
        }

        while let Some(orphan_block) = orphan_blocks.pop() {
            for block in func.blocks.values_mut() {
                block.remove_predecessor(orphan_block);
            }

            func.blocks.shift_remove(&orphan_block);
        }
    }
}

/// Determines which parameters are required for a given block, as well
/// as any subsequent successor blocks, so that matching registers can be
/// passed from any successor blocks.
///
/// For example, given MIR like the following:
/// ```mir
/// B0:
///     #0 = 4_i32
///     goto B1
/// B1:
///     #1 = 7_i32
///     #2: i32 = +(#0, #1)
///     goto B2
/// B2:
///     #3: i32 = *(#2, #0)
///     return #3
/// ```
///
/// Each successor block, except for `B0`, requires a register from a previous blocks. This
/// pass will look for all registers required for each block and declare them as parameters
/// of the block, if they cannot be found in the block itself. After the pass, the same
/// MIR will look like this:
/// ```mir
/// B0:
///     #0 = 4_i32
///     goto B1
/// B1(#0):       <-- notice the added block parameters
///     #1 = 7_i32
///     #2: i32 = +(#0, #1)
///     goto B2
/// B2(#2, #0):   <-- notice the added block parameters
///     #3: i32 = *(#2, #0)
///     return #3
/// ```
#[derive(Default, Debug)]
struct DefineBlockParameters {
    params: IndexMap<BasicBlockId, IndexSet<RegisterId>>,
}

impl DefineBlockParameters {
    pub fn execute(&mut self, func: &mut Function) {
        for block in func.blocks.values() {
            let mut visited = IndexSet::new();
            self.find_required_input_registers(func, block, &mut visited);
        }

        for block in func.blocks.values_mut() {
            // Skip blocks which don't have any predecessors, since they cannot
            // have any input parameters. This is mostly for entry blocks, as they
            // get their parameters implictly from the function itself.
            if block.predecessors().count() == 0 {
                continue;
            }

            let Some(params) = self.params.swap_remove(&block.id) else {
                continue;
            };

            block.parameters = params.into_iter().collect();
        }
    }

    fn find_required_input_registers(
        &mut self,
        func: &Function,
        block: &BasicBlock,
        visited: &mut IndexSet<BasicBlockId>,
    ) {
        let mut regs = IndexSet::new();

        for successor in block.successors() {
            if visited.contains(&successor) {
                continue;
            }

            visited.insert(successor);

            let successor_block = func.block(successor);
            self.find_required_input_registers(func, successor_block, visited);

            if let Some(successor_params) = self.params.get(&successor) {
                for successor_param in successor_params {
                    if block.is_register_phi_dest(*successor_param) {
                        continue;
                    }

                    regs.insert(*successor_param);
                }
            }
        }

        // Find all referenced registers within the block from all the instructions
        // and the block terminator...
        for inst in block.instructions() {
            regs.extend(inst.register_refs());
        }

        if let Some(terminator) = block.terminator() {
            regs.extend(terminator.register_refs());
        }

        // ...and scoop away all registers which were referenced within the block, but were
        // also defined within the block.
        for inst in block.instructions() {
            if let Some(def) = inst.register_def() {
                regs.shift_remove(&def);
            }
        }

        self.params.insert(block.id, regs);
    }
}

/// Using the result of the previous pass, [`DefineBlockParameters`], traverses through
/// each block and passes the appropriate registers to each call to any successor blocks.
///
/// For example, given MIR like the following:
/// ```mir
/// B0:
///     #0 = 4_i32
///     goto B1
/// B1(#0):
///     #1 = 7_i32
///     #2: i32 = +(#0, #1)
///     goto B2
/// B2(#2, #0):
///     #3: i32 = *(#2, #0)
///     return #3
/// ```
///
/// Each successor block, except for `B2`, is meant to pass one-or-more registers to their
/// successor blocks. This pass will look through each block and update their terminator to pass
/// the required registers. After the pass, the same MIR will look like this:
/// ```mir
/// B0:
///     #0 = 4_i32
///     goto B1(#0)          <-- notice the added block arguments
/// B1(#0):
///     #1 = 7_i32
///     #2: i32 = +(#0, #1)
///     goto B2(#2, #0)
/// B2(#2, #0):
///     #3: i32 = *(#2, #0)  <-- notice the added block arguments
///     return #3
/// ```
#[derive(Default, Debug)]
struct PassBlockArguments;

impl PassBlockArguments {
    pub fn execute(func: &mut Function) {
        // TODO:
        // I hate this, but we're not quite at the "we should optimize the compiler more"
        // phase, yet. But, this is certainly not a good solution to a lifetime issue.
        let func_immut = func.clone();

        for block in func.blocks.values_mut() {
            let block_id = block.id;

            if let Some(terminator) = block.terminator_mut() {
                match &mut terminator.kind {
                    TerminatorKind::Branch(target) => Self::update_branch_terminator(block_id, target, &func_immut),
                    TerminatorKind::ConditionalBranch {
                        then_block, else_block, ..
                    } => {
                        Self::update_branch_terminator(block_id, then_block, &func_immut);
                        Self::update_branch_terminator(block_id, else_block, &func_immut);
                    }
                    TerminatorKind::Switch { arms, fallback, .. } => {
                        for arm in arms {
                            Self::update_branch_terminator(block_id, &mut arm.1, &func_immut);
                        }

                        Self::update_branch_terminator(block_id, fallback, &func_immut);
                    }
                    TerminatorKind::Return(_) | TerminatorKind::Unreachable => {}
                }
            }
        }
    }

    fn update_branch_terminator(block: BasicBlockId, call_site: &mut BlockBranchSite, func: &Function) {
        let source_block = func.block(block);
        let target_block = func.block(call_site.block);

        call_site.arguments.reserve(target_block.parameters.len());

        for param in &target_block.parameters {
            let arg = if let Some(phi_source) = source_block.resolve_phi_source(*param) {
                phi_source
            } else {
                *param
            };

            if !call_site.arguments.contains(&arg) {
                call_site.arguments.push(arg);
            }
        }
    }
}

/// This pass converts any found direct assignment expressions, turns them into
/// a new declaration and uses that declaration for any following register references.
/// This is done because SSA, by design, does not support altering existing register values
/// directly.
///
/// For example, MIR like the following:
/// ```mir
///     let #0 = 4_i32
///     call foo(#0)
///     #0 = 6_i32     <-- invalid! SSA forbids reassigning existing registers
///     return #0
/// ```
///
/// is invalid SSA, since `#0` has now been reassigned. This pass attempts to convert the
/// assignment instruction into the following MIR:
/// ```mir
///     let #0 = 4_i32
///     call foo(#0)
///     let #1 = 6_i32
///     return #1
/// ```
#[derive(Default, Debug)]
struct ConvertAssignmentExpressions {
    register_count: usize,
    moved_regs: Vec<(RegisterId, RegisterId)>,
    registers: Registers,
}

impl ConvertAssignmentExpressions {
    pub fn execute(&mut self, func: &mut Function) {
        self.registers = func.registers.clone();
        self.register_count = self.registers.next_id().as_usize();

        let mut new_registers = IndexSet::new();

        for block in func.blocks.values_mut() {
            for inst in block.instructions_mut() {
                self.update_regs_inst(inst);
            }

            if let Some(term) = block.terminator_mut() {
                self.update_regs_term(term);
            }

            for (old, new) in &self.moved_regs {
                let old_reg = func.registers.register(*old);

                new_registers.insert(Register {
                    id: *new,
                    ty: old_reg.ty.clone(),
                    block: Some(block.id),
                });
            }

            self.moved_regs.clear();
        }

        for new_reg in new_registers {
            let id = func.registers.allocate(new_reg.ty, new_reg.block.unwrap());

            debug_assert_eq!(new_reg.id, id);
        }
    }

    fn move_register(&mut self, old: &mut RegisterId) -> RegisterId {
        let new = RegisterId::new(self.register_count);

        self.moved_regs.push((*old, new));
        self.register_count += 1;

        *old = new;
        new
    }

    fn get_moved_register(&self, reg: &mut RegisterId) {
        if let Some(moved) = self.moved_regs.iter().rfind(|r| r.0 == *reg) {
            *reg = moved.1;
        }
    }

    fn update_regs_inst(&mut self, inst: &mut Instruction) {
        match &mut inst.kind {
            InstructionKind::Let { register, decl, .. } => {
                self.get_moved_register(register);
                self.update_regs_decl(decl);
            }
            InstructionKind::Assign { target, value } => {
                let ty = self.registers.register_ty(*target).to_owned();

                self.update_regs_op(value);
                self.move_register(target);

                // After the registers of the assignment instruction have been converted
                // to SSA registers, we transform the instruction itself to declare a new register
                // instead of attempting to assign a non-existent register.
                inst.kind = InstructionKind::Let {
                    register: *target,
                    decl: Declaration {
                        kind: DeclarationKind::Operand(value.clone()),
                        location: inst.location,
                    },
                    ty,
                };
            }
            InstructionKind::Allocate { register, .. } => {
                self.get_moved_register(register);
            }
            InstructionKind::Store { target, value } | InstructionKind::StoreField { target, value, .. } => {
                self.get_moved_register(target);
                self.update_regs_op(value);
            }
            InstructionKind::ObjectRegister { register } => {
                self.get_moved_register(register);
            }
            InstructionKind::CreateSlot { .. } | InstructionKind::StoreSlot { .. } => {}
        }
    }

    fn update_regs_term(&mut self, term: &mut Terminator) {
        match &mut term.kind {
            TerminatorKind::Return(op) => {
                if let Some(value) = op {
                    self.update_regs_op(value);
                }
            }
            TerminatorKind::Branch(site) => {
                for arg in &mut site.arguments {
                    self.get_moved_register(arg);
                }
            }
            TerminatorKind::ConditionalBranch {
                condition,
                then_block,
                else_block,
            } => {
                self.get_moved_register(condition);

                for arg in &mut then_block.arguments {
                    self.get_moved_register(arg);
                }

                for arg in &mut else_block.arguments {
                    self.get_moved_register(arg);
                }
            }
            TerminatorKind::Switch {
                operand,
                arms,
                fallback,
            } => {
                self.get_moved_register(operand);

                for arm in arms {
                    for arg in &mut arm.1.arguments {
                        self.get_moved_register(arg);
                    }
                }

                for arg in &mut fallback.arguments {
                    self.get_moved_register(arg);
                }
            }
            TerminatorKind::Unreachable => {}
        }
    }

    fn update_regs_decl(&mut self, decl: &mut Declaration) {
        match &mut decl.kind {
            DeclarationKind::Operand(op) => self.update_regs_op(op),
            DeclarationKind::Cast { operand, .. } => {
                self.get_moved_register(operand);
            }
            DeclarationKind::Call { args, .. } | DeclarationKind::Intrinsic { args, .. } => {
                for arg in args {
                    self.update_regs_op(arg);
                }
            }
            DeclarationKind::IndirectCall { ptr, args, .. } => {
                self.get_moved_register(ptr);

                for arg in args {
                    self.update_regs_op(arg);
                }
            }
        }
    }

    fn update_regs_op(&mut self, op: &mut Operand) {
        match &mut op.kind {
            OperandKind::Load { id } | OperandKind::Reference { id } => {
                self.get_moved_register(id);
            }
            OperandKind::LoadField { target, .. } => {
                self.get_moved_register(target);
            }
            OperandKind::Bitcast { source, .. } => {
                self.get_moved_register(source);
            }
            OperandKind::Boolean { .. }
            | OperandKind::Integer { .. }
            | OperandKind::Float { .. }
            | OperandKind::String { .. }
            | OperandKind::SlotAddress { .. } => {}
        }
    }
}

type RegisterMapping = IndexMap<(RegisterId, BasicBlockId), RegisterId>;

/// Some backend implementations, specifically Cranelift, does not allow using the same register
/// across block boundaries. Because of the *conservative* nature of Cranelift optimization, we must
/// do this preprocessing ourselves.
///
/// It will attempt to rename registers in an MIR function, so that each block start it's register
/// index at 0, then increments it when a new register is declared. This also include block parameters.
///
/// As an example, take the given MIR:
/// ```mir
/// B0:
///     #0 = 4_i32
///     #1 = 1_i32
///     #2: i32 = -(#0, #1)
///     goto B1(#2)
/// B1(#2):
///     #3 = 7_i32
///     #4: i32 = +(#0, #2)
///     goto B2(#2, #4)
/// B2(#2, #4):
///     #5: i32 = *(#2, #4)
///     return #5
/// ```
///
/// This is not valid in Cranelift, since most registers are used across multiple blocks. Look at `#0`
/// specifically; it is referenced in all of the blocks in the function!
///
/// To circumvent this ~pedantry~ *requirement*, this pass renames all the registers to be local to
/// the block in which they're declared. Using the given MIR from before, we transform it into:
/// ```mir
/// B0:
///     #0 = 4_i32
///     #1 = 1_i32
///     #2: i32 = -(#0, #1)
///     goto B1(#2)
/// B1(#0):
///     #1 = 7_i32
///     #2: i32 = +(#0, #1)
///     goto B2(#2, #0)
/// B2(#0, #1):
///     #2: i32 = *(#0, #1)
///     return #2
/// ```
///
/// <div class="warning">
///
/// **Here be dragons!**
///
/// On a more serious note, this is quite possibly the most error-prone part of the MIR lowering process.
/// The reason for this is mostly because of the awkward implementation of "phi" nodes between
/// blocks in some instances.
///
/// This pass should receive a refactor at some point, but is currently not planned.
///
/// </div>
#[derive(Default, Debug)]
pub(crate) struct RenameSsaVariables {
    register_counter: usize,
}

impl RenameSsaVariables {
    pub fn execute(&mut self, func: &mut Function) {
        let mut new_registers = func
            .registers
            .iter()
            .filter(|reg| reg.block.is_none())
            .cloned()
            .collect::<Vec<Register>>();

        let mut register_mapping = RegisterMapping::new();

        for block in func.blocks.values_mut() {
            let block_id = block.id;

            for param_idx in 0..func.signature.parameters.len() {
                let param_reg = func.registers.register_mut(RegisterId::new(param_idx));

                self.rename_register_index(param_reg.id, block_id, &mut register_mapping);
            }

            for param in &mut block.parameters {
                self.rename_register_index_mut(param, block_id, &mut register_mapping);
            }

            for inst in block.instructions_mut() {
                self.update_regs_inst(inst, block_id, &mut register_mapping);
            }

            if let Some(term) = block.terminator_mut() {
                Self::update_regs_term(term, block_id, &mut register_mapping);
            }

            for (old, new) in &register_mapping {
                let old_reg = func.registers.register(old.0);

                if new_registers.len() <= new.as_usize() {
                    new_registers.resize_with(new.as_usize() + 1, Register::default);
                }

                new_registers[new.as_usize()] = lume_mir::Register {
                    id: *new,
                    ty: old_reg.ty.clone(),
                    block: Some(block.id),
                };
            }

            register_mapping.clear();
        }

        func.registers.replace_all(new_registers.into_iter());
    }

    fn rename_register_index(
        &mut self,
        old: RegisterId,
        block: BasicBlockId,
        mapping: &mut RegisterMapping,
    ) -> RegisterId {
        let new = RegisterId::new(self.register_counter);

        mapping.insert((old, block), new);
        self.register_counter += 1;

        new
    }

    fn rename_register_index_mut(
        &mut self,
        old: &mut RegisterId,
        block: BasicBlockId,
        mapping: &mut RegisterMapping,
    ) -> RegisterId {
        let new = RegisterId::new(self.register_counter);

        mapping.insert((*old, block), new);
        self.register_counter += 1;

        *old = new;

        new
    }

    fn update_regs_inst(&mut self, inst: &mut Instruction, block: BasicBlockId, mapping: &mut RegisterMapping) {
        match &mut inst.kind {
            InstructionKind::Let { register, decl, .. } => {
                self.rename_register_index_mut(register, block, mapping);

                Self::update_regs_decl(decl, block, mapping);
            }
            InstructionKind::Assign { .. } => unreachable!("bug!: assignments should be removed in previous SSA pass"),
            InstructionKind::Allocate { register, .. } => {
                self.rename_register_index_mut(register, block, mapping);
            }
            InstructionKind::Store { target, value } | InstructionKind::StoreField { target, value, .. } => {
                *target = *mapping.get(&(*target, block)).unwrap();
                Self::update_regs_op(value, block, mapping);
            }
            InstructionKind::ObjectRegister { register } => {
                *register = *mapping.get(&(*register, block)).unwrap();
            }
            InstructionKind::CreateSlot { .. } | InstructionKind::StoreSlot { .. } => {}
        }
    }

    fn update_regs_term(term: &mut Terminator, block: BasicBlockId, mapping: &mut RegisterMapping) {
        match &mut term.kind {
            TerminatorKind::Return(op) => {
                if let Some(value) = op {
                    Self::update_regs_op(value, block, mapping);
                }
            }
            TerminatorKind::Branch(site) => {
                for arg in &mut site.arguments {
                    *arg = *mapping.get(&(*arg, block)).unwrap();
                }
            }
            TerminatorKind::ConditionalBranch {
                condition,
                then_block,
                else_block,
            } => {
                *condition = *mapping.get(&(*condition, block)).unwrap();

                for arg in &mut then_block.arguments {
                    *arg = *mapping.get(&(*arg, block)).unwrap();
                }

                for arg in &mut else_block.arguments {
                    *arg = *mapping.get(&(*arg, block)).unwrap();
                }
            }
            TerminatorKind::Switch {
                operand,
                arms,
                fallback,
            } => {
                *operand = *mapping.get(&(*operand, block)).unwrap();

                for arm in arms {
                    for arg in &mut arm.1.arguments {
                        *arg = *mapping.get(&(*arg, block)).unwrap();
                    }
                }

                for arg in &mut fallback.arguments {
                    *arg = *mapping.get(&(*arg, block)).unwrap();
                }
            }
            TerminatorKind::Unreachable => {}
        }
    }

    fn update_regs_decl(decl: &mut Declaration, block: BasicBlockId, mapping: &mut RegisterMapping) {
        match &mut decl.kind {
            DeclarationKind::Operand(op) => Self::update_regs_op(op, block, mapping),
            DeclarationKind::Cast { operand, .. } => {
                *operand = *mapping.get(&(*operand, block)).unwrap();
            }
            DeclarationKind::Call { args, .. } | DeclarationKind::Intrinsic { args, .. } => {
                for arg in args {
                    Self::update_regs_op(arg, block, mapping);
                }
            }
            DeclarationKind::IndirectCall { ptr, args, .. } => {
                *ptr = *mapping.get(&(*ptr, block)).unwrap();

                for arg in args {
                    Self::update_regs_op(arg, block, mapping);
                }
            }
        }
    }

    fn update_regs_op(op: &mut Operand, block: BasicBlockId, mapping: &mut RegisterMapping) {
        match &mut op.kind {
            OperandKind::Load { id } | OperandKind::Reference { id } => {
                *id = *mapping.get(&(*id, block)).unwrap();
            }
            OperandKind::LoadField { target, .. } => {
                *target = *mapping.get(&(*target, block)).unwrap();
            }
            OperandKind::Bitcast { source, .. } => {
                *source = *mapping.get(&(*source, block)).unwrap();
            }
            OperandKind::Boolean { .. }
            | OperandKind::Integer { .. }
            | OperandKind::Float { .. }
            | OperandKind::String { .. }
            | OperandKind::SlotAddress { .. } => {}
        }
    }
}

/// Attempts to mark managed objects as GC references, by adding [`InstructionKind::ObjectRegister`]
/// instructions to the MIR, wherever fitting. These instructions are used when the garbage collector
/// looks for live objects in any given location.
#[derive(Default, Debug)]
struct MarkObjectReferences {
    /// Current offset into the current block where the instruction
    /// should be placed.
    offset: usize,

    /// Defines all the currently marked objects, to prevent multiple instructions from being
    /// inserted, all referencing the same register.
    marked: HashSet<(BasicBlockId, RegisterId)>,

    /// List of all instructions which require an object register instruction.
    ///
    /// The tuple in each list entry is:
    ///  - the block in which the instruction was found,
    ///  - the offset of where to place the instruction
    ///  - and the register which needs to marked as an object.
    reference_inst: Vec<(BasicBlockId, usize, RegisterId)>,
}

impl MarkObjectReferences {
    pub fn execute(&mut self, func: &mut Function) {
        self.find_object_references(func);

        while let Some((block, offset, register)) = self.reference_inst.pop() {
            let block = func.block_mut(block);
            let inst = lume_mir::Instruction {
                kind: lume_mir::InstructionKind::ObjectRegister { register },
                location: Location::empty(),
            };

            // Even though it's valid for the offset to be the same length
            // as the instruction list, `insert` will panic if that's the case.
            if block.instructions.len() + 1 == offset {
                block.instructions.push(inst);
            } else {
                block.instructions.insert(offset, inst);
            }
        }
    }

    fn find_object_references(&mut self, func: &Function) {
        for block in func.blocks.values() {
            for param in &block.parameters {
                self.register_gc_object(func, block.id, *param);
                self.offset += 1;
            }

            for inst in block.instructions() {
                // We increment the offset before the main loop body, as the
                // object reference instruction needs to be declared *after* the
                // instruction which declares the register.
                self.offset += 1;

                match &inst.kind {
                    InstructionKind::Let { register, .. } | InstructionKind::Allocate { register, .. } => {
                        self.register_gc_object(func, block.id, *register);
                    }
                    InstructionKind::Store { target, .. } | InstructionKind::StoreField { target, .. } => {
                        self.register_gc_object(func, block.id, *target);
                    }
                    InstructionKind::ObjectRegister { .. }
                    | InstructionKind::Assign { .. }
                    | InstructionKind::CreateSlot { .. }
                    | InstructionKind::StoreSlot { .. } => {}
                }
            }

            self.offset = 0;
        }
    }

    fn register_gc_object(&mut self, func: &Function, block: BasicBlockId, register: RegisterId) {
        if self.marked.contains(&(block, register)) || !func.registers.register_ty(register).requires_stack_map() {
            return;
        }

        self.marked.insert((block, register));
        self.reference_inst.push((block, self.offset, register));
    }
}
