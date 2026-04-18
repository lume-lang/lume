use indexmap::IndexMap;
use lume_mir::*;
use lume_span::Interned;

use crate::*;

impl<'tcx> MirQueryCtx<'tcx> {
    /// Create a new MIR patcher for the given MIR function.
    #[inline]
    pub fn patcher(&self, func: &Function) -> Patcher<'_, 'tcx> {
        Patcher::new(self, func)
    }
}

struct RegisterMapping {
    pub func_name: Interned<String>,

    /// Mapping between old register and new register. The block for the new
    /// register is the same as within the key.
    pub mapping: IndexMap<LocalRegister, RegisterId>,
}

impl RegisterMapping {
    pub fn new(func_name: Interned<String>) -> Self {
        Self {
            func_name,
            mapping: IndexMap::new(),
        }
    }

    #[track_caller]
    pub fn get(&self, block: BasicBlockId, id: RegisterId) -> RegisterId {
        match self.mapping.get(&LocalRegister { register: id, block }) {
            Some(register_id) => *register_id,
            None => panic!("could not find register {block}.{id} in `{}`", self.func_name),
        }
    }

    pub fn insert(&mut self, old: LocalRegister, new: RegisterId) {
        assert!(self.mapping.insert(old, new).is_none());
    }
}

/// Represents a placement within a basic block, relative to some instruction.
#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct RelativePlacement {
    pub block_id: BasicBlockId,
    pub relative: Placement,
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Placement {
    /// The target should be placed at the beginning of the block.
    Beginning,

    /// The reference should be placed before the target instruction.
    Before { inst: InstructionId },

    /// The reference should be placed after the target instruction.
    After { inst: InstructionId },
}

pub struct Patcher<'mcx, 'tcx> {
    pub mcx: &'mcx MirQueryCtx<'tcx>,
    mapped_registers: RegisterMapping,
    new_instructions: Vec<(RelativePlacement, InstructionKind)>,

    next_local: usize,
}

impl<'mcx, 'tcx> Patcher<'mcx, 'tcx> {
    pub fn new(mcx: &'mcx MirQueryCtx<'tcx>, func: &Function) -> Self {
        Self {
            mcx,
            mapped_registers: RegisterMapping::new(func.name),
            new_instructions: Vec::new(),
            next_local: 0,
        }
    }

    pub fn next_local(&mut self) -> RegisterId {
        let local = RegisterId::new(self.next_local);
        self.next_local += 1;

        local
    }

    #[tracing::instrument(
        level = "TRACE",
        skip_all,
        fields(func = self.mapped_registers.func_name.as_str(), %block, %old, %new)
    )]
    pub fn rename_register(&mut self, block: BasicBlockId, old: RegisterId, new: RegisterId) {
        self.mapped_registers
            .insert(LocalRegister { block, register: old }, new);
    }

    #[tracing::instrument(level = "TRACE", skip_all)]
    pub fn add_instruction(&mut self, placement: RelativePlacement, kind: InstructionKind) {
        self.new_instructions.push((placement, kind));
    }

    pub fn apply(mut self, target: &mut Function) {
        if !self.new_instructions.is_empty() {
            add_instruction::apply(&mut self, target);
        }

        if !self.mapped_registers.mapping.is_empty() {
            rename::apply(&mut self, target);
        }
    }
}

mod rename {
    use super::*;

    pub fn apply(p: &mut Patcher<'_, '_>, target: &mut Function) {
        // Handle cases where a renamed register crosses a block-boundary, causing two
        // blocks to use the same register:
        // ```mir
        // fn "foo" () {
        // B0:
        //     let #1: u64 = 0_u64
        //     goto B1(#1)
        // B1(#1):  B0
        //     return #1
        // }
        // ```
        // Even though `#1` is clearly defined in `B0`, it is used within `B1` and is
        // passed in as a block parameter. This causes issues when renaming a register
        // within a specific block
        //
        // Imagine we want to rename `B1.#1` to `B1.#2` (which we do in the `rename_ssa`
        // MIR pass), we have to duplicate the register, since it would become distinct
        // from `B0.#1`. We do this *before* applying the renaming, since we need the
        // type of the source register.
        let mut new_registers = IndexMap::new();

        for (&local_source, &dest_register) in &p.mapped_registers.mapping {
            // If the function's register already exists in the correct block, skip over it.
            if let Some(register) = target.registers.locals.get(&local_source.register)
                && register.block.is_some_and(|block| block == local_source.block)
            {
                continue;
            }

            let value_type = target.registers.local_type(local_source.register).clone();

            new_registers.insert(dest_register, Register {
                id: dest_register,
                value_type,
                block: Some(local_source.block),
            });
        }

        // Replace all register references within the function's blocks, parameters,
        // instructions and terminator.
        for (&block_id, block) in &mut target.blocks {
            for parameter in std::mem::take(&mut block.parameters) {
                let replacement_id = p.mapped_registers.get(block_id, parameter);
                assert!(block.parameters.insert(replacement_id));
            }

            for inst in block.instructions_mut() {
                rename::rename_inst(inst, block_id, &p.mapped_registers);
            }

            if let Some(term) = block.terminator_mut() {
                rename::rename_term(term, block_id, &p.mapped_registers);
            }
        }

        // Update the register mapping within the function's own registers, so we can
        // still look them up in later passes.
        for (register_id, mut register) in std::mem::take(&mut target.registers.locals) {
            let block = register.block.unwrap_or_default();
            let replacement_id = p.mapped_registers.get(block, register_id);
            register.id = replacement_id;

            assert!(target.registers.locals.insert(replacement_id, register).is_none());
        }

        // Insert all the new registers at the end. Since the locals are indexed in a
        // map, their position does not matter.
        target.registers.locals.extend(new_registers);
    }

    pub(super) fn rename_inst(inst: &mut Instruction, block: BasicBlockId, mapping: &RegisterMapping) {
        match &mut inst.kind {
            InstructionKind::Let { register, decl, .. } => {
                *register = mapping.get(block, *register);
                rename_decl(decl, block, mapping);
            }
            InstructionKind::Allocate { register, metadata, .. } => {
                *register = mapping.get(block, *register);
                *metadata = mapping.get(block, *metadata);
            }
            InstructionKind::Store { target, value } | InstructionKind::StoreField { target, value, .. } => {
                *target = mapping.get(block, *target);
                rename_operand(value, block, mapping);
            }
            InstructionKind::ObjectRegister { register } => {
                *register = mapping.get(block, *register);
            }
            InstructionKind::CreateSlot { .. } | InstructionKind::StoreSlot { .. } => {}
        }
    }

    pub(super) fn rename_term(term: &mut Terminator, block: BasicBlockId, mapping: &RegisterMapping) {
        match &mut term.kind {
            TerminatorKind::Return(op) => {
                if let Some(value) = op {
                    rename_operand(value, block, mapping);
                }
            }
            TerminatorKind::Branch(site) => {
                for arg in &mut site.arguments {
                    rename_operand(arg, block, mapping);
                }
            }
            TerminatorKind::ConditionalBranch {
                condition,
                then_block,
                else_block,
            } => {
                *condition = mapping.get(block, *condition);

                for arg in &mut then_block.arguments {
                    rename_operand(arg, block, mapping);
                }

                for arg in &mut else_block.arguments {
                    rename_operand(arg, block, mapping);
                }
            }
            TerminatorKind::Switch {
                operand,
                arms,
                fallback,
            } => {
                *operand = mapping.get(block, *operand);

                for arm in arms {
                    for arg in &mut arm.1.arguments {
                        rename_operand(arg, block, mapping);
                    }
                }

                for arg in &mut fallback.arguments {
                    rename_operand(arg, block, mapping);
                }
            }
            TerminatorKind::Unreachable => {}
        }
    }

    pub(super) fn rename_decl(decl: &mut Declaration, block: BasicBlockId, mapping: &RegisterMapping) {
        match decl.kind.as_mut() {
            DeclarationKind::Operand(operand) => {
                rename_operand(operand, block, mapping);
            }
            DeclarationKind::Cast { operand, .. } => {
                *operand = mapping.get(block, *operand);
            }
            DeclarationKind::Call { args, .. } | DeclarationKind::Intrinsic { args, .. } => {
                for arg in args {
                    rename_operand(arg, block, mapping);
                }
            }
            DeclarationKind::IndirectCall { ptr, args, .. } => {
                *ptr = mapping.get(block, *ptr);

                for arg in args {
                    rename_operand(arg, block, mapping);
                }
            }
        }
    }

    pub(super) fn rename_operand(op: &mut Operand, block: BasicBlockId, mapping: &RegisterMapping) {
        match &mut op.kind {
            OperandKind::Load { id, .. } | OperandKind::Reference { id } | OperandKind::Untagged { id } => {
                *id = mapping.get(block, *id);
            }
            OperandKind::LoadField { target, .. } => {
                *target = mapping.get(block, *target);
            }
            OperandKind::Bitcast { source, .. } => {
                *source = mapping.get(block, *source);
            }
            OperandKind::Boolean { .. }
            | OperandKind::Integer { .. }
            | OperandKind::Float { .. }
            | OperandKind::String { .. }
            | OperandKind::LoadSlot { .. }
            | OperandKind::SlotAddress { .. } => {}
        }
    }
}

mod add_instruction {
    use super::*;

    pub fn apply(p: &mut Patcher<'_, '_>, target: &mut Function) {
        for (placement, instruction_kind) in std::mem::take(&mut p.new_instructions) {
            let block = target.block(placement.block_id);
            let location = match placement.relative {
                Placement::Beginning => target.location,
                Placement::Before { inst } | Placement::After { inst } => {
                    block.instructions.get(&inst).unwrap().location
                }
            };

            let new_instruction_id = InstructionId(block.instructions.len());
            let new_instruction = lume_mir::Instruction {
                kind: instruction_kind,
                location,
            };

            let block = target.block_mut(placement.block_id);

            match placement.relative {
                Placement::Beginning => {
                    block.instructions.shift_insert(0, new_instruction_id, new_instruction);
                }

                Placement::Before { inst } | Placement::After { inst } => {
                    let target_offset = block.instructions.get_index_of(&inst).unwrap_or(0);

                    match placement.relative {
                        Placement::Before { .. } => {
                            block
                                .instructions
                                .insert_before(target_offset, new_instruction_id, new_instruction);
                        }
                        Placement::After { .. } => {
                            if target_offset == block.instructions.len() {
                                block.instructions.insert(new_instruction_id, new_instruction);
                            } else {
                                block.instructions.insert_before(
                                    target_offset + 1,
                                    new_instruction_id,
                                    new_instruction,
                                );
                            }
                        }
                        Placement::Beginning => unreachable!(),
                    }
                }
            }
        }
    }
}
