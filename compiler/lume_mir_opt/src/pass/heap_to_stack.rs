//! Optimizer pass to convert heap-allocations into stack-allocations when
//! the compiler can be absolutely sure the allocation doesn't escape the
//! method.
//!
//! Since stack allocations are much cheaper than heap allocations, this can be
//! an immense speed boost.

use lume_mir_queries::analysis::EscapeResult;

use crate::pass::*;

#[derive(Debug, Default)]
pub(super) struct HeapToStack;

impl OptimizerPass for HeapToStack {
    #[inline]
    fn new() -> Self {
        Self
    }

    #[inline]
    fn enabled(level: OptimizationLevel) -> bool {
        level.speed_level() >= 2
    }

    fn execute(&mut self, mcx: &mut MirQueryCtx, func_id: NodeId) {
        let mut replacements = Vec::new();

        let func = mcx.mir().function(func_id);

        for block in func.blocks.values() {
            for (idx, inst) in block.instructions().enumerate() {
                let InstructionKind::Allocate { register, .. } = &inst.kind else {
                    continue;
                };

                if mcx.does_register_escape(func, block.id, *register) == EscapeResult::Unescaped {
                    replacements.push((block.id, idx));
                }
            }
        }

        let func = mcx.mir_mut().function_mut(func_id);

        let mut slot_updates = Vec::new();

        for (block_id, offset) in replacements {
            let block = func.block(block_id);
            let inst = block.instructions.get(offset).unwrap();

            let InstructionKind::Allocate { register, ty, .. } = inst.kind.clone() else {
                unreachable!();
            };

            let slot = func.add_slot(ty.clone());
            let block = func.block_mut(block_id);
            let inst = block.instructions.get_mut(offset).unwrap();

            inst.kind = InstructionKind::CreateSlot { slot, ty };

            slot_updates.push((block_id, register, slot, offset));
        }

        for (block_id, register, slot, offset) in slot_updates {
            replace_reg_with_slot(mcx, func_id, block_id, register, slot, offset);
        }
    }
}

/// Replaces all references of the register `register` with corresponding
/// instructions which utilize the slot `slot`.
fn replace_reg_with_slot(
    mcx: &mut MirQueryCtx,
    func_id: NodeId,
    block_id: BasicBlockId,
    register: RegisterId,
    slot: SlotId,
    offset: usize,
) {
    let func = mcx.mir_mut().function_mut(func_id);
    let block = func.block_mut(block_id);

    for inst in block.instructions_mut().skip(offset) {
        match &mut inst.kind {
            InstructionKind::Let { register: id, decl, .. } => {
                debug_assert_ne!(*id, register, "declaration instruction should have been skipped");

                match &mut decl.kind {
                    DeclarationKind::Operand(operand) => replace_reg_in_op(operand, register, slot),
                    DeclarationKind::Cast { .. } => unreachable!("can't cast stack-slot"),
                    DeclarationKind::Intrinsic { args, .. } | DeclarationKind::Call { args, .. } => {
                        for arg in args {
                            if arg.references_register(register) {
                                replace_reg_in_op(arg, register, slot);
                            }
                        }
                    }
                    DeclarationKind::IndirectCall { ptr, args, .. } => {
                        debug_assert_ne!(*ptr, register);

                        for arg in args {
                            if arg.references_register(register) {
                                replace_reg_in_op(arg, register, slot);
                            }
                        }
                    }
                }
            }
            InstructionKind::StoreField { target, offset, value } => {
                if *target == register {
                    inst.kind = InstructionKind::StoreSlot {
                        target: slot,
                        value: value.to_owned(),
                        offset: *offset,
                    }
                } else {
                    replace_reg_in_op(value, register, slot);
                }
            }
            InstructionKind::Assign { .. }
            | InstructionKind::CreateSlot { .. }
            | InstructionKind::Allocate { .. }
            | InstructionKind::Store { .. }
            | InstructionKind::StoreSlot { .. }
            | InstructionKind::ObjectRegister { .. } => {}
        }
    }

    match &mut block.terminator_mut().unwrap().kind {
        TerminatorKind::Return(Some(value)) => {
            if value.references_register(register) {
                replace_reg_in_op(value, register, slot);
            }
        }
        TerminatorKind::Branch(call) => {
            replace_reg_in_call_site(call, register, slot);
        }
        TerminatorKind::ConditionalBranch {
            then_block, else_block, ..
        } => {
            replace_reg_in_call_site(then_block, register, slot);
            replace_reg_in_call_site(else_block, register, slot);
        }
        TerminatorKind::Switch { arms, fallback, .. } => {
            for (_, arm) in arms {
                replace_reg_in_call_site(arm, register, slot);
            }

            replace_reg_in_call_site(fallback, register, slot);
        }
        TerminatorKind::Return(None) | TerminatorKind::Unreachable => {}
    }

    // Remove any object register instructions which point to the register,
    // since it is now no longer a register.
    block.instructions.retain(|ins| {
        if let InstructionKind::ObjectRegister { register: target } = &ins.kind
            && *target == register
        {
            false
        } else {
            true
        }
    });
}

fn replace_reg_in_call_site(call: &mut BlockBranchSite, register: RegisterId, slot: SlotId) {
    for arg in &mut call.arguments {
        if arg.references_register(register) {
            replace_reg_in_op(arg, register, slot);
        }
    }
}

fn replace_reg_in_op(operand: &mut Operand, register: RegisterId, slot: SlotId) {
    match &mut operand.kind {
        OperandKind::Boolean { .. }
        | OperandKind::Integer { .. }
        | OperandKind::Float { .. }
        | OperandKind::String { .. } => unreachable!(),
        OperandKind::Bitcast { .. } => unreachable!("can't bitcast stack slot"),
        OperandKind::Load { .. } => unimplemented!(),
        OperandKind::LoadField {
            target,
            offset,
            field_type,
            ..
        } => {
            if *target == register {
                operand.kind = OperandKind::LoadSlot {
                    target: slot,
                    offset: *offset,
                    loaded_type: field_type.clone(),
                }
            }
        }
        OperandKind::LoadSlot { .. } | OperandKind::SlotAddress { .. } => {}
        OperandKind::Reference { id } => {
            if *id == register {
                operand.kind = OperandKind::SlotAddress { id: slot, offset: 0 }
            }
        }
    }
}
