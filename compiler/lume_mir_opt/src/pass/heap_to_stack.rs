//! Optimizer pass to convert heap-allocations into stack-allocations when
//! the compiler can be absolutely sure the allocation doesn't escape the method.
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
        Self::default()
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

        for (block_id, offset) in replacements {
            let block = func.block(block_id);
            let inst = block.instructions.get(offset).unwrap();

            let InstructionKind::Allocate { ty, .. } = inst.kind.clone() else {
                continue;
            };

            let slot_id = func.add_slot(ty.clone());

            let block = func.block_mut(block_id);
            let inst = block.instructions.get_mut(offset).unwrap();

            inst.kind = InstructionKind::CreateSlot { slot: slot_id, ty };
        }
    }
}
