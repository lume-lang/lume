use super::*;

/// Detects unreachable instructions of blocks and removes them. When an
/// unreachable instruction is found, the pass might update the terminator of
/// the block to be `Unreachable`.
#[derive(Debug, Default)]
pub(crate) struct RemoveUnreachable;

impl Pass for RemoveUnreachable {
    fn name() -> &'static str {
        "remove_unreachable"
    }

    fn new() -> Self {
        Self
    }

    fn execute(&mut self, func: &mut Function) {
        let mut unreachable = Vec::<(BasicBlockId, usize)>::new();

        for block in func.blocks.values() {
            for (idx, inst) in block.instructions().enumerate() {
                let InstructionKind::Let { ty, .. } = &inst.kind else {
                    continue;
                };

                if ty.is_never() {
                    unreachable.push((block.id, idx));
                }
            }
        }

        for (block, last_reachable) in unreachable {
            let block = func.block_mut(block);
            let location = block.instructions[last_reachable].location;

            block.instructions.drain(last_reachable + 1..);
            block.set_terminator_full(Terminator {
                kind: TerminatorKind::Unreachable,
                location,
            });
        }
    }
}
