use super::*;

/// Removes all blocks, except the entry block, which have no predecessors,
/// since they only serve to clobber future passes and codegen.
///
/// The pass also removes any orphan blocks as predecessor of any successor
/// blocks.
#[derive(Default, Debug)]
pub(crate) struct RemoveOrphanBlocks;

impl Pass for RemoveOrphanBlocks {
    fn name() -> &'static str {
        "remove_orphans"
    }

    /// Creates a new instance of the pass without default settings.
    fn new() -> Self {
        Self
    }

    /// Executes the pass on the given function.
    fn execute(&mut self, func: &mut Function) {
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
