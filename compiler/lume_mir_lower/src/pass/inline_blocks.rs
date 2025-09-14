use lume_mir::*;

/// Attempts to inline blocks whose sole purpose is to jump to another block. This
/// is often the case with if-expressions or loop statements, which end up creating
/// vestigial blocks in the lowering process.
#[derive(Default, Debug)]
pub(super) struct InlineBlocks;

#[derive(Clone, PartialEq)]
enum InlineTransformation {
    /// Inline the terminator from `source_block` into `target_block` and assume
    /// no other changes are required.
    InlineTerminator {
        source_block: BasicBlockId,
        target_block: BasicBlockId,
    },
}

impl InlineBlocks {
    pub fn execute(&self, func: &mut Function) {
        let mut transforms = Vec::new();

        for block in &func.blocks {
            let terminator = block.terminator().expect("expected all blocks to contain a terminator");

            match &terminator.kind {
                TerminatorKind::Branch(call) if self.can_block_be_inlined(func, call.block) => {
                    transforms.push(InlineTransformation::InlineTerminator {
                        source_block: call.block,
                        target_block: block.id,
                    });
                }
                _ => {}
            }
        }

        while let Some(transform) = transforms.pop() {
            match transform {
                InlineTransformation::InlineTerminator {
                    source_block,
                    target_block,
                } => {
                    let source = func.block(source_block);
                    let source_terminator = source
                        .terminator()
                        .expect("expected all blocks to contain a terminator")
                        .clone();

                    func.block_mut(target_block).set_terminator_full(source_terminator);
                    func.block_mut(source_block).set_usage(BlockUsage::Unused);
                }
            }
        }
    }

    /// Determines whether the given block can be inlined into whichever parent
    /// block branches to it.
    fn can_block_be_inlined(&self, func: &Function, id: BasicBlockId) -> bool {
        let block = func.block(id);

        // We exclude blocks which have any instructions, since they may
        // be harder to actually inline.
        if block.instructions().count() > 0 {
            return false;
        }

        true
    }
}
