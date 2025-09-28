use super::*;

/// Detects edges between all blocks within a function, which is used
/// for later passes.
///
/// After edge detection has finished, the function can effectively be
/// remapped into a directed graph of blocks, where each node corresponds
/// to a block and each directed edge corresponds to a call between blocks A and B.
#[derive(Debug, Default)]
pub(crate) struct DefineBlockEdges;

impl Pass for DefineBlockEdges {
    fn name() -> &'static str {
        "define_edges"
    }

    fn new() -> Self {
        Self::default()
    }

    fn execute(&mut self, func: &mut Function) {
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
