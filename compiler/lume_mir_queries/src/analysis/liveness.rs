use std::collections::{HashMap, VecDeque};

use indexmap::{IndexMap, IndexSet};
use lume_mir::*;

use crate::MirQueryCtx;

/// Represents the liveness information for a single basic block.
///
/// Liveness information is used to determine which variables are live within
/// any basic block, which is required when passing control flow.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct BlockLiveness {
    /// Set of all variables which are required as inputs for the block.
    pub live_in: IndexSet<VariableId>,

    /// Set of all variables which are required as outputs for the block, keyed
    /// by the successor block.
    pub live_out: IndexMap<BasicBlockId, IndexSet<VariableId>>,
}

impl BlockLiveness {
    /// Flattens the live-out set into a single iterator.
    pub fn flatten_out(&self) -> impl Iterator<Item = VariableId> {
        self.live_out.values().flatten().copied()
    }
}

impl std::fmt::Display for BlockLiveness {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "live-in:\t\t{}",
            self.live_in
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )?;

        for (successor, live_out) in &self.live_out {
            writeln!(
                f,
                "live-out[->{successor}]:\t{}",
                live_out.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ")
            )?;
        }

        Ok(())
    }
}

impl MirQueryCtx<'_> {
    /// Computes the liveness information for each basic block in the given
    /// function.
    ///
    /// Liveness information is used to determine which variables are live
    /// within any basic block, which is required when passing control flow.
    pub fn liveness(&self, func: &Function) -> IndexMap<BasicBlockId, BlockLiveness> {
        // This method computes the liveness of the function using basic liveness
        // analysis.
        //
        // For each block, we compute the live-in and live-out sets given the current
        // inputs and outputs of predecessor blocks. Since the graph can be cyclic,
        // we use a worklist to ensure that we keep visiting blocks until we converge
        // on a fixed-point.

        let mut liveness = IndexMap::new();

        // The use-set and def-set for each block only define which variables are
        // directly used and defined within the block, respectively. They do not
        // contain any variables required by the block's successors or predecessors.
        let mut def_sets: HashMap<BasicBlockId, IndexSet<VariableId>> = HashMap::new();
        let mut use_sets: HashMap<BasicBlockId, IndexSet<VariableId>> = HashMap::new();

        for (&block_id, block) in &func.blocks {
            liveness.insert(block_id, BlockLiveness::default());

            let def_set = func.variables.definitions_in(block.id).map(|(_reg, var)| var).collect();
            let use_set = func.variables.references_in(block.id).collect::<IndexSet<_>>();

            let local_use_set = use_set.difference(&def_set).copied().collect::<IndexSet<_>>();

            def_sets.insert(block_id, def_set);
            use_sets.insert(block_id, local_use_set);
        }

        let mut worklist: VecDeque<BasicBlockId> = func.blocks.keys().copied().collect();

        while let Some(block_id) = worklist.pop_front() {
            let block = func.block(block_id);

            let def_set = def_sets.get(&block_id).unwrap();
            let use_set = use_sets.get(&block_id).unwrap();

            let old_liveness = liveness.get(&block_id).unwrap().clone();
            let mut new_liveness = BlockLiveness::default();

            // live_out[B] = ⋃ live_in[S] for all successors S
            for successor in block.successors() {
                let successor_liveness = liveness.get(&successor).unwrap();

                new_liveness
                    .live_out
                    .entry(successor)
                    .or_default()
                    .extend(&successor_liveness.live_in);
            }

            // live_in[B] = use[B] ⋃ (live_out[B] - def[B])
            new_liveness.live_in.extend(use_set);

            let flattened_outputs = new_liveness.flatten_out().collect::<IndexSet<_>>();
            for live_in in flattened_outputs.difference(def_set) {
                new_liveness.live_in.insert(*live_in);
            }

            // If the liveness has changed, add the back-edges back onto the worklist.
            //
            // The liveness will eventually converge, since there's a finite number of
            // variables and dataflow within the CFG is already known.
            if new_liveness != old_liveness {
                worklist.extend(block.predecessors());
            }

            liveness.insert(block_id, new_liveness);
        }

        liveness
    }
}
