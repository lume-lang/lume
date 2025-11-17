use std::collections::HashSet;

use indexmap::{IndexMap, IndexSet};
use lume_mir::*;

pub(crate) mod define_block_params;
pub(crate) mod define_edges;
pub(crate) mod mark_gc_refs;
pub(crate) mod pass_block_args;
pub(crate) mod remove_orphans;
pub(crate) mod remove_unreachable;
pub(crate) mod rename_ssa;
pub(crate) mod ssa_assign;

use crate::FunctionTransformer;

/// Defines a MIR pass which can be executed over a function, during MIR
/// lowering.
///
/// Passes can perform a variety of actions - some required, some optional.
/// Examples include **escape analysis**, **removing unused blocks/registers**,
/// **marking objects for the GC**, or just normal optimization passes.
pub(crate) trait Pass {
    /// Returns the unique name of the pass.
    fn name() -> &'static str;

    /// Creates a new instance of the pass without default settings.
    fn new() -> Self;

    /// Executes the pass on the given function.
    fn execute(&mut self, func: &mut Function);
}

impl FunctionTransformer<'_, '_> {
    pub(crate) fn run_passes(&mut self) {
        self.run_pass::<remove_unreachable::RemoveUnreachable>();
        self.run_pass::<define_edges::DefineBlockEdges>();
        self.run_pass::<remove_orphans::RemoveOrphanBlocks>();
        self.run_pass::<define_block_params::DefineBlockParameters>();
        self.run_pass::<pass_block_args::PassBlockArguments>();
        self.run_pass::<ssa_assign::ConvertAssignmentExpressions>();
        self.run_pass::<rename_ssa::RenameSsaVariables>();
        self.run_pass::<mark_gc_refs::MarkObjectReferences>();
    }

    #[inline]
    pub(crate) fn run_pass<P: Pass>(&mut self) {
        let name = P::name();

        if let Some(dump_mir) = self.tcx().gcx().session.options.dump_mir.as_ref()
            && dump_mir.contains(&String::from(name))
        {
            println!("{}", self.func);
        }

        let mut pass = P::new();
        pass.execute(&mut self.func);
    }
}
