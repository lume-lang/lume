mod heap_to_stack;

use lume_mir::*;
use lume_mir_queries::MirQueryCtx;
use lume_session::OptimizationLevel;
use lume_span::NodeId;

/// Defines a MIR optimization pass which can be executed over a function or
/// block.
pub(crate) trait OptimizerPass {
    /// Creates a new instance of the pass without default settings.
    fn new() -> Self;

    /// Determines whether the pass is enabled for the given optimization level.
    fn enabled(level: OptimizationLevel) -> bool;

    /// Executes the pass on the given function.
    fn execute(&mut self, mcx: &mut MirQueryCtx, func_id: NodeId);
}

/// Executes the given optimizer pass on the given function, if the pass is
/// enabled under the optimization level.
#[inline]
pub(crate) fn run_pass<P: OptimizerPass>(mcx: &mut MirQueryCtx, level: OptimizationLevel, func_id: NodeId) {
    if P::enabled(level) {
        let mut pass = P::new();
        pass.execute(mcx, func_id);
    }
}

/// Attempts to run all optimization passes which have been enabled by `level`
/// on the given function.
#[inline]
pub(crate) fn run_all_passes(mcx: &mut MirQueryCtx, func_id: NodeId) {
    let session = &mcx.gcx().session;
    let level = session.options.optimize;

    run_pass::<heap_to_stack::HeapToStack>(mcx, level, func_id);

    // If the `dump_mir` property is defined but empty, we dump the function MIR
    // after all the passes have been executed.
    let func = mcx.mir().function(func_id);

    if mcx.should_dump_func(func, None) {
        println!("{func}");
    }
}
