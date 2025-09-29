mod heap_to_stack;

use lume_mir::*;
use lume_session::OptimizationLevel;

/// Defines a MIR optimization pass which can be executed over a function or block.
pub(crate) trait OptimizerPass {
    /// Creates a new instance of the pass without default settings.
    fn new() -> Self;

    /// Determines whether the pass is enabled for the given optimization level.
    fn enabled(level: OptimizationLevel) -> bool;

    /// Executes the pass on the given function.
    fn execute(&mut self, func: &mut Function);
}

/// Executes the given optimizer pass on the given function, if the pass is
/// enabled under the optimization level.
#[inline]
pub(crate) fn run_pass<P: OptimizerPass>(level: OptimizationLevel, func: &mut Function) {
    if P::enabled(level) {
        let mut pass = P::new();
        pass.execute(func);
    }
}

/// Attempts to run all optimization passes which have been enabled by `level` on
/// the given function.
#[inline]
pub(crate) fn run_all_passes(level: OptimizationLevel, func: &mut Function) {
    run_pass::<heap_to_stack::HeapToStack>(level, func);
}
