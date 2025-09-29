//! Optimizer pass to convert heap-allocations into stack-allocations when
//! the compiler can be absolutely sure the allocation doesn't escape the method.
//!
//! Since stack allocations are much cheaper than heap allocations, this can be
//! an immense speed boost.

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

    fn execute(&mut self, func: &mut Function) {
        // ..
    }
}
