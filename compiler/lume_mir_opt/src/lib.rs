pub(crate) mod pass;

use lume_mir::ModuleMap;
use lume_mir_queries::MirQueryCtx;
use lume_span::NodeId;
use lume_typech::TyCheckCtx;

pub struct Optimizer<'tcx> {
    /// Defines the MIR map which is being optimized.
    mcx: MirQueryCtx<'tcx>,
}

impl<'tcx> Optimizer<'tcx> {
    /// Transforms the supplied context into a MIR map.
    pub fn new(tcx: &'tcx TyCheckCtx, mir: ModuleMap) -> Self {
        Self {
            mcx: MirQueryCtx::new(tcx, mir),
        }
    }

    /// Completes the optimizer and returns the optimized [`ModuleMap`].
    pub fn finish(mut self) -> ModuleMap {
        self.mcx.take_mir()
    }

    /// Optimizes the given MIR, configured by the optimization flags
    /// passed from `tcx`.
    pub fn optimize(tcx: &'tcx TyCheckCtx, mir: ModuleMap) -> ModuleMap {
        let mut optimizer = Self::new(tcx, mir);
        optimizer.execute();
        optimizer.finish()
    }

    /// Invokes all enabled optimization passes on all relevant functions
    /// within the contained [`ModuleMap`].
    pub fn execute(&mut self) {
        let functions = self.mcx.mir().functions.iter();

        let eligible_functions = functions
            .filter_map(|(id, func)| if is_func_eligible(func) { Some(*id) } else { None })
            .collect::<Vec<NodeId>>();

        for func_id in eligible_functions {
            pass::run_all_passes(&mut self.mcx, func_id);
        }
    }
}

/// Determines whether the given function is eligible for optimization.
///
/// Most functions are automatically eligible, but some are excluded because
/// they are too time-consuming or has limited-to-no performance gain. Currently,
/// only external functions are non-eligible for optimization, since they have no
/// body to optimize.
#[inline]
fn is_func_eligible(func: &lume_mir::Function) -> bool {
    if func.signature.external {
        return false;
    }

    true
}
