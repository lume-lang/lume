use lume_mir::Function;
use lume_span::NodeId;

use crate::MirQueryCtx;

impl<'tcx> MirQueryCtx<'tcx> {
    /// Attempts to find the function definition with the given ID
    /// within the MIR.
    #[inline]
    pub fn function(&'tcx self, id: NodeId) -> Option<&'tcx Function> {
        self.mir().functions.values().find(move |func| func.id == id)
    }
}
