use lume_mir::Function;

use crate::MirQueryCtx;

impl<'tcx> MirQueryCtx<'tcx> {
    /// Attempts to find the function definition with the given name
    /// within the MIR.
    pub fn find_function(&'tcx self, name: &str) -> Option<&'tcx Function> {
        self.mir().functions.values().find(move |func| func.name == name)
    }
}
