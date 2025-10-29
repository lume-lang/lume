pub mod analysis;
pub mod lookup;

use lume_architect::DatabaseContext;
use lume_mir::ModuleMap;
use lume_session::GlobalCtx;
use lume_typech::TyCheckCtx;

/// Data structure for querying information from an MIR map,
/// which has either been fully lowered or is currently being lowered.
pub struct MirQueryCtx<'tcx> {
    tcx: &'tcx TyCheckCtx,

    /// Defines the MIR map which is being queried on.
    mir: ModuleMap,
}

impl<'tcx> MirQueryCtx<'tcx> {
    /// Creates a new querying context.
    pub fn new(tcx: &'tcx TyCheckCtx, mir: ModuleMap) -> Self {
        Self { tcx, mir }
    }

    /// Gets the inner [`TyCheckCtx`] reference.
    pub fn tcx(&self) -> &TyCheckCtx {
        self.tcx
    }

    /// Gets the inner [`GlobalCtx`] reference.
    pub fn gcx(&self) -> &GlobalCtx {
        self.tcx().gcx()
    }

    /// Gets the inner [`ModuleMap`] reference.
    pub fn mir(&self) -> &ModuleMap {
        &self.mir
    }

    /// Gets the inner [`ModuleMap`] reference.
    pub fn mir_mut(&mut self) -> &mut ModuleMap {
        &mut self.mir
    }

    /// Takes the inner [`ModuleMap`] our of the query context.
    pub fn take_mir(&mut self) -> ModuleMap {
        std::mem::take(&mut self.mir)
    }
}

impl DatabaseContext for MirQueryCtx<'_> {
    fn db(&self) -> &lume_architect::Database {
        self.gcx().db()
    }
}
