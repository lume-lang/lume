#![feature(map_try_insert)]

use std::ops::Deref;

use lume_errors::DiagCtx;
use lume_hir::SymbolName;
use lume_infer::TyInferCtx;
use lume_infer::query::CallReference;
use lume_types::TypeDatabaseContext;

mod check;
mod errors;
pub(crate) mod query;
#[cfg(test)]
mod tests;

/// Central data structure for checking compatibility between types within the source package.
///
/// Notibly, [`TyCheckCtx`] is not responsible for inference. While it may perform some inference
/// during the type-checking stage, the inference is deferred to the inner [`TyInferCtx`] instance.
pub struct TyCheckCtx {
    infer: TyInferCtx,
}

impl TyCheckCtx {
    /// Creates a new empty type checker context.
    pub fn new(infer: TyInferCtx) -> TyCheckCtx {
        TyCheckCtx { infer }
    }

    /// Retrieves the High-Level Intermediate Representation (HIR) map from the build context.
    pub fn hir(&self) -> &lume_hir::map::Map {
        self.infer.hir()
    }

    /// Retrieves the type context from the build context.
    pub fn tdb(&self) -> &TypeDatabaseContext {
        self.infer.tdb()
    }

    /// Retrieves the diagnostics handler from the parent context.
    pub fn dcx(&self) -> DiagCtx {
        self.infer.dcx()
    }
}

impl Deref for TyCheckCtx {
    type Target = TyInferCtx;

    fn deref(&self) -> &Self::Target {
        &self.infer
    }
}
