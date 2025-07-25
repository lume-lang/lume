mod check;
mod inference;
mod query;

use std::sync::Arc;

use error_snippet::Result;
use lume_errors::DiagCtx;
use lume_errors_test::assert_dcx_snapshot;
use lume_hir::map::Map;
use lume_hir_lower::LowerState;
use lume_infer::TyInferCtx;
use lume_session::{GlobalCtx, Package};
use lume_span::{SourceFile, SourceMap};
use lume_types::TyCtx;

use crate::TyCheckCtx;

/// Creates a new [`Package`] instance, which has the standard library included,
/// along with a single source file with the given content.
#[track_caller]
fn package_with_src(input: &str) -> Package {
    let mut project = Package::default();

    project.add_std_sources();
    project.add_source(Arc::new(SourceFile::internal(input)));

    project
}

#[track_caller]
fn lower_into_hir(input: &str) -> Result<Map> {
    let dcx = DiagCtx::new().handle();
    let mut source_map = SourceMap::new();

    let package = package_with_src(input);

    dcx.with(|handle| LowerState::lower(&package, &mut source_map, handle))
}

#[track_caller]
fn type_infer(input: &str) -> Result<TyCheckCtx> {
    let gcx = GlobalCtx::default();
    let tcx = TyCtx::new(Arc::new(gcx));

    let hir = lower_into_hir(input)?;

    let mut tic = TyInferCtx::new(tcx, hir);
    tic.infer()?;

    Ok(TyCheckCtx::new(tic))
}

#[track_caller]
fn empty_tcx() -> TyCheckCtx {
    type_infer("").unwrap()
}

/// Asserts that the given Lume code renders the same output as
/// has been saved and snapshot in a previous iteration.
#[macro_export]
macro_rules! assert_typech_snapshot {
    ($input:expr) => {
        let gcx = lume_session::GlobalCtx::new(lume_session::Session::default(), lume_errors::DiagCtx::new());
        let tcx = lume_types::TyCtx::new(std::sync::Arc::new(gcx));

        let hir = $crate::tests::lower_into_hir($input).unwrap();

        let mut tic = lume_infer::TyInferCtx::new(tcx, hir);
        let _ = tic.infer();

        let mut tcc = $crate::TyCheckCtx::new(tic);
        let _ = tcc.typecheck();

        let dcx = tcc.dcx();

        $crate::tests::assert_dcx_snapshot!($input, &dcx);
    };
}
