use std::sync::Arc;

use error_snippet::Result;
use lume_errors::DiagCtx;
use lume_hir::map::Map;
use lume_hir_lower::LowerState;
use lume_session::{GlobalCtx, Package};
use lume_span::{PackageId, SourceFile, SourceMap};
use lume_types::TyCtx;

use crate::TyInferCtx;

mod inference;

/// Creates a new [`Package`] instance, which has the standard library included,
/// along with a single source file with the given content.
#[track_caller]
fn package_with_src(input: &str) -> Package {
    let mut project = Package {
        id: PackageId::from_usize(1),
        ..Default::default()
    };

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
fn type_infer(input: &str) -> Result<TyInferCtx> {
    let gcx = GlobalCtx::default();
    let tcx = TyCtx::new(Arc::new(gcx));

    let hir = lower_into_hir(input)?;

    let mut tic = TyInferCtx::new(tcx, hir);
    tic.infer()?;

    Ok(tic)
}
