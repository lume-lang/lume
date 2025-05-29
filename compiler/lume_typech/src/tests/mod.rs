mod query;

use std::sync::Arc;

use arc::Package;
use error_snippet::Result;
use lume_errors::{DiagCtx, DiagOutputFormat};
use lume_hir::map::Map;
use lume_hir_lower::LowerState;
use lume_span::{SourceFile, SourceMap};

use crate::ThirBuildCtx;

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
    let mut dcx = DiagCtx::new(DiagOutputFormat::Stubbed);
    let mut source_map = SourceMap::new();

    let package = package_with_src(input);

    dcx.with(|handle| LowerState::lower(&package, &mut source_map, handle))
}

#[track_caller]
fn type_infer(input: &str) -> Result<(ThirBuildCtx, Map)> {
    let mut dcx = DiagCtx::new(DiagOutputFormat::Stubbed);
    let mut hir = lower_into_hir(input)?;

    let mut tcx = dcx.with(ThirBuildCtx::new);
    tcx.define_types(&mut hir)?;

    Ok((tcx, hir))
}
