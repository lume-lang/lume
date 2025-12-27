use std::path::{Path, PathBuf};

use build_stage::ManifoldDriver;
use error_snippet::IntoDiagnostic;
use lume_errors::{DiagCtx, Result};

use crate::TestResult;
use crate::diff::normalize_output;

pub(crate) fn run_test(path: PathBuf) -> Result<TestResult> {
    let mut map_path = path.clone();
    map_path.set_extension("map");

    let file_content = std::fs::read_to_string(&path).map_err(IntoDiagnostic::into_diagnostic)?;
    let hir_output = build_hir(&path, file_content);

    crate::diff::diff_output_of(hir_output, path, map_path)
}

fn build_hir(path: &Path, content: String) -> String {
    let source_file_name = path.file_name().unwrap();
    let package = build_stage::PackageBuilder::new("<manifold-test>")
        .with_source(source_file_name, content)
        .finish();

    let dcx = DiagCtx::new();
    let manifold_driver = ManifoldDriver::new(package, dcx.clone());

    let map = match manifold_driver.build_hir() {
        Ok(hir) => hir,
        Err(err) => {
            dcx.emit(err);

            let mut renderer = error_snippet::GraphicalRenderer::new();
            renderer.use_colors = false;
            renderer.highlight_source = false;

            owo_colors::set_override(false);
            let buffer = dcx.render_buffer(&mut renderer).unwrap_or_default();

            return normalize_output(&buffer);
        }
    };

    format!("{map:#?}")
}
