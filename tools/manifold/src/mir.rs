use std::path::{Path, PathBuf};

use build_stage::ManifoldDriver;
use error_snippet::IntoDiagnostic;
use lume_errors::{DiagCtx, Result};

use crate::TestResult;
use crate::diff::normalize_output;

pub(crate) fn run_test(path: PathBuf) -> Result<TestResult> {
    let mut map_path = path.clone();
    map_path.set_extension("mir");

    let file_content = std::fs::read_to_string(&path).map_err(IntoDiagnostic::into_diagnostic)?;
    let mir_output = build_mir(&path, file_content);

    crate::diff::diff_output_of(mir_output, path, map_path)
}

fn build_mir(path: &Path, content: String) -> String {
    let source_file_name = path.file_name().unwrap();

    let package = build_stage::PackageBuilder::new("<manifold-test>")
        .with_source(source_file_name, content)
        .with_standard_library()
        .finish();

    let dcx = DiagCtx::new();
    let manifold_driver = ManifoldDriver::new(package, dcx.clone());

    let mir = match manifold_driver.build_mir() {
        Ok(mir) => mir,
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

    let filtered_functions: Vec<_> = mir
        .functions
        .values()
        .filter(|func| func.location.file.name.to_pathbuf().ends_with(source_file_name))
        .map(ToString::to_string)
        .collect();

    filtered_functions.join("")
}
