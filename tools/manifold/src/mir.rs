use std::path::PathBuf;

use lume_errors::{DiagCtx, MapDiagnostic, Result};

use crate::diff::render_dcx_output;
use crate::{TestPath, TestResult};

pub(crate) fn run_test(path: TestPath) -> Result<TestResult> {
    let mut map_path = path.absolute.0.clone();
    map_path.set_extension("mir");

    let file_content = std::fs::read_to_string(&path.absolute.0).map_diagnostic()?;
    let mir_output = build_mir(&path, file_content)?;

    crate::diff::diff_output_of(mir_output, path.relative.0, map_path)
}

fn build_mir(path: &TestPath, content: String) -> Result<String> {
    let dcx = DiagCtx::new();
    let file_name = path.relative.file_name().unwrap();

    let pipeline = lume_driver::test_support::workspace(&*path.root)
        .with_option(|opts| opts.enable_incremental = false)
        .with_file(
            "Arcfile",
            r#"
                [package]
                name = "<manifold-test>"
                version = "1.0.0"
                lume_version = "^0"
            "#,
        )
        .with_file(PathBuf::from("src").join(file_name), &content)
        .pipeline(dcx.handle())?;

    let mir_result = || -> Result<lume_driver::LoweredToMir> {
        pipeline.lower_to_hir()?.type_check()?.lower_to_tir()?.lower_to_mir()
    }();

    let mir = match mir_result {
        Ok(lume_driver::LoweredToMir { gcx, mut mir }) => {
            let root_package_id = gcx.session.dep_graph.root;

            let lume_driver::StageResult::Value(result) = mir.swap_remove(&root_package_id).unwrap() else {
                panic!("unable to find root package");
            };

            result.1
        }
        Err(err) => {
            dcx.emit(err);

            return Ok(render_dcx_output(&dcx));
        }
    };

    let filtered_functions: Vec<_> = mir
        .functions
        .values()
        .filter(|func| func.location.file.name.to_pathbuf().ends_with(file_name))
        .map(ToString::to_string)
        .collect();

    Ok(filtered_functions.join(""))
}
