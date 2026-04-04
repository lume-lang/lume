use lume_errors::{DiagCtx, MapDiagnostic, Result};

use crate::diff::render_dcx_output;
use crate::{TestPath, TestResult};

pub(crate) fn run_test(path: TestPath) -> Result<TestResult> {
    let mut map_path = path.absolute.0.clone();
    map_path.set_extension("map");

    let file_content = std::fs::read_to_string(&path.absolute.0).map_diagnostic()?;
    let hir_output = build_hir(&path, file_content)?;

    crate::diff::diff_output_of(hir_output, path.relative.0, map_path)
}

fn build_hir(path: &TestPath, content: String) -> Result<String> {
    let dcx = DiagCtx::new();
    let source_file_name = path.relative.file_name().unwrap();

    let pipeline = lume_driver::test_support::workspace(path.root.0.clone())
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
        .with_file(source_file_name, content)
        .pipeline(dcx.handle())?;

    let map = match pipeline.lower_to_hir() {
        Ok(lume_driver::LoweredToHir { gcx, mut maps }) => {
            let root_package_id = gcx.session.dep_graph.root;

            let lume_driver::StageResult::Value(result) = maps.swap_remove(&root_package_id).unwrap() else {
                panic!("unable to find root package");
            };

            let mut hir = result.hir;
            hir.nodes.retain(|id, _node| id.package == root_package_id);

            hir
        }
        Err(err) => {
            dcx.emit(err);

            return Ok(render_dcx_output(&dcx));
        }
    };

    Ok(format!("{map:#?}"))
}
