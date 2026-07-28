use lume_driver::StageResult;
use lume_driver::pipeline::LoweredToHir;
use lume_driver::test_support::workspace;
use lume_errors::DiagCtx;
use lume_hir::Map;

pub fn fixture_as_hir(source: &str) -> Map {
    let dcx = DiagCtx::new();
    let pipeline = workspace(std::env::current_dir().unwrap())
        .with_config(|config| config.dry_run = true)
        .with_option(|opts| opts.enable_incremental = false)
        .with_file(
            "Arcfile",
            r#"
                [package]
                name = "foo"
                version = "1.0.0"
                lume_version = "^0"

                no_std = true
            "#,
        )
        .with_file("src/main.lm", source)
        .pipeline(dcx.handle())
        .unwrap();

    let LoweredToHir { gcx, mut maps } = pipeline.lower_to_hir().unwrap();
    let root_package_id = gcx.session.dep_graph.root;

    let StageResult::Value(tcx) = maps.swap_remove(&root_package_id).unwrap() else {
        unreachable!();
    };

    tcx.hir
}
