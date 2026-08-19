extern crate afl;

use afl::fuzz;
use lume_driver::test_support::workspace;
use lume_errors::DiagCtx;

fn main() {
    fuzz!(|data: &[u8]| {
        if let Ok(content) = std::str::from_utf8(data) {
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
                    "#,
                )
                .with_file("src/main.lm", content)
                .pipeline(dcx.clone())
                .unwrap();

            let _ = (|| {
                pipeline
                    .lower_to_hir()?
                    .type_check()?
                    .lower_to_tir()?
                    .lower_to_mir()?
                    .codegen()
            })();
        }
    });
}
