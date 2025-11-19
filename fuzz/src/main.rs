use std::sync::Arc;

extern crate afl;

use afl::fuzz;
use build_stage::ManifoldDriver;
use lume_errors::DiagCtx;
use lume_span::SourceFile;

fn main() {
    fuzz!(|data: &[u8]| {
        if let Ok(content) = std::str::from_utf8(data) {
            let source_file = SourceFile::internal(content);
            let mut stub_package = build_stage::stub_package_with(|pkg| pkg.add_source(Arc::new(source_file)));
            stub_package.add_std_sources();

            let dcx = DiagCtx::new();
            let manifold_driver = ManifoldDriver::new(stub_package, dcx.clone());

            let _ = manifold_driver.compile();
        }
    });
}
