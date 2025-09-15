#![no_main]

use std::sync::Arc;

use build_stage::ManifoldDriver;
use libfuzzer_sys::fuzz_target;
use lume_errors::DiagCtx;
use lume_span::SourceFile;

fuzz_target!(|data: &[u8]| {
    if let Ok(content) = std::str::from_utf8(data) {
        let source_file = SourceFile::internal(content);
        let mut stub_package = build_stage::stub_package_with(|pkg| pkg.add_source(Arc::new(source_file)));
        stub_package.add_std_sources();

        let dcx = DiagCtx::new();
        let manifold_driver = ManifoldDriver::new(stub_package, dcx.clone());

        let _ = manifold_driver.build_mir();
    }
});
