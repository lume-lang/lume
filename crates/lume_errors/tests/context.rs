use lume_errors::{DiagCtx, Severity, diagnostic};

#[test]
fn no_panic_on_warning_raised() {
    let dcx = DiagCtx::new();
    dcx.panic_on_error();

    dcx.emit(diagnostic!("warning").with_severity(Severity::Warning));
}

#[test]
#[should_panic = "IM AN ERROR"]
fn panic_on_error_raised() {
    let dcx = DiagCtx::new();
    dcx.panic_on_error();

    dcx.emit(diagnostic!("IM AN ERROR").with_severity(Severity::Error));
}
