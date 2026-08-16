use lume_errors_derive::Diagnostic;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "failed to read Arcfile", code = "ARC0103")]
pub struct ArcfileGlobError {
    #[related]
    pub inner: lume_errors::Error,
}
