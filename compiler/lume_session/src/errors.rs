use error_snippet_derive::Diagnostic;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "failed to read Arcfile", code = "ARC0103")]
pub struct ArcfileGlobError {
    #[related]
    pub inner: error_snippet::Error,
}
