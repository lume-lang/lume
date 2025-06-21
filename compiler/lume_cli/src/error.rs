use error_snippet_derive::Diagnostic;

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "Could not determine the path to build",
    code = "CLI0002",
    help = "Is the build path correct?"
)]
pub struct CouldNotDetermineBuildPath {
    #[cause]
    pub inner: error_snippet::Error,
}
