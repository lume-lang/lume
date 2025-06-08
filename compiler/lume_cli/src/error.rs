use error_snippet_derive::Diagnostic;

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "Could not determine the path to build",
    code = "CLI0002",
    help = "Is the build path correct?"
)]
pub struct CouldNotDetermineBuildPath {
    #[related]
    pub inner: Vec<error_snippet::Error>,
}
