use error_snippet_derive::Diagnostic;

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "could not determine the path to build",
    code = "CLI0002",
    help = "is the build path correct?"
)]
pub struct CouldNotDetermineBuildPath {
    #[cause]
    pub inner: error_snippet::Error,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "could not determine the current directory",
    code = "CLI0003",
    help = "is the current directory readable?"
)]
pub struct CouldNotDetermineCurrentDir {
    #[cause]
    pub inner: error_snippet::Error,
}
