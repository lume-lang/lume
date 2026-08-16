use lume_errors_derive::Diagnostic;

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "could not determine the path to build",
    code = "CLI0002",
    help = "is the build path correct?"
)]
pub struct CouldNotDetermineBuildPath {
    #[cause]
    pub inner: lume_errors::Error,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "could not determine the current directory",
    code = "CLI0003",
    help = "is the current directory readable?"
)]
pub struct CouldNotDetermineCurrentDir {
    #[cause]
    pub inner: lume_errors::Error,
}
