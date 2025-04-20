use lume_diag_macros::Diagnostic;

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "Error occured parsing CLI arguments",
    code = "CLI0000",
    help = "Please check your CLI arguments"
)]
pub struct InvalidCliError {
    #[source]
    pub inner: getopts::Fail,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "Could not find command: {command:?}",
    code = "CLI0001",
    help = "Please check your CLI arguments"
)]
pub struct UnknownCommandError {
    pub command: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "Could not determine the path to build",
    code = "CLI0002",
    help = "Is the build path correct?"
)]
pub struct CouldNotDetermineBuildPath {
    #[source]
    pub inner: std::io::Error,
}
