use error_snippet_derive::Diagnostic;

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "Error occured parsing CLI arguments",
    code = "CLI0000",
    help = "Please check your CLI arguments"
)]
pub struct InvalidCliError {
    #[related]
    pub inner: Vec<error_snippet::Error>,
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
    #[related]
    pub inner: Vec<error_snippet::Error>,
}
