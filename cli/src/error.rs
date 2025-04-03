use compiler::error::CompilerError;
use miette::Diagnostic;

#[derive(thiserror::Error, Diagnostic, Debug)]
pub enum Error {
    #[diagnostic(transparent)]
    #[error(transparent)]
    CliError(#[from] CliError),

    #[diagnostic(transparent)]
    #[error(transparent)]
    CompilerError(#[from] CompilerError),
}

#[derive(thiserror::Error, Diagnostic, Debug)]
pub enum CliError {
    #[diagnostic(code(CLI0000), help("Please check your CLI arguments"))]
    #[error("Error occured parsing CLI arguments")]
    ParsingError(#[from] getopts::Fail),

    #[diagnostic(code(CLI0001), help("Please check your CLI arguments"))]
    #[error("Could not find command: {0:?}")]
    UnknownCommand(String),

    #[diagnostic(code(CLI0002), help("Is the build path correct?"))]
    #[error("Could not determine the path to build")]
    CouldNotDetermineBuildPath(#[from] std::io::Error),
}

impl Error {
    /// Prints the report associated with the error.
    pub fn report(self) {
        match self {
            Error::CliError(err) => {
                println!("{:?}", err);
            }
            err => {
                let report: miette::Report = err.into();

                println!("{:?}", report);
            }
        };

        std::process::exit(1);
    }
}
