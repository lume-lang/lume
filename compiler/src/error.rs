use crate::parser::ParsingError;
use arc::errors::ArcError;

#[derive(thiserror::Error, miette::Diagnostic, Debug)]
pub enum CompilerError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    ArcError(#[from] ArcError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    Parsing(#[from] ParsingError),
}

#[derive(Debug, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Info,
    Warning,
    Error,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub severity: DiagnosticSeverity,

    pub message: String,
}

impl Diagnostic {
    pub fn new(message: String) -> Self {
        Diagnostic {
            severity: DiagnosticSeverity::Error,
            message,
        }
    }
}
