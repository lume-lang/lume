use error_snippet_derive::Diagnostic;
use lume_hir::Path;
use lume_span::Location;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find callable", code = "LM4108")]
pub(crate) struct CallableNotFound {
    #[label(source, "could not find any callable with the name of {name:+}")]
    pub source: Location,

    pub name: Path,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "type argument mismatch", code = "LM4119")]
pub(crate) struct TypeArgumentCountMismatch {
    #[label(
        source,
        "expected type {type_name:+} to have {expected} type arguments, but got {actual}"
    )]
    pub source: Location,

    pub type_name: Path,
    pub expected: usize,
    pub actual: usize,
}
