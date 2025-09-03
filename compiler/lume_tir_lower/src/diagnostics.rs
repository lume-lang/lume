use error_snippet_derive::Diagnostic;
use lume_span::Location;

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "non-exhaustive switch expression",
    code = "LM4613",
    help = "add a fallback case in the switch"
)]
pub(crate) struct MissingWildcardCase {
    #[label(source, "switch expression is non-exhaustive")]
    pub source: Location,
}
