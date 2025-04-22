use std::ops::Range;

use lume_diag::source::NamedSource;
use lume_diag_macros::Diagnostic;

use crate::TypeRef;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "mismatched types", code = "LM4001")]
pub struct MismatchedTypes {
    #[span]
    pub source: NamedSource,

    #[label("Expected type '{expected:?}', but found type '{found:?}'")]
    pub range: Range<usize>,

    pub expected: TypeRef,
    pub found: TypeRef,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "Type unavailable in Lume",
    code = "LM4101",
    help = "You can use the `{suggestion}` type, which likely is what you meant."
)]
pub struct UnavailableScalarType {
    #[span]
    pub source: NamedSource,

    #[label("The type `{found}` does not exist in Lume.")]
    pub range: Range<usize>,

    pub found: String,
    pub suggestion: &'static str,
}
