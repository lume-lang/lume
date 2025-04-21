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
