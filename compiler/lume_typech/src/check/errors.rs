use std::{ops::Range, sync::Arc};

use lume_diag_macros::Diagnostic;
use lume_span::SourceFile;

use crate::TypeRef;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "mismatched types", code = "LM4001")]
pub struct MismatchedTypes {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected type {expected:?}, but found type {found:?}")]
    pub range: Range<usize>,

    pub expected: TypeRef,
    pub found: TypeRef,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "type unavailable in Lume",
    code = "LM4101",
    help = "you can use the {suggestion} type, which likely is what you meant."
)]
pub struct UnavailableScalarType {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("the type {found} does not exist in Lume.")]
    pub range: Range<usize>,

    pub found: String,
    pub suggestion: &'static str,
}
