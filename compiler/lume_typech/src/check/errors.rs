use std::{ops::Range, sync::Arc};

use error_snippet_derive::Diagnostic;
use lume_span::SourceFile;
use lume_types::NamedTypeRef;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "mismatched types", code = "LM4001")]
pub struct MismatchedTypes {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected type {expected}, but found type {found}...")]
    pub expect_range: Range<usize>,

    #[label("...because of type defined here")]
    pub reason_range: Range<usize>,

    pub expected: NamedTypeRef,
    pub found: NamedTypeRef,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "unavailable cast",
    code = "LM4004",
    help = "to allow casting, add the `Cast<{to}>` trait to {from}"
)]
pub struct UnavailableCast {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("cannot cast {from} to type {to}")]
    pub range: Range<usize>,

    pub from: NamedTypeRef,
    pub to: NamedTypeRef,
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
