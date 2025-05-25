use std::{ops::Range, sync::Arc};

use error_snippet_derive::Diagnostic;
use lume_hir::{Identifier, SymbolName};
use lume_span::SourceFile;

use super::MethodDisqualificationReason;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "no such method was found", code = "LM4113")]
pub struct MissingMethod {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("could not find method {method_name} on type {type_name}")]
    pub range: Range<usize>,

    pub type_name: SymbolName,
    pub method_name: Identifier,

    #[related]
    pub suggestions: Vec<error_snippet::Error>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "similar method found",
    code = "LM4118",
    severity = Help,
    help = "incompatible because of {reason}")]
pub struct SuggestedMethod {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("found similar method {method_name} on type {type_name}")]
    pub range: Range<usize>,

    pub method_name: Identifier,
    pub type_name: SymbolName,
    pub reason: MethodDisqualificationReason,
}
