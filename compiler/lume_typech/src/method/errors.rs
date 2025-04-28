use std::ops::Range;

use lume_diag::source::NamedSource;
use lume_diag_macros::Diagnostic;
use lume_types::{Identifier, SymbolName};

use super::MethodDisqualificationReason;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "no such method was found", code = "LM4113")]
pub struct MissingMethod {
    #[span]
    pub source: NamedSource,

    #[label("could not find method {method_name} on type {type_name}")]
    pub range: Range<usize>,

    pub type_name: SymbolName,
    pub method_name: Identifier,

    #[related]
    pub suggestions: Vec<SuggestedMethod>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "similar method found",
    code = "LM4118",
    severity = Help,
    help = "incompatible because: {reason}")]
pub struct SuggestedMethod {
    #[span]
    pub source: NamedSource,

    #[label("found similar method {method_name} on type {type_name}")]
    pub range: Range<usize>,

    pub method_name: Identifier,
    pub type_name: SymbolName,
    pub reason: MethodDisqualificationReason,
}
