use error_snippet_derive::Diagnostic;
use lume_hir::{Identifier, PathSegment, SymbolName};
use lume_span::Location;

use crate::query::CallableCheckError;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "no such method was found", code = "LM4113")]
pub(crate) struct MissingMethod {
    #[label(source, "could not find method {method_name} on type {type_name}")]
    pub source: Location,

    pub type_name: SymbolName,
    pub method_name: Identifier,

    #[related(collection)]
    pub suggestions: Vec<error_snippet::Error>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "similar method found",
    code = "LM4118",
    severity = Help,
    help = "incompatible because of {reason}")]
pub(crate) struct SuggestedMethod {
    #[label(source, "found similar method {method_name} on type {type_name}")]
    pub source: Location,

    pub method_name: PathSegment,
    pub type_name: SymbolName,
    pub reason: CallableCheckError,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "no such function was found", code = "LM4113")]
pub struct MissingFunction {
    #[label(source, "could not find function {function_name}")]
    pub source: Location,

    pub function_name: Identifier,

    #[related(collection)]
    pub suggestions: Vec<error_snippet::Error>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "similar function found",
    code = "LM4118",
    severity = Help,
    help = "incompatible because of {reason}")]
pub(crate) struct SuggestedFunction {
    #[label(source, "found similar function {function_name}")]
    pub source: Location,

    pub function_name: PathSegment,
    pub reason: CallableCheckError,
}
