use std::{ops::Range, sync::Arc};

use error_snippet_derive::Diagnostic;
use lume_hir::{Identifier, PathSegment, SymbolName};
use lume_span::SourceFile;

use crate::query::lookup::CallableCheckError;

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

    pub method_name: PathSegment,
    pub type_name: SymbolName,
    pub reason: CallableCheckError,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "no such function was found", code = "LM4113")]
pub struct MissingFunction {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("could not find function {function_name}")]
    pub range: Range<usize>,

    pub function_name: Identifier,

    #[related]
    pub suggestions: Vec<error_snippet::Error>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "similar function found",
    code = "LM4118",
    severity = Help,
    help = "incompatible because of {reason}")]
pub struct SuggestedFunction {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("found similar function {function_name}")]
    pub range: Range<usize>,

    pub function_name: PathSegment,
    pub reason: CallableCheckError,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find returning ancestor", code = "LM4216")]
pub struct NoReturningAncestor {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected returning ancestor, found None")]
    pub range: Range<usize>,
}
