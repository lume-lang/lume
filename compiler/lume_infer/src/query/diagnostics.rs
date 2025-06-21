use std::{ops::Range, sync::Arc};

use error_snippet_derive::Diagnostic;
use lume_hir::{Identifier, PathSegment};
use lume_span::{Location, SourceFile};
use lume_types::NamedTypeRef;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "no such method was found", code = "LM4113")]
pub struct MissingMethod {
    #[label(source, "could not find method {method_name} on type {type_name}")]
    pub source: Location,

    pub type_name: NamedTypeRef,
    pub method_name: Identifier,

    #[related(collection)]
    pub suggestions: Vec<error_snippet::Error>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "similar method found",
    code = "LM4118",
    severity = Help)]
pub struct SuggestedMethod {
    #[label(source, "found similar method {method_name} on type {type_name}")]
    pub source: Location,

    pub method_name: PathSegment,
    pub type_name: NamedTypeRef,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "no such function was found", code = "LM4113")]
pub struct MissingFunction {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("could not find function {function_name}")]
    pub range: Range<usize>,

    pub function_name: Identifier,

    #[related(collection)]
    pub suggestions: Vec<error_snippet::Error>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "similar function found",
    code = "LM4118",
    severity = Help)]
pub struct SuggestedFunction {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("found similar function {function_name}")]
    pub range: Range<usize>,

    pub function_name: PathSegment,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find returning ancestor", code = "LM4216")]
pub struct NoReturningAncestor {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected returning ancestor, found None")]
    pub range: Range<usize>,
}
