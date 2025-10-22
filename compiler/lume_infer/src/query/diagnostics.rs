use std::ops::Range;
use std::sync::Arc;

use error_snippet_derive::Diagnostic;
use lume_hir::{Identifier, Path, PathSegment};
use lume_span::{Location, SourceFile};
use lume_types::NamedTypeRef;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find method", code = "LM4113")]
pub(crate) struct MissingMethod {
    #[label(source, "could not find method {method_name} on type {type_name}")]
    pub source: Location,

    pub type_name: NamedTypeRef,
    pub method_name: Identifier,

    #[related(collection)]
    pub suggestions: Vec<error_snippet::Error>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "did you mean to call {method_name}?", severity = Help)]
pub struct SuggestedMethod {
    #[label(source, "found method with similar name")]
    pub source: Location,

    pub method_name: PathSegment,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find function", code = "LM4113")]
pub struct MissingFunction {
    #[label(source, "could not find function {function_name}")]
    pub source: Location,

    pub function_name: Identifier,

    #[related(collection)]
    pub suggestions: Vec<error_snippet::Error>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "did you mean to call {function_name}?", severity = Help)]
pub struct SuggestedFunction {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("found function with similar name")]
    pub range: Range<usize>,

    pub function_name: PathSegment,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "argument count mismatch", code = "LM4116")]
pub(crate) struct ArgumentCountMismatch {
    #[label(source, "expected {expected} arguments, but got {actual}")]
    pub source: Location,

    pub expected: usize,
    pub actual: usize,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find variant", code = "LM4134")]
pub struct MissingVariant {
    #[label(source, "could not find variant {name} in type {type_name:+}")]
    pub source: Location,

    pub name: Path,
    pub type_name: Path,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find returning ancestor", code = "LM4216")]
pub struct NoReturningAncestor {
    #[label(source, "expected returning ancestor, found None")]
    pub source: Location,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find parent `impl` block", code = "LM4218")]
pub struct NoParentImpl {
    #[label(source, "expected `impl` block ancestor, found None")]
    pub source: Location,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find parent `struct` block", code = "LM4219")]
pub struct NoParentStruct {
    #[label(source, "expected `struct` block ancestor, found None")]
    pub source: Location,
}
