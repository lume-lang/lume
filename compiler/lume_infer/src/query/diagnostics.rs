use std::ops::Range;
use std::sync::Arc;

use error_snippet_derive::Diagnostic;
use lume_hir::{Identifier, Path, PathSegment};
use lume_span::{Location, NodeId, SourceFile};
use lume_types::{NamedTypeRef, TypeKind};

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
#[diagnostic(message = "node cannot hold type parameters: {id}", code = "LM4215")]
pub struct CannotHoldTypeParams {
    pub id: NodeId,
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

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "cannot assign expression", code = "LM4155")]
pub struct CannotAssignExpression {
    #[label(source, "invalid expression on left-hand side of assignment")]
    pub source: Location,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find node {id:?} in context")]
pub struct NodeNotFound {
    pub id: NodeId,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find item")]
pub struct TypeNameNotFound {
    pub name: Path,

    #[label(source, "could not find {name:+} in context")]
    pub location: Location,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "expected path {path:+} to have a parent")]
pub struct PathWithoutParent {
    pub path: Path,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "expected type to be kind of {expected:?}, found {found:?}")]
pub struct UnexpectedTypeKind {
    pub expected: TypeKind,
    pub found: TypeKind,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find variant {name} on type {type_id:?}")]
pub struct VariantNameNotFound {
    pub type_id: NodeId,
    pub name: String,
}
