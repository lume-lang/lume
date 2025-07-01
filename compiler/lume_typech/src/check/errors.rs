use std::{ops::Range, sync::Arc};

use error_snippet_derive::Diagnostic;
use lume_hir::{SignatureOwned, Visibility};
use lume_span::{Location, SourceFile};
use lume_types::NamedTypeRef;

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "mismatched types",
    code = "LM4001",
    help = "expected type {expected}\n   found type {found}"
)]
pub struct MismatchedTypes {
    #[label(source, "expected type {expected}, but found type {found}...")]
    pub found_loc: Location,

    #[label(source, "...because of type defined here")]
    pub reason_loc: Location,

    pub expected: NamedTypeRef,
    pub found: NamedTypeRef,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "trait is not implemented", code = "LM4002")]
pub struct TraitNotImplemented {
    #[label(source, "the trait {trait_name} is not implemented for the type {type_name}")]
    pub location: Location,

    pub trait_name: NamedTypeRef,
    pub type_name: NamedTypeRef,
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
#[diagnostic(message = "missing method in implementation", code = "LM4161")]
pub struct TraitImplMissingMethod {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("trait implementation does not implement method {name}")]
    pub range: Range<usize>,

    pub name: lume_hir::Identifier,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "implemented trait does not match trait definition", code = "LM4165")]
pub struct TraitImplTypeParameterCountMismatch {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("trait has {found} type parameters, but expected {expected} from trait definition")]
    pub range: Range<usize>,

    pub expected: usize,
    pub found: usize,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "implemented method does not match definition visibility", code = "LM4168")]
pub struct TraitMethodVisibilityMismatch {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected method to be {expected}, found {found}")]
    pub range: Range<usize>,

    pub expected: Visibility,
    pub found: Visibility,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "implemented method does not match definition signature", code = "LM4169")]
pub struct TraitMethodSignatureMismatch {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected signature {expected}, found {found}")]
    pub range: Range<usize>,

    pub expected: SignatureOwned,
    pub found: SignatureOwned,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "binary operation on non-matching types", code = "LM4372")]
pub(crate) struct NonMatchingBinaryOp {
    #[label(source, "cannot perform binary operation between non-matching types")]
    pub source: lume_span::Location,

    #[label(source, "found type {lhs_ty} on left-hand side...")]
    pub lhs: lume_span::Location,

    #[label(source, "...and found type {rhs_ty} on right-hand side")]
    pub rhs: lume_span::Location,

    pub lhs_ty: String,
    pub rhs_ty: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "boolean operation on non-boolean type", code = "LM4373")]
pub(crate) struct BooleanOperationOnNonBoolean {
    #[label(source, "expected boolean operation on {expected}, found {found}")]
    pub source: lume_span::Location,

    pub expected: String,
    pub found: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "mismatched types in branch",
    code = "LM4384",
    help = "expected type {expected}\n   found type {found}",
    help = "all branches must return the same type"
)]
pub struct MismatchedTypesBranches {
    #[label(source, "expected type {expected}, but found type {found}...")]
    pub found_loc: Location,

    #[label(source, "...because of type defined here")]
    pub reason_loc: Location,

    pub expected: NamedTypeRef,
    pub found: NamedTypeRef,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "missing structure field", code = "LM4385")]
pub struct MissingPropertyField {
    #[label(source, "constructor is missing field {field}")]
    pub source: Location,

    pub field: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "unknown structure field", code = "LM4386")]
pub struct UnknownPropertyField {
    #[label(source, "type {ty} has no field {field}")]
    pub source: Location,

    pub ty: NamedTypeRef,
    pub field: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "missing return in branch",
    code = "LM4387",
    help = "conditionals without an `else` branch can return `void`\nwhich is incompatible with {expected}"
)]
pub struct MissingReturnBranch {
    #[label(source, "not all branches return a value")]
    pub source: Location,

    pub expected: NamedTypeRef,
}
