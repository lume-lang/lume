use std::{ops::Range, sync::Arc};

use error_snippet_derive::Diagnostic;
use lume_hir::{SignatureOwned, Visibility};
use lume_span::{Location, SourceFile};
use lume_types::NamedTypeRef;

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "mismatched types",
    code = "LM4001",
    help = "expected type {expected}\nfound type {found}"
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
