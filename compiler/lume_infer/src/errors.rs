use std::{ops::Range, sync::Arc};

use error_snippet_derive::Diagnostic;
use lume_hir::{Identifier, Path};
use lume_span::{Location, SourceFile};

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find type {name} in this scope", code = "LM4100")]
pub struct MissingType {
    #[label(source, "is there a missing import for the type {name}?")]
    pub source: Location,

    pub name: Path,
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
#[diagnostic(message = "could not find namespace", code = "LM4104")]
pub struct InvalidNamespace {
    #[label(source, "could not find {name} within namespace {parent:+}")]
    pub source: Location,

    pub name: String,
    pub parent: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find namespace", code = "LM4104")]
pub struct InvalidNamespaceRoot {
    #[label(source, "could not find namespace {name}")]
    pub source: Location,

    pub name: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find type in namespace", code = "LM4105")]
pub struct InvalidTypeInNamespace {
    #[label(source, "could not find type {name} in {namespace}")]
    pub source: Location,

    pub name: Path,
    pub namespace: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "no such field was found", code = "LM4115")]
pub struct MissingField {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("could not find field {field_name} on type {type_name}")]
    pub range: Range<usize>,

    pub type_name: Path,
    pub field_name: Identifier,
}
