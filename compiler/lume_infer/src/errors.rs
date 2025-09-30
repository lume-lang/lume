use std::ops::Range;
use std::sync::Arc;

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

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "attempted instance call on static method", code = "LM4124")]
pub(crate) struct InstanceCallOnStaticMethod {
    #[label(source, "cannot call static method {method_name} on an instance")]
    pub source: Location,

    pub method_name: Path,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "could not infer type argument",
    code = "LM4139",
    help = "try specifying the type arguments explicitly"
)]
pub struct TypeArgumentInferenceFailed {
    #[label(source, "could not infer type argument {type_param_name}")]
    pub source: Location,

    pub type_param_name: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "could not infer type argument on callable",
    code = "LM4145",
    help = "try specifying the type arguments explicitly"
)]
pub struct TypeArgumentInferenceFailedCallable {
    #[label(
        source,
        "could not infer type argument {type_param_name} on callable {callable_name}"
    )]
    pub source: Location,

    pub type_param_name: String,
    pub callable_name: String,
}
