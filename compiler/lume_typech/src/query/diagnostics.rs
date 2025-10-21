use error_snippet_derive::Diagnostic;
use lume_hir::Path;
use lume_span::Location;
use lume_types::NamedTypeRef;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "argument count mismatch", code = "LM4116")]
pub(crate) struct ArgumentCountMismatch {
    #[label(source, "expected {expected} arguments, but got {actual}")]
    pub source: Location,

    pub expected: usize,
    pub actual: usize,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "argument count mismatch", code = "LM4116")]
pub(crate) struct VariableArgumentCountMismatch {
    #[label(source, "expected at least {expected} arguments, but got {actual}")]
    pub source: Location,

    pub expected: usize,
    pub actual: usize,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "type argument mismatch", code = "LM4119")]
pub(crate) struct TypeArgumentCountMismatch {
    #[label(source, "expected {expected} type arguments, but got {actual}")]
    pub source: Location,

    pub expected: usize,
    pub actual: usize,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "type constraint not satisfied", code = "LM4120")]
pub(crate) struct TypeParameterConstraintUnsatisfied {
    #[label(source, "type {type_name} does not implement {constraint_name}...")]
    pub source: Location,

    #[label(source, "...which is required by the type parameter {param_name}")]
    pub constraint_loc: Location,

    pub param_name: String,
    pub type_name: NamedTypeRef,
    pub constraint_name: NamedTypeRef,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "attempted instance call on static method", code = "LM4124")]
pub(crate) struct InstanceCallOnStaticMethod {
    #[label(source, "cannot call static method {method_name} on an instance")]
    pub source: Location,

    pub method_name: Path,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "node cannot hold visibility modifiers", code = "LM4126")]
pub struct CannotHoldVisibility {
    #[label(source, "expected node with visibility modifier")]
    pub source: Location,
}
