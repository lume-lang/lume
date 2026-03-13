use error_snippet_derive::Diagnostic;
use lume_span::Location;

#[derive(Diagnostic, Clone, Debug, PartialEq, Eq)]
#[diagnostic(
    message = "could not infer type",
    code = "LM4139",
    help = "try specifying the type explicitly"
)]
pub struct UnresolvedTypeVariable {
    #[label(source, "could not infer the type for type parameter {type_parameter_name}")]
    pub location: Location,

    pub type_parameter_name: String,
}

#[derive(Diagnostic, Clone, Debug, PartialEq, Eq)]
#[diagnostic(message = "type constraint not satisfied", code = "LM4141")]
pub struct BoundUnsatisfied {
    #[label(source, "type {type_name} does not implement {constraint_name}...")]
    pub source: Location,

    #[label(source, help, "...which is required by the type parameter {param_name}")]
    pub constraint_loc: Location,

    pub param_name: String,
    pub type_name: String,
    pub constraint_name: String,
}

#[derive(Diagnostic, Clone, Debug, PartialEq, Eq)]
#[diagnostic(message = "infinite type", code = "LM4142")]
pub struct InfiniteType {
    #[label(source, "could not resolve type argument for {type_parameter_name}")]
    pub location: Location,

    #[label(source, note, "type parameter defined here")]
    pub type_parameter_span: Location,

    pub type_parameter_name: String,
}
