use error_snippet_derive::Diagnostic;
use lume_hir::Path;
use lume_span::Location;
use lume_types::NamedTypeRef;

#[derive(Diagnostic, Clone, Debug, PartialEq, Eq)]
#[diagnostic(
    message = "mismatched types",
    code = "LM4001",
    help = "expected type {expected}\n   found type {found}"
)]
pub struct MismatchedTypes {
    #[label(source, "expected type {expected}, but found type {found}...")]
    pub found_loc: Location,

    #[label(source, note, "...because of type defined here")]
    pub reason_loc: Location,

    pub expected: NamedTypeRef,
    pub found: NamedTypeRef,
}

#[derive(Diagnostic, Clone, Debug, PartialEq, Eq)]
#[diagnostic(
    message = "mismatched types",
    code = "LM4001",
    help = "expected type {expected}\n   found type {found}"
)]
pub struct MismatchedTypesBoolean {
    #[label(source, "expected type {expected}, but found type {found}")]
    pub source: Location,

    pub expected: NamedTypeRef,
    pub found: NamedTypeRef,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "unavailable cast",
    code = "LM4004",
    help = "to allow casting, add the `Cast<{to}>` trait to {from}"
)]
pub struct UnavailableCast {
    #[label(source, "cannot cast {from} to type {to}")]
    pub source: Location,

    pub from: NamedTypeRef,
    pub to: NamedTypeRef,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "missing method in implementation", code = "LM4161")]
pub struct TraitImplMissingMethod {
    #[label(source, "trait implementation does not implement method {name}")]
    pub source: Location,

    pub name: lume_hir::Identifier,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "extraneous method in implementation", code = "LM4162")]
pub struct TraitImplExtraneousMethod {
    #[label(source, "implemented method {name} does not exist in trait definition")]
    pub source: Location,

    pub name: lume_hir::Identifier,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "implemented trait does not match trait definition", code = "LM4165")]
pub struct TraitImplTypeParameterCountMismatch {
    #[label(
        source,
        "trait has {found} type parameters, but expected {expected} from trait definition"
    )]
    pub source: Location,

    pub expected: usize,
    pub found: usize,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "implemented method does not match definition signature", code = "LM4169")]
pub struct TraitMethodSignatureMismatch {
    #[label(source, "expected signature {expected}, found {found}")]
    pub source: Location,

    pub expected: String,
    pub found: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "mismatching types in assignment", code = "LM4376")]
pub(crate) struct NonMatchingAssignment {
    #[label(source, "cannot assign value of type {value_ty} to {target_ty}")]
    pub source: lume_span::Location,

    #[label(source, note, "found type {target_ty} on right-hand side...")]
    pub target_loc: lume_span::Location,

    #[label(source, note, "...and found type {value_ty} on left-hand side")]
    pub value_loc: lume_span::Location,

    pub target_ty: String,
    pub value_ty: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "trait implementation cannot be inferred",
    code = "LM4379",
    help = "specify a specific implementation of the trait"
)]
pub struct DispatchCannotBeInferred {
    #[label(source, "cannot infer trait implementation from static call")]
    pub source: Location,
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

    #[label(source, note, "...because of type defined here")]
    pub reason_loc: Location,

    pub expected: NamedTypeRef,
    pub found: NamedTypeRef,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "mismatched types in branch",
    code = "LM4384",
    help = "expected type {expected}\n   found type {found}",
    help = "all branches must return the same type"
)]
pub struct MismatchedTypesBranchesCondition {
    #[label(source, "expected type {expected}, but found type {found}...")]
    pub found_loc: Location,

    pub expected: NamedTypeRef,
    pub found: NamedTypeRef,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "missing structure field", code = "LM4385")]
pub struct MissingField {
    #[label(source, "constructor is missing field {field}")]
    pub source: Location,

    pub field: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "unknown structure field", code = "LM4386")]
pub struct UnknownField {
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

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "type {type_name:+} is inaccessible", code = "LM4392")]
pub struct InaccessibleType {
    #[label(source, "type {type_name:+} is inaccessible, because of it's visibility")]
    pub source: Location,

    #[label(source, help, "type defined here")]
    pub type_def: Location,

    pub type_name: Path,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "method {method_name:+} is inaccessible", code = "LM4393")]
pub struct InaccessibleMethod {
    #[label(source, "method {method_name:+} is inaccessible, because of it's visibility")]
    pub source: Location,

    #[label(source, help, "method defined here")]
    pub method_def: Location,

    pub method_name: Path,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "function {func_name:+} is inaccessible", code = "LM4394")]
pub struct InaccessibleFunction {
    #[label(source, "function {func_name:+} is inaccessible, because of it's visibility")]
    pub source: Location,

    #[label(source, help, "function defined here")]
    pub func_def: Location,

    pub func_name: Path,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "field {field_name:+} is inaccessible", code = "LM4395")]
pub struct InaccessibleField {
    #[label(source, "field {field_name:+} is inaccessible, because of it's visibility")]
    pub source: Location,

    #[label(source, help, "field defined here")]
    pub field_def: Location,

    pub field_name: String,
}
