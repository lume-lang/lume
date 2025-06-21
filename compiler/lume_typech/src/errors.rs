use error_snippet_derive::Diagnostic;
use lume_hir::{Identifier, SymbolName};
use lume_span::Location;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Could not find type {name} in this scope", code = "LM4100")]
pub struct MissingType {
    #[label(source, "is there a missing import for the type {name}?")]
    pub source: Location,

    pub name: SymbolName,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find symbol {name:?}", code = "LM4112")]
pub struct MissingSymbol {
    #[label(source, "no symbol with name {name:?} was found")]
    pub source: Location,

    pub name: SymbolName,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "attempted to invoke type", code = "LM4113")]
pub struct AttemptedTypeInvocation {
    #[label(source, "attempted to invoke type {name}")]
    pub source: Location,

    pub name: SymbolName,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "no such property was found", code = "LM4115")]
pub struct MissingProperty {
    #[label(source, "could not find property {property_name} on type {type_name}")]
    pub source: Location,

    pub type_name: SymbolName,
    pub property_name: Identifier,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "type argument mismatch", code = "LM4164")]
pub struct TypeArgumentMismatch {
    #[label(source, "expected {expected} type arguments, found {found}")]
    pub source: Location,

    pub expected: usize,
    pub found: usize,
}
