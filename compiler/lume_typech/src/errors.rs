use std::{ops::Range, sync::Arc};

use error_snippet_derive::Diagnostic;
use lume_hir::{Identifier, SymbolName};
use lume_span::SourceFile;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Could not find type {name} in this scope", code = "LM4100")]
pub struct MissingType {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("Is there a missing import for the type {name}?")]
    pub range: Range<usize>,

    pub name: SymbolName,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find symbol {name:?}", code = "LM4112")]
pub struct MissingSymbol {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("no symbol with name {name:?} was found")]
    pub range: Range<usize>,

    pub name: SymbolName,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "attempted to invoke type", code = "LM4113")]
pub struct AttemptedTypeInvocation {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("attempted to invoke type {name}")]
    pub range: Range<usize>,

    pub name: SymbolName,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "no such property was found", code = "LM4115")]
pub struct MissingProperty {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("could not find property {property_name} on type {type_name}")]
    pub range: Range<usize>,

    pub type_name: SymbolName,
    pub property_name: Identifier,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "type argument mismatch", code = "LM4164")]
pub struct TypeArgumentMismatch {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected {expected} type arguments, found {found}")]
    pub range: Range<usize>,

    pub expected: usize,
    pub found: usize,
}
