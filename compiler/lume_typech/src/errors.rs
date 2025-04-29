use std::{ops::Range, sync::Arc};

use lume_diag_macros::Diagnostic;
use lume_span::SourceFile;
use lume_types::{Identifier, SymbolName};

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
#[diagnostic(message = "could not find function {name}", code = "LM4112")]
pub struct MissingFunction {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("no such function was found")]
    pub range: Range<usize>,

    pub name: SymbolName,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "cannot instantiate non-concrete type {name}", code = "LM4114")]
pub struct AbstractTypeInstantiate {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("`new` can only instantiate class-types, found {kind}")]
    pub range: Range<usize>,

    pub name: SymbolName,
    pub kind: &'static str,
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
