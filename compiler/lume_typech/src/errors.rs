use std::{ops::Range, sync::Arc};

use lume_diag_macros::Diagnostic;
use lume_span::SourceFile;
use lume_types::SymbolName;

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
