use std::ops::Range;

use lume_diag::source::NamedSource;
use lume_diag_macros::Diagnostic;
use lume_types::SymbolName;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Could not find type {name} in this scope", code = "LM4100")]
pub struct MissingType {
    #[span]
    pub source: NamedSource,

    #[label("Is there a missing import for the type {name}?")]
    pub range: Range<usize>,

    pub name: SymbolName,
}
