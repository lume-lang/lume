use std::ops::Range;

use diag::source::NamedSource;
use diag_macros::Diagnostic;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Undeclared function", code = "LM3012")]
pub struct MissingFunction {
    #[span]
    pub source: NamedSource,

    #[label("Could not find a function named '{name}' in the current scope")]
    pub range: Range<usize>,

    pub name: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Undeclared type", code = "LM3013")]
pub struct MissingType {
    #[span]
    pub source: NamedSource,

    #[label("Could not find a type named '{name}' in the current scope")]
    pub range: Range<usize>,

    pub name: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Undeclared variable", code = "LM3014")]
pub struct UndeclaredVariable {
    #[span]
    pub source: NamedSource,

    #[label("Could not find a variable named '{name}' in the current scope")]
    pub range: Range<usize>,

    pub name: String,
}
