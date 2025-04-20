use std::ops::Range;

use lume_diag::source::NamedSource;
use lume_diag_macros::Diagnostic;

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
#[diagnostic(message = "Cannot use 'Self' outside of a class", code = "LM3013")]
pub struct SelfOutsideClass {
    #[span]
    pub source: NamedSource,

    #[label("Type 'Self' cannot be used outside of a class, since it has nothing to refer to")]
    pub range: Range<usize>,
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
