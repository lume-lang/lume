use std::{ops::Range, sync::Arc};

use lume_diag_macros::Diagnostic;
use lume_span::SourceFile;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Undeclared function", code = "LM3012")]
pub struct MissingFunction {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("Could not find a function named '{name}' in the current scope")]
    pub range: Range<usize>,

    pub name: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Cannot use 'Self' outside of a class", code = "LM3013")]
pub struct SelfOutsideClass {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("Type 'Self' cannot be used outside of a class, since it has nothing to refer to")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Undeclared variable", code = "LM3014")]
pub struct UndeclaredVariable {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("Could not find a variable named '{name}' in the current scope")]
    pub range: Range<usize>,

    pub name: String,
}
