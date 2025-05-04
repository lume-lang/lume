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
#[diagnostic(message = "cannot use {ty} outside of a class", code = "LM3013")]
pub struct SelfOutsideClass {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("type {ty} cannot be used outside of a class, since it has nothing to refer to")]
    pub range: Range<usize>,

    pub ty: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "{ty} must be the first parameter",
    code = "LM3014",
    help = "consider moving the {ty} parameter to the beginning"
)]
pub struct SelfNotFirstParameter {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("instance methods must have {ty} as the first parameter")]
    pub range: Range<usize>,

    pub ty: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Undeclared variable", code = "LM3024")]
pub struct UndeclaredVariable {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("Could not find a variable named '{name}' in the current scope")]
    pub range: Range<usize>,

    pub name: String,
}
