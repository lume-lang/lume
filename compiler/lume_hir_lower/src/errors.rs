use std::{ops::Range, sync::Arc};

use error_snippet_derive::Diagnostic;
use lume_span::SourceFile;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "invalid namespace path", code = "LM3005")]
pub struct InvalidNamespacePath {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("namespace path is not valid: {path:?}")]
    pub range: Range<usize>,

    pub path: Box<dyn std::fmt::Debug + Send + Sync>,
}

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
#[diagnostic(
    message = "{ty} cannot be used in functions",
    code = "LM3015",
    help = "since functions have no instance, remove the {ty} parameter"
)]
pub struct SelfOutsideObjectContext {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("{ty} cannot be used in functions; they must only be used in traits and classes")]
    pub range: Range<usize>,

    pub ty: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "vararg parameter must be last",
    code = "LM3016",
    help = "consider moving the vararg parameter to the end"
)]
pub struct VarargNotLastParameter {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("vararg parameter must be the last parameter")]
    pub range: Range<usize>,
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
