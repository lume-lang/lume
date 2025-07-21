use std::{ops::Range, sync::Arc};

use error_snippet_derive::Diagnostic;
use lume_span::SourceFile;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "unexpected character", code = "LM1020", help = "check your syntax")]
pub struct UnexpectedCharacter {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("unexpected character {char}")]
    pub range: Range<usize>,

    pub char: char,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "expected ending quote",
    code = "LM1021",
    help = "did you forget to end your string?"
)]
pub struct MissingEndingQuote {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("string literal was started, but has no matching end-quote")]
    pub range: Range<usize>,
}
