use std::{ops::Range, sync::Arc};

use error_snippet_derive::Diagnostic;
use lume_span::SourceFile;

use super::lexer::TokenKind;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "unexpected token", code = "ARC0001")]
pub struct UnexpectedToken {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected {expected}, got {actual} instead")]
    pub range: Range<usize>,

    pub expected: TokenKind,
    pub actual: TokenKind,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "expected identifier", code = "ARC0003")]
pub struct ExpectedIdentifier {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected identifier, found {actual} instead")]
    pub range: Range<usize>,

    pub actual: TokenKind,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "expected value", code = "ARC0004")]
pub struct ExpectedValue {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected value or block, found {actual} instead")]
    pub range: Range<usize>,

    pub actual: TokenKind,
}
