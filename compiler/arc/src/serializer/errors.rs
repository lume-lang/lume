use std::{ops::Range, sync::Arc};

use error_snippet_derive::Diagnostic;
use lume_span::SourceFile;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "expected {expected} block", code = "ARC0015")]
pub struct UnexpectedBlockType {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected {expected}, got {actual} instead")]
    pub range: Range<usize>,

    pub expected: String,
    pub actual: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "source property required", code = "ARC0024")]
pub struct SourcePropertyRequired {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("a {name} property is required inside {block} blocks")]
    pub range: Range<usize>,

    pub name: String,
    pub block: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "version property required", code = "ARC0025")]
pub struct VersionPropertyRequired {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("a {name} property is required inside {block} blocks")]
    pub range: Range<usize>,

    pub name: String,
    pub block: String,
}
