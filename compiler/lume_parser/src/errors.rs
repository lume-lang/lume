use std::{ops::Range, sync::Arc};

use error_snippet_derive::Diagnostic;
use lume_lexer::TokenKind;
use lume_span::SourceFile;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "unexpected token", code = "LM1050")]
pub struct UnexpectedToken {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected {expected:?}, got {actual:?} instead")]
    pub range: Range<usize>,

    pub expected: TokenKind,
    pub actual: TokenKind,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "unexpected type", code = "LM1051")]
pub struct UnexpectedType {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected type, got {actual:?} instead")]
    pub range: Range<usize>,

    pub actual: TokenKind,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "unexpected top-level statement", code = "LM1055")]
pub struct InvalidTopLevelStatement {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected a top-level statement, got {actual:?} instead")]
    pub range: Range<usize>,

    pub actual: TokenKind,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "expected identifier", code = "LM1056")]
pub struct ExpectedIdentifier {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected identifier, found {actual:?} instead")]
    pub range: Range<usize>,

    pub actual: TokenKind,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "unexpected function name",
    code = "LM1057",
    help = "function names only allow alphanumeric characters and underscores"
)]
pub struct ExpectedFunctionName {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected a function name")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "enexpected struct name",
    code = "LM1058",
    help = "struct names only allow alphanumeric characters and underscores"
)]
pub struct ExpectedStructName {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected a struct name")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "unexpected trait name",
    code = "LM1059",
    help = "trait names only allow alphanumeric characters and underscores"
)]
pub struct ExpectedTraitName {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected a trait name")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "expected struct property", code = "LM1062")]
pub struct ExpectedStructProperty {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected a struct property definition")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "property in implementation",
    code = "LM1063",
    help = "properties can only be defined in `struct` blocks"
)]
pub struct PropertyInImpl {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("found unexpected property in implementation block")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "method in struct",
    code = "LM1064",
    help = "methods can only be defined in `impl` blocks"
)]
pub struct MethodInStruct {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("found unexpected method in struct block")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "unexpected expression", code = "LM1071")]
pub struct InvalidExpression {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected expression, got {actual:?} instead")]
    pub range: Range<usize>,

    pub actual: TokenKind,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "expected value, found type", code = "LM1073")]
pub struct ExpectedValueType {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected value, found type {actual} instead")]
    pub range: Range<usize>,

    pub actual: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "expected value, found namespace", code = "LM1074")]
pub struct ExpectedValueNamespace {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected value, found namespace {actual} instead")]
    pub range: Range<usize>,

    pub actual: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "invalid literal value", code = "LM1078")]
pub struct InvalidLiteral {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("failed to parse literal value {value} as {target:?}")]
    pub range: Range<usize>,

    pub value: String,
    pub target: TokenKind,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "invalid literal type", code = "LM1079")]
pub struct InvalidLiteralType {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("invalid literal type suffix {found}")]
    pub range: Range<usize>,

    pub found: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "missing delimiter in sequence", code = "LM1080")]
pub struct MissingDelimiterInSequence {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected {delimiter} in sequence")]
    pub range: Range<usize>,

    pub delimiter: TokenKind,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "unexpected function body",
    code = "LM1090",
    help = "either remove the body or change the function to not be external"
)]
pub struct ExternalFunctionBody {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("external functions cannot declare a body")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "invalid import path",
    code = "LM1100",
    help = "imports can be defined as `import std::io (File, Buffer)`"
)]
pub struct InvalidImportPath {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected a one or more identifiers to import, found {found}")]
    pub range: Range<usize>,

    pub found: TokenKind,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "unimplemented", code = "LM9999")]
pub struct Unimplemented {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("unimplemented functionality: {desc}")]
    pub range: Range<usize>,

    pub desc: String,
}
