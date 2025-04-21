use std::ops::Range;

use lume_ast::Type;
use lume_diag::source::NamedSource;
use lume_diag_macros::Diagnostic;

use crate::lexer::TokenKind;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Unexpected character", code = "LM1020", help = "Check your syntax")]
pub struct UnexpectedCharacter {
    #[span]
    pub source: NamedSource,

    #[label("Unexpected character '{char}'")]
    pub range: Range<usize>,

    pub char: char,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "Expected ending quote",
    code = "LM1021",
    help = "Did you forget to end your string?"
)]
pub struct MissingEndingQuote {
    #[span]
    pub source: NamedSource,

    #[label("String literal was started, but has no matching end-quote")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "End of file", code = "LM1023", help = "Has the file been fully written?")]
pub struct UnexpectedEndOfFile {
    #[span]
    pub source: NamedSource,

    #[label("Unexpected end-of-file")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Unexpected token", code = "LM1050")]
pub struct UnexpectedToken {
    #[span]
    pub source: NamedSource,

    #[label("Expected {expected:?}, got {actual:?} instead")]
    pub range: Range<usize>,

    pub expected: TokenKind,
    pub actual: TokenKind,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Unexpected type", code = "LM1051")]
pub struct UnexpectedType {
    #[span]
    pub source: NamedSource,

    #[label("Expected type, got {actual:?} instead")]
    pub range: Range<usize>,

    pub actual: TokenKind,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Unexpected top-level statement", code = "LM1055")]
pub struct InvalidTopLevelStatement {
    #[span]
    pub source: NamedSource,

    #[label("Expected a top-level statement, got {actual:?} instead")]
    pub range: Range<usize>,

    pub actual: TokenKind,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Expected identifier", code = "LM1056")]
pub struct ExpectedIdentifier {
    #[span]
    pub source: NamedSource,

    #[label("Expected identifier")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "Unexpected function name",
    code = "LM1057",
    help = "Function names only allow alphanumeric characters and underscores"
)]
pub struct ExpectedFunctionName {
    #[span]
    pub source: NamedSource,

    #[label("Expected a function name")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "Unexpected class name",
    code = "LM1058",
    help = "Class names only allow alphanumeric characters and underscores"
)]
pub struct ExpectedClassName {
    #[span]
    pub source: NamedSource,

    #[label("Expected a class name")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "Unexpected trait name",
    code = "LM1059",
    help = "Trait names only allow alphanumeric characters and underscores"
)]
pub struct ExpectedTraitName {
    #[span]
    pub source: NamedSource,

    #[label("Expected a trait name")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "Unexpected class member",
    code = "LM1062",
    help = "Class members can be defined using `let` for properties or `fn` for methods."
)]
pub struct ExpectedClassMember {
    #[span]
    pub source: NamedSource,

    #[label("Expected a class member definition")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Unexpected expression", code = "LM1071")]
pub struct InvalidExpression {
    #[span]
    pub source: NamedSource,

    #[label("Expected expression, got {actual:?} instead")]
    pub range: Range<usize>,

    pub actual: TokenKind,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Invalid literal value", code = "LM1078")]
pub struct InvalidLiteral {
    #[span]
    pub source: NamedSource,

    #[label("Failed to parse literal value `{value}` as {target:?}")]
    pub range: Range<usize>,

    pub value: String,
    pub target: TokenKind,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Invalid literal type", code = "LM1079")]
pub struct InvalidLiteralType {
    #[span]
    pub source: NamedSource,

    #[label("Invalid literal type suffix '{found}'")]
    pub range: Range<usize>,

    pub found: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "Unexpected function body",
    code = "LM1090",
    help = "Either remove the body or change the function to not be external"
)]
pub struct ExternalFunctionBody {
    #[span]
    pub source: NamedSource,

    #[label("External functions cannot declare a body")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Unexpected `else if` clause", code = "LM1097")]
pub struct UnlessElseIfClause {
    #[span]
    pub source: NamedSource,

    #[label("`unless` conditionals cannot have `else if` clauses")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Invalid trait type", code = "LM1099")]
pub struct InvalidTraitType {
    #[span]
    pub source: NamedSource,

    #[label("Trait types must be either scalar or generic, found `{found}`.")]
    pub range: Range<usize>,

    pub found: Type,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "Invalid import path",
    code = "LM1100",
    help = "Imports can be defined as `import std.io (File, Buffer)`"
)]
pub struct InvalidImportPath {
    #[span]
    pub source: NamedSource,

    #[label("Expected a one or more identifiers to import, found `{found}`.")]
    pub range: Range<usize>,

    pub found: TokenKind,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "Constrained type parameter not found",
    code = "LM1109",
    help = "You can only define constraints for type parameters defined in the current definition",
    help = "Defined type parameters: {defined:?}"
)]
pub struct ConstrainedTypeParameterNotFound {
    #[span]
    pub source: NamedSource,

    #[label("Could not find type parameter `{found}`")]
    pub range: Range<usize>,

    pub found: String,

    pub defined: Vec<String>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Unimplemented", code = "LM9999")]
pub struct Unimplemented {
    #[span]
    pub source: NamedSource,

    #[label("Unimplemented functionality: {desc}")]
    pub range: Range<usize>,

    pub desc: String,
}
