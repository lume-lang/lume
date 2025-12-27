use std::ops::Range;
use std::sync::Arc;

use error_snippet_derive::Diagnostic;
use lume_span::{Location, SourceFile};

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
#[diagnostic(
    message = "cannot declare parameter type on {ty}",
    code = "LM3017",
    help = "remove the type in the parameter"
)]
pub struct SelfWithExplicitType {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("{ty} parameters cannot have a explicit type declared")]
    pub range: Range<usize>,

    pub ty: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "undeclared variable", code = "LM3024")]
pub struct UndeclaredVariable {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("could not find a variable named {name} in the current scope")]
    pub range: Range<usize>,

    pub name: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "duplicate definition", code = "LM3028")]
pub struct DuplicateDefinition {
    #[label(source, "item {name} is already defined within this file")]
    pub duplicate_range: Location,

    #[label(source, note, "original definition found here")]
    pub original_range: Location,

    pub name: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "duplicate type parameter", code = "LM3029")]
pub struct DuplicateTypeParameter {
    #[label(source, "type parameter {name} is already defined")]
    pub duplicate_range: Location,

    #[label(source, note, "original type parameter found here")]
    pub original_range: Location,

    pub name: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "duplicate parameter", code = "LM3030")]
pub struct DuplicateParameter {
    #[label(source, "parameter {name} is already defined")]
    pub duplicate_range: Location,

    #[label(source, note, "original parameter found here")]
    pub original_range: Location,

    pub name: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "duplicate method", code = "LM3032")]
pub struct DuplicateMethod {
    #[label(source, "method {name} is already defined")]
    pub duplicate_range: Location,

    #[label(source, note, "original method found here")]
    pub original_range: Location,

    pub name: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "duplicate field", code = "LM3033")]
pub struct DuplicateField {
    #[label(source, "field {name} is already defined within this struct")]
    pub duplicate_range: Location,

    #[label(source, note, "original field found here")]
    pub original_range: Location,

    pub name: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "duplicate variant", code = "LM3034")]
pub struct DuplicateVariant {
    #[label(source, "variant {name} is already defined in this enum")]
    pub duplicate_range: Location,

    #[label(source, note, "original variant found here")]
    pub original_range: Location,

    pub name: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "no `name` argument in `lang_item` attribute", code = "LM3061")]
pub struct LangItemMissingName {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("`![lang_item]` attribute must define a `name` string argument")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "`name` argument in `lang_item` attribute must be a string",
    code = "LM3062"
)]
pub struct LangItemInvalidNameType {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("`![lang_item]` attribute must define a `name` string argument")]
    pub range: Range<usize>,
}
