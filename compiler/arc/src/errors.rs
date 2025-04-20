use std::ops::Range;

use lume_diag::source::NamedSource;
use lume_diag_macros::Diagnostic;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Error occured while reading Arcfile", code = "ARC0102")]
pub struct ArcfileIoError {
    #[source]
    pub inner: std::io::Error,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Error occured while reading Arcfile", code = "ARC0103")]
pub struct ArcfileGlobError {
    #[source]
    pub inner: glob::GlobError,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Error occured while parsing Arcfile", code = "ARC0104")]
pub struct ArcfileTomlError {
    #[source]
    pub inner: toml::de::Error,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "No `{name}` section was found in the Arcfile", code = "ARC0203")]
pub struct ArcfileMissingSection {
    pub name: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "Property `{name}` must be a `{expected}`, but found `{actual}`",
    code = "ARC0204"
)]
pub struct ArcfileInvalidPropertyType {
    pub name: String,

    pub expected: String,

    pub actual: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "No package name was found in the Arcfile", code = "ARC0210")]
pub struct ArcfileMissingName {
    #[span]
    pub source: NamedSource,

    #[label("Package table is missing a `name` field.")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "No package version was found in the Arcfile", code = "ARC0211")]
pub struct ArcfileMissingVersion {
    #[span]
    pub source: NamedSource,

    #[label("Package table is missing a `version` field.")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "No minimum required Lume version was found in the Arcfile",
    code = "ARC0212"
)]
pub struct ArcfileMissingLumeVersion {
    #[span]
    pub source: NamedSource,

    #[label("Package table is missing a `lume_version` field.")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Invalid version `{version}` for field `{field}`", code = "ARC0213")]
pub struct ArcfileInvalidVersion {
    #[span]
    pub source: NamedSource,

    #[label("This field should be SemVer-compatible")]
    pub range: Range<usize>,

    pub version: String,

    pub field: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "Lume version `{current}` is incompatible with the required version `{required}`",
    code = "ARC0214"
)]
pub struct ArcfileIncompatibleLumeVersion {
    #[span]
    pub source: NamedSource,

    #[label("This field should be SemVer-compatible")]
    pub range: Range<usize>,

    pub current: String,

    pub required: String,
}
