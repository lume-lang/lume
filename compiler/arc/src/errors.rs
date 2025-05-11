use std::{ops::Range, sync::Arc};

use error_snippet::Error;
use error_snippet_derive::Diagnostic;
use lume_span::SourceFile;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "failed to read Arcfile", code = "ARC0102")]
pub struct ArcfileIoError {
    #[related]
    pub inner: Vec<Error>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "failed to read Arcfile", code = "ARC0103")]
pub struct ArcfileGlobError {
    #[related]
    pub inner: Vec<Error>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "failed to parse Arcfile", code = "ARC0104")]
pub struct ArcfileTomlError {
    #[related]
    pub inner: Vec<Error>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "unexpected type", code = "ARC0108")]
pub struct ArcfileUnexpectedType {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("Expected `{name}` to be of type '{expected}', but found '{found}'")]
    pub range: Range<usize>,

    pub name: String,
    pub expected: String,
    pub found: &'static str,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "no `{name}` section was found in the Arcfile", code = "ARC0203")]
pub struct ArcfileMissingSection {
    pub name: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "missing `name` attribute", code = "ARC0210")]
pub struct ArcfileMissingName {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("Package table is missing a `name` field.")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "missing `lume_version` attribute", code = "ARC0212")]
pub struct ArcfileMissingLumeVersion {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("Package table is missing a `lume_version` field.")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "invalid version string", code = "ARC0213")]
pub struct ArcfileInvalidVersion {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("Invalid version `{version}` for field `{field}`")]
    pub range: Range<usize>,

    pub version: String,

    pub field: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "incompatible Lume version",
    code = "ARC0214",
    help = "consider updating your Lume compiler to the newest version"
)]
pub struct ArcfileIncompatibleLumeVersion {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("Current Lume version {current} is lower than minimum required version {required}.")]
    pub range: Range<usize>,

    pub current: semver::Version,
    pub required: semver::VersionReq,
}
