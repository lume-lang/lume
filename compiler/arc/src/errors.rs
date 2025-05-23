use std::{ops::Range, path::PathBuf, sync::Arc};

use error_snippet::Error;
use error_snippet_derive::Diagnostic;
use lume_span::SourceFile;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "missing Arcfile within {dir:?}", code = "ARC0101")]
pub struct ArcfileMissing {
    pub dir: PathBuf,
}

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
#[diagnostic(message = "property missing in block", code = "ARC0107")]
pub struct ArcfileMissingProperty {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected block to have property `name`")]
    pub range: Range<usize>,

    pub name: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "unexpected type", code = "ARC0108")]
pub struct ArcfileUnexpectedType {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("expected type {expected}, but found {found}")]
    pub range: Range<usize>,

    pub expected: String,
    pub found: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "unknown item", code = "ARC0202")]
pub struct ArcfileUnknownItem {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("unknown item {name} found")]
    pub range: Range<usize>,

    pub name: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "missing package name argument",
    code = "ARC0210",
    help = "define the package name using `Package \"package-name\" {{ ... }}`"
)]
pub struct ArcfileMissingName {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("`Package` package has no named argument")]
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

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "no packages defined in {path}",
    code = "ARC0215",
    help = "define a package using the `Package \"name\" {{ ... }}` syntax"
)]
pub struct ArcfileNoPackages {
    pub path: String,
}
