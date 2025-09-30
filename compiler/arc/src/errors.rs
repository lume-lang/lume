use std::fmt::Debug;
use std::ops::Range;
use std::sync::Arc;

use error_snippet::Error;
use error_snippet_derive::Diagnostic;
use lume_span::SourceFile;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "missing Arcfile within {dir}", code = "ARC0101")]
pub struct ArcfileMissing {
    pub dir: String,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "failed to read Arcfile", code = "ARC0102")]
pub struct ArcfileIoError {
    #[cause]
    pub inner: Error,
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
