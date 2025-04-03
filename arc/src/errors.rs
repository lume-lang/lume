use miette::Diagnostic;

#[derive(thiserror::Error, Diagnostic, Debug)]
pub enum ArcError {
    #[error(transparent)]
    #[diagnostic(code(lume::arc::io_error))]
    IoError(#[from] std::io::Error),

    #[error(transparent)]
    #[diagnostic(code(lume::arc::glob_error))]
    GlobError(#[from] glob::GlobError),

    #[error(transparent)]
    #[diagnostic(code(lume::arc::parsing_error))]
    TomlError(#[from] toml::de::Error),

    #[error(transparent)]
    #[diagnostic(code(lume::arc::arcfile_error))]
    ArcfileError(#[from] ArcfileErrorKind),
}

#[derive(thiserror::Error, miette::Diagnostic, Debug, Clone, PartialEq, Eq)]
#[error("Error occured while reading Arcfile")]
pub enum ArcfileErrorKind {
    #[error("No `{0}` section was found in the Arcfile")]
    #[diagnostic(code(ARC0001))]
    MissingSection(String),

    #[error("Property `{0}` must be a `{1}`, but found `{2}`")]
    #[diagnostic(code(ARC0002))]
    InvalidPropertyType(String, String, String),

    #[error("No package name was found in the Arcfile")]
    #[diagnostic(code(ARC0003))]
    MissingName,

    #[error("No package version was found in the Arcfile")]
    #[diagnostic(code(ARC0004))]
    MissingVersion,

    #[error("No minimum required Lume version was found in the Arcfile")]
    #[diagnostic(code(ARC0005))]
    MissingLumeVersion,

    #[error("Invalid version `{0}` for field `{1}`")]
    #[diagnostic(code(ARC0006))]
    InvalidVersion(String, String),

    #[error("Lume version `{0}` is incompatible with the required version `{1}`")]
    #[diagnostic(code(ARC0007), help("Please update Lume to the required version"))]
    IncompatibleLumeVersion(String, String),
}
