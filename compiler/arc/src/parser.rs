use std::collections::HashMap;
use std::fmt::Display;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::errors::*;

use error_snippet::{Label, WithSource};
use lume_errors::{Result, SimpleDiagnostic};
use lume_session::{Dependencies, Package};
use lume_span::{PackageId, SourceFile};
use semver::{Version, VersionReq};
use serde::Deserialize;
use toml::Spanned;
use url::Url;

pub const DEFAULT_ARCFILE: &str = "Arcfile";

#[derive(Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Manifest {
    /// Defines the root directory which defines this package.
    #[serde(skip)]
    pub path: PathBuf,

    /// Defines the package metadata.
    pub package: ManifestPackage,

    /// Defines the dependencies for the package.
    #[serde(default)]
    pub dependencies: HashMap<String, ManifestDependency>,
}

impl Manifest {
    pub fn package_id(&self) -> PackageId {
        PackageId::from_name(&self.package.name)
    }
}

impl From<Manifest> for Package {
    fn from(manifest: Manifest) -> Self {
        Self {
            id: manifest.package_id(),
            path: manifest.path,
            name: manifest.package.name,
            lume_version: manifest.package.lume_version.map(|v| v.into_inner()),
            version: manifest.package.version.into_inner(),
            description: manifest.package.description,
            license: manifest.package.license,
            repository: manifest.package.repository,
            files: Vec::new(),
            dependencies: Dependencies {
                no_std: manifest.package.no_std,
                ..Dependencies::default()
            },
        }
    }
}

#[derive(Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ManifestPackage {
    /// Defines the name of the package.
    pub name: String,

    /// Defines the current version of the package.
    pub version: Spanned<Version>,

    /// Defines the minimum required version of Lume.
    pub lume_version: Option<Spanned<VersionReq>>,

    /// Defines an optional description of the package.
    pub description: Option<String>,

    /// Defines the license of the source code within the package. Optional.
    pub license: Option<String>,

    /// Defines the URL of the source code repository for the package.
    pub repository: Option<String>,

    /// Defines whether the parent [`Package`] should compile without linking
    /// the standard library. Defaults to [`false`].
    pub no_std: bool,
}

#[derive(Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ManifestDependency {
    /// Defines where the dependency can be found.
    #[serde(flatten)]
    pub source: ManifestDependencySource,

    /// Defines the required version of the dependency, which is required
    /// by the referencing package.
    #[serde(rename = "version")]
    #[serde(default)]
    pub required_version: Option<VersionReq>,
}

impl Display for ManifestDependency {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.source.fmt(f)
    }
}

#[derive(Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(untagged)]
pub enum ManifestDependencySource {
    /// Defines a local dependency which exists on the file system.
    ///
    /// Local dependencies cannot define a version requirement, since
    /// only a single version can exist in a folder at any given time.
    Local {
        /// Defines the path to the dependency root on the file system.
        path: String,
    },

    /// Defines a local or remote Git repository.
    ///
    /// Git repository dependencies can be reference using branch name,
    /// revision or tag.
    Git {
        /// Defines the URL of the Git repository.
        ///
        /// The URL can be both local and remote.
        #[serde(rename = "git")]
        repository: String,

        /// Optional. Defines the branch name to fetch the dependency from.
        branch: Option<String>,

        /// Optional. Defines the tag to fetch the dependency from.
        tag: Option<String>,

        /// Optional. Defines the revision to fetch the dependency from.
        rev: Option<String>,

        /// Optional. Defines a sub-path to use within the repository.
        dir: Option<String>,
    },
}

impl Display for ManifestDependencySource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local { .. } => f.write_str("local"),
            Self::Git { .. } => f.write_str("git"),
        }
    }
}

pub(crate) struct PackageParser {
    /// Absolute path to the package's Arcfile.
    path: PathBuf,

    /// Source file of the project's Arcfile.
    source: Arc<SourceFile>,

    /// Defines the current Lume version.
    current_lume_version: Version,
}

impl PackageParser {
    /// Creates a new [`PackageParser`] instance using the given `Arcfile` source file path.
    ///
    /// # Errors
    ///
    /// This method may fail if the given path is unreadable or
    /// otherwise inaccessible.
    fn new(path: &Path) -> Result<Self> {
        let content = match std::fs::read_to_string(path) {
            Ok(content) => content,
            Err(err) => {
                return Err(ArcfileIoError { inner: err.into() }.into());
            }
        };

        let file_name: String = match path.file_name() {
            Some(os_str) => os_str.to_string_lossy().into_owned(),
            None => DEFAULT_ARCFILE.into(),
        };

        let source = SourceFile::new(PackageId::empty(), file_name, content);

        Ok(Self::from_source(path, Arc::new(source)))
    }

    /// Creates a new [`PackageParser`] instance using the given `Arcfile` source file.
    ///
    /// # Errors
    ///
    /// This method may fail if the given `Arcfile` is improperly formatted or
    /// otherwise invalid.
    pub fn from_source(path: &Path, source: Arc<SourceFile>) -> Self {
        Self {
            path: path.to_path_buf(),
            source,
            current_lume_version: Self::current_lume_version(),
        }
    }

    /// Creates a new [`PackageParser`] instance by locating the `Arcfile` in the given root directory.
    ///
    /// # Errors
    ///
    /// This method may fail if:
    /// - the given path has no `Arcfile` stored within it,
    /// - the located `Arcfile` doesn't refer to a file
    /// - or if the given `Arcfile` is otherwise invalid
    pub fn locate(root: &Path) -> Result<Manifest> {
        let url = normalize_path_url(root)?;

        if url.scheme() != "file" {
            return Err(error_snippet::SimpleDiagnostic::new(format!(
                "only file:// URIs are supported, found {}",
                url.scheme()
            ))
            .into());
        }

        let root = url.to_file_path().unwrap();
        let path = root.join(DEFAULT_ARCFILE);

        if !path.is_file() {
            return Err(ArcfileMissing {
                dir: root.display().to_string(),
            }
            .into());
        }

        Self::new(&path)?.parse()
    }

    /// Parses the [`Manifest`] instance from the input source code.
    fn parse(&mut self) -> Result<Manifest> {
        let manifest = match toml::from_str::<Manifest>(&self.source.content) {
            Ok(mut manifest) => {
                manifest.path = self.path.parent().unwrap().to_path_buf();
                manifest
            }
            Err(err) => {
                let source_label = Label::error(None, err.span().unwrap_or_default(), err.message());

                return Err(SimpleDiagnostic::new("failed to parse Arcfile")
                    .with_label(source_label)
                    .with_source(self.source.clone())
                    .into());
            }
        };

        self.verify_lume_version(&manifest)?;

        Ok(manifest)
    }

    /// Verifies that the current Lume compiler version is compatible
    /// with the one required by the [`Manifest`].
    pub(crate) fn verify_lume_version(&self, manifest: &Manifest) -> Result<()> {
        let Some(required_version) = manifest.package.lume_version.clone() else {
            return Ok(());
        };

        let current_lume_version = self.current_lume_version.clone();

        if !required_version.get_ref().matches(&current_lume_version) {
            return Err(ArcfileIncompatibleLumeVersion {
                source: self.source.clone(),
                range: required_version.span().clone(),
                current: current_lume_version,
                required: required_version.into_inner(),
            }
            .into());
        }

        Ok(())
    }

    /// Gets the current Lume compiler version.
    pub(crate) fn current_lume_version() -> Version {
        let version_str = env!("CARGO_PKG_VERSION");

        match Version::parse(version_str) {
            Ok(version) => version,
            Err(err) => panic!("Invalid Lume compiler version `{version_str}`: {err}"),
        }
    }
}

fn normalize_path_url(path: &Path) -> Result<Url> {
    let path_str = path.as_os_str().to_str().unwrap();

    if let Ok(url) = url::Url::parse(path_str) {
        return Ok(url);
    }

    if let Ok(url) = url::Url::parse(&format!("file://{path_str}")) {
        return Ok(url);
    }

    Err(SimpleDiagnostic::new(format!("invalid file URI: {}", path.display())).into())
}
