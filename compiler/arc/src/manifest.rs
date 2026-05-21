use std::collections::HashMap;
use std::fmt::Display;
use std::path::PathBuf;

use indexmap::IndexMap;
use lume_session::{Dependencies, Package};
use lume_span::PackageId;
use semver::{Version, VersionReq};
use serde::Deserialize;
use toml::Spanned;

#[derive(Deserialize, Debug, Clone, PartialEq, Eq)]
pub(crate) struct Manifest {
    /// Defines the root directory which defines this package.
    #[serde(skip)]
    pub path: PathBuf,

    /// Defines the package metadata.
    pub package: ManifestPackage,

    /// Defines the dependencies for the package.
    #[serde(default)]
    pub dependencies: HashMap<String, ManifestDependency>,

    /// Defines the options for the runtime.
    #[serde(default)]
    pub runtime: lume_options::RuntimeOptions,
}

impl Manifest {
    pub fn package_id(&self) -> PackageId {
        if self.package.name.get_ref() == "std" {
            return PackageId::std();
        }

        PackageId::from_name(&self.package.name)
    }
}

impl From<Manifest> for Package {
    fn from(manifest: Manifest) -> Self {
        Self {
            id: manifest.package_id(),
            path: manifest.path,
            name: manifest.package.name.into_inner(),
            lume_version: manifest.package.lume_version.map(Spanned::into_inner),
            version: manifest.package.version.into_inner(),
            description: manifest.package.description,
            license: manifest.package.license,
            repository: manifest.package.repository,
            allow_unsafe: manifest.package.allow_unsafe,
            files: IndexMap::new(),
            dependencies: Dependencies {
                no_std: manifest.package.no_std,
                ..Dependencies::default()
            },
            runtime: manifest.runtime,
        }
    }
}

#[derive(Deserialize, Debug, Clone, PartialEq, Eq)]
pub(crate) struct ManifestPackage {
    /// Defines the name of the package.
    pub name: Spanned<String>,

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
    #[serde(default)]
    pub no_std: bool,

    /// Defines whether the parent package allows declaring and using unsafe
    /// code.
    #[serde(default)]
    pub allow_unsafe: bool,
}

#[derive(Deserialize, Hash, Debug, Clone, PartialEq, Eq)]
pub(crate) struct ManifestDependency {
    /// Defines where the dependency can be found.
    #[serde(flatten)]
    pub source: ManifestDependencySource,

    /// Defines the required version of the dependency, which is required
    /// by the referencing package.
    #[serde(rename = "version")]
    #[serde(default)]
    pub required_version: VersionReq,
}

impl Display for ManifestDependency {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.source.fmt(f)
    }
}

#[derive(Deserialize, Hash, Debug, Clone, PartialEq, Eq)]
#[serde(untagged)]
pub(crate) enum ManifestDependencySource {
    /// Defines a local dependency which exists on the file system.
    ///
    /// Local dependencies cannot define a version requirement, since
    /// only a single version can exist in a folder at any given time.
    Local(FileDependency),

    /// Defines a local or remote Git repository.
    ///
    /// Git repository dependencies can be reference using branch name,
    /// revision or tag.
    Git(GitDependency),
}

impl Display for ManifestDependencySource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local(dep) => write!(f, "{dep}"),
            Self::Git(dep) => write!(f, "{dep}"),
        }
    }
}

#[derive(Deserialize, Hash, Debug, Clone, PartialEq, Eq)]
pub struct FileDependency {
    /// Defines the path to the dependency root on the file system.
    pub path: String,
}

impl Display for FileDependency {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.path)
    }
}

#[derive(Deserialize, Hash, Debug, Clone, PartialEq, Eq)]
pub(crate) struct GitDependency {
    /// Defines the URL of the Git repository.
    ///
    /// The URL can be both local and remote.
    #[serde(rename = "git")]
    pub repository: String,

    /// Optional. Defines the branch name to fetch the dependency from.
    pub branch: Option<String>,

    /// Optional. Defines the tag to fetch the dependency from.
    pub tag: Option<String>,

    /// Optional. Defines the revision to fetch the dependency from.
    pub rev: Option<String>,

    /// Optional. Defines a sub-path to use within the repository.
    pub dir: Option<String>,
}

impl Display for GitDependency {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.repository)
    }
}
