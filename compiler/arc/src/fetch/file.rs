use std::collections::HashMap;
use std::fmt::Display;
use std::path::PathBuf;

use error_snippet::Result;
use lume_session::FileLoader;
use semver::VersionReq;
use serde::Deserialize;

use crate::fetch::{DependencyFetcher, PackageMetadata};
use crate::parser::{ManifestDependencySource, PackageParser};

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

/// Defines a [`DependencyFetcher`] which handles dependencies
/// defined on the current filesystem.
pub struct FileDependencyFetcher;

impl DependencyFetcher for FileDependencyFetcher {
    type Source = FileDependency;

    fn metadata<L: FileLoader>(&self, FileDependency { path }: &Self::Source, loader: &L) -> Result<PackageMetadata> {
        let path = PathBuf::from(path);
        let manifest = PackageParser::locate(&path, loader)?;

        let package_id = manifest.package_id();
        let version = manifest.package.version.get_ref().clone();

        let version_dependencies: HashMap<ManifestDependencySource, VersionReq> = manifest
            .dependencies
            .into_values()
            .map(|dep| (dep.source, dep.required_version))
            .collect();

        let mut dependencies = HashMap::with_capacity(1);
        dependencies.insert(version, version_dependencies);

        Ok(PackageMetadata {
            package_id,
            name: manifest.package.name.into_inner(),
            dependencies,
        })
    }

    fn fetch(&self, source: &Self::Source) -> Result<PathBuf> {
        Ok(PathBuf::from(&source.path))
    }
}
