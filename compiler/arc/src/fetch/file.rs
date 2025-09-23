use std::collections::HashMap;
use std::path::PathBuf;

use error_snippet::{Result, SimpleDiagnostic};
use semver::VersionReq;

use crate::fetch::{DependencyFetcher, PackageMetadata};
use crate::parser::{ManifestDependency, ManifestDependencySource, PackageParser};

/// Defines a [`DependencyFetcher`] which handles dependencies
/// defined on the current filesystem.
pub struct FileDependencyFetcher;

impl DependencyFetcher for FileDependencyFetcher {
    fn metadata(&self, dependency: &ManifestDependencySource) -> Result<PackageMetadata> {
        let ManifestDependencySource::Local { path } = dependency else {
            unreachable!();
        };

        let path = PathBuf::from(path);
        let manifest = PackageParser::locate(&path)?;

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
            dependencies,
        })
    }

    fn fetch(&self, dependency: &ManifestDependency) -> Result<PathBuf> {
        match &dependency.source {
            ManifestDependencySource::Local { path } => Ok(PathBuf::from(path)),

            _ => {
                return Err(SimpleDiagnostic::new(format!(
                    "unsupported path scheme: only local paths are supported, found {dependency}"
                ))
                .into());
            }
        }
    }
}
