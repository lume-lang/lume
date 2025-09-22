use std::path::PathBuf;

use error_snippet::{Result, SimpleDiagnostic};

use crate::fetch::DependencyFetcher;
use crate::parser::{ManifestDependency, ManifestDependencySource};

/// Defines a [`DependencyFetcher`] which handles dependencies
/// defined on the current filesystem.
pub struct FileDependencyFetcher;

impl DependencyFetcher for FileDependencyFetcher {
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
