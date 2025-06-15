use std::path::PathBuf;

use error_snippet::{Result, SimpleDiagnostic};
use semver::VersionReq;

use crate::{
    deps::{DependencyPath, FILE_SCHEME},
    fetch::DependencyFetcher,
};

/// Defines a [`DependencyFetcher`] which handles dependencies
/// defined on the current filesystem.
pub struct FileDependencyFetcher;

impl DependencyFetcher for FileDependencyFetcher {
    fn fetch(&self, path: DependencyPath, _: &VersionReq) -> Result<PathBuf> {
        let path = match path {
            DependencyPath::Path(p) => p,
            DependencyPath::Url(url) if url.scheme() == FILE_SCHEME => PathBuf::from(url.path()),

            DependencyPath::Url(url) => {
                return Err(SimpleDiagnostic::new(format!(
                    "unsupported path scheme: only local paths are supported, found {}",
                    url.scheme()
                ))
                .into());
            }
        };

        Ok(path)
    }
}
