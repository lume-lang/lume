use std::path::PathBuf;

use error_snippet::Result;
use semver::VersionReq;

use crate::deps::DependencyPath;
use crate::deps::FILE_SCHEME;

pub trait DependencyFetcher {
    /// Fetches the package defined at the given path and returns
    /// the path to a local copy of the dependency root.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - the path is invalid for the implementation,
    /// - the dependency was found, but inaccessible or invalid,
    /// - the dependency was found, but had no matching versions,
    /// - or some other implementation-dependent error.
    fn fetch(&self, path: DependencyPath, version: &VersionReq) -> Result<PathBuf>;
}

/// Defines a [`DependencyFetcher`] which handles dependencies
/// defined on the current filesystem.
pub struct FileDependencyFetcher;

impl DependencyFetcher for FileDependencyFetcher {
    fn fetch(&self, path: DependencyPath, _: &VersionReq) -> Result<PathBuf> {
        let path = match path {
            DependencyPath::Path(p) => p,
            DependencyPath::Url(url) if url.scheme() == FILE_SCHEME => PathBuf::from(url.path()),

            DependencyPath::Url(url) => {
                return Err(error_snippet::SimpleDiagnostic::new(format!(
                    "unsupported path scheme: only local paths are supported, found {}",
                    url.scheme()
                ))
                .into());
            }
        };

        Ok(path)
    }
}
