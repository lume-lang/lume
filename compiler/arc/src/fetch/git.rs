use std::path::PathBuf;

use error_snippet::{IntoDiagnostic, Result, SimpleDiagnostic};
use git2::{FetchOptions, build::RepoBuilder};
use semver::VersionReq;

use crate::{
    deps::DependencyPath,
    fetch::{DependencyFetcher, LOCAL_CACHE_DIR},
};

/// Defines a [`DependencyFetcher`] which handles dependencies which live
/// inside of a Git repository, either local or remote.
pub struct GitDependencyFetcher;

impl DependencyFetcher for GitDependencyFetcher {
    fn fetch(&self, path: DependencyPath, _: &VersionReq) -> Result<PathBuf> {
        let DependencyPath::Url(url) = path else {
            return Err(SimpleDiagnostic::new(format!(
                "unsupported path scheme: only Git URLs are supported, found {}",
                path.protocol()
            ))
            .into());
        };

        let repository_path = PathBuf::from(url.path());
        let repository_name = match repository_path.components().next_back() {
            Some(seg) => seg.as_os_str().to_string_lossy().to_string(),
            None => {
                return Err(SimpleDiagnostic::new(format!(
                    "failed to clone repository: could not find repository name from `{}`",
                    url.path()
                ))
                .into());
            }
        };

        let local_directory = LOCAL_CACHE_DIR.join(repository_name);

        if !local_directory.exists() {
            if let Err(err) = std::fs::create_dir_all(&local_directory) {
                return Err(SimpleDiagnostic::new(
                    "failed to clone repository: could not create local directory for clone",
                )
                .add_cause(err.into_diagnostic())
                .into());
            }
        }

        let mut fetch_opts = FetchOptions::new();
        fetch_opts.depth(1);

        let mut clone_builder = RepoBuilder::new();
        clone_builder.fetch_options(fetch_opts);

        if let Err(err) = clone_builder.clone(url.as_str(), local_directory.as_path()) {
            return Err(SimpleDiagnostic::new("failed to clone repository")
                .add_cause(err.into_diagnostic())
                .into());
        }

        Ok(local_directory)
    }
}
