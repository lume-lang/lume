use std::path::PathBuf;

use error_snippet::IntoDiagnostic;
use error_snippet::Result;
use error_snippet::SimpleDiagnostic;
use semver::VersionReq;

use crate::deps::DependencyPath;
use crate::deps::FILE_SCHEME;

/// Defines the name of the environment variable, which defines where
/// Arc should place local clones and/or caches of remote dependencies.
pub const ARC_CACHE_ENVKEY: &str = "ARC_CACHE_DIR";

/// Defines which subfolder to place Arc caches inside, when the cache directory
/// is some existing directory, such as the current directory or home directory.
pub const ARC_CACHE_FOLDER: &str = ".arc";

static LOCAL_CACHE_DIR: std::sync::LazyLock<PathBuf> = std::sync::LazyLock::new(|| {
    // Prioritize the user-defined location for the caching directory.
    if let Some(dir) = std::env::var_os(ARC_CACHE_ENVKEY) {
        return PathBuf::from(dir.to_string_lossy().to_string());
    }

    // Otherwise, attempt to create a new folder in the current working directory.
    if let Ok(cwd) = std::env::current_dir() {
        return cwd.join(ARC_CACHE_FOLDER);
    }

    // Or attempt to place it witin the home directory...
    if let Some(home) = std::env::home_dir() {
        return home.join(ARC_CACHE_FOLDER);
    }

    // If all hope fails, create a temporary directory.
    std::env::temp_dir()
});

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

        let mut fetch_opts = git2::FetchOptions::new();
        fetch_opts.depth(1);

        let mut clone_builder = git2::build::RepoBuilder::new();
        clone_builder.fetch_options(fetch_opts);

        if let Err(err) = clone_builder.clone(url.as_str(), local_directory.as_path()) {
            return Err(SimpleDiagnostic::new("failed to clone repository")
                .add_cause(err.into_diagnostic())
                .into());
        }

        Ok(local_directory)
    }
}
