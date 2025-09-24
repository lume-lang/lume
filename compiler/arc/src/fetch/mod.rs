pub mod file;
pub mod git;

use std::{collections::HashMap, path::PathBuf};

use crate::parser::ManifestDependencySource;
use lume_errors::Result;

pub use file::*;
pub use git::*;
use lume_span::PackageId;
use semver::{Version, VersionReq};

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

/// Returns the current local cache directory for saving caches and/or clones
/// of remote dependency packages.
#[tracing::instrument(level = "TRACE", ret)]
pub fn local_cache_dir() -> PathBuf {
    LOCAL_CACHE_DIR.to_path_buf()
}

/// Clears the local cache directory for all caches and clones of remote dependency packages.
///
/// To see which directory is the current local cache directory, see [`local_cache_dir`].
#[tracing::instrument(level = "DEBUG", err)]
pub fn clean_local_cache_dir(dry_run: bool) -> Result<()> {
    let lcd = local_cache_dir();
    tracing::debug!("attempting to clear local cache directory ({})", lcd.display());

    // If the directory doesn't exist, we have nothing to do.
    if !lcd.is_dir() {
        tracing::debug!("cache directory does not exist, skipping...");
        return Ok(());
    }

    let cached_packages = match std::fs::read_dir(lcd) {
        Ok(packages) => packages,
        Err(err) => return Err(err.into()),
    };

    for entry in cached_packages {
        let cached_pkg_path = match entry {
            Ok(path) => path.path(),
            Err(err) => return Err(err.into()),
        };

        tracing::trace!("attempting to remove `{}`...", cached_pkg_path.display());

        if dry_run {
            continue;
        }

        let result = if cached_pkg_path.is_dir() {
            std::fs::remove_dir_all(&cached_pkg_path)
        } else {
            std::fs::remove_file(&cached_pkg_path)
        };

        if let Err(err) = result {
            return Err(err.into());
        }
    }

    Ok(())
}

#[derive(Debug, Default)]
pub struct PackageMetadata {
    pub package_id: PackageId,
    pub name: String,
    pub dependencies: HashMap<Version, HashMap<ManifestDependencySource, VersionReq>>,
}

pub trait DependencyFetcher {
    /// Gets the metadata of the given dependency, optionally without
    /// fetching it from the source.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - the path is invalid for the implementation,
    /// - the dependency was found, but inaccessible or invalid,
    /// - the dependency was found, but had no matching versions,
    /// - or some other implementation-dependent error.
    fn metadata(&self, source: &ManifestDependencySource) -> Result<PackageMetadata>;

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
    fn fetch(&self, source: &ManifestDependencySource) -> Result<PathBuf>;
}

impl ManifestDependencySource {
    pub fn get_metadata(&self) -> Result<PackageMetadata> {
        match self {
            ManifestDependencySource::Local { .. } => FileDependencyFetcher.metadata(self),
            ManifestDependencySource::Git { .. } => GitDependencyFetcher.metadata(self),
        }
    }

    pub fn fetch(&self) -> Result<PathBuf> {
        match &self {
            ManifestDependencySource::Local { .. } => FileDependencyFetcher.fetch(self),
            ManifestDependencySource::Git { .. } => GitDependencyFetcher.fetch(self),
        }
    }
}
