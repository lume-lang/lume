use std::path::PathBuf;

use lume_errors::Result;

/// Defines the name of the environment variable, which defines where
/// Arc should place local clones and/or caches of remote dependencies.
pub const ARC_CACHE_ENVKEY: &str = "ARC_CACHE_DIR";

/// Defines which subfolder to place Arc caches inside, when the cache directory
/// is some existing directory, such as the current directory or home directory.
pub const ARC_CACHE_FOLDER: &str = "arc";

/// Returns the current local cache directory for saving caches and/or clones
/// of remote dependency packages.
#[tracing::instrument(level = "TRACE", skip_all, ret)]
pub fn local_cache_dir() -> PathBuf {
    // Prioritize the user-defined location for the caching directory.
    if let Some(dir) = std::env::var_os(ARC_CACHE_ENVKEY) {
        return PathBuf::from(dir.to_string_lossy().to_string());
    }

    match lume_assets::determine_lume_home() {
        // Otherwise, attempt to create a new folder in the Lume data directory
        Some(data_dir) => data_dir.join(ARC_CACHE_FOLDER),

        // If all hope fails, create a temporary directory.
        None => std::env::temp_dir(),
    }
}

/// Clears the local cache directory for all caches and clones of remote
/// dependency packages.
///
/// To see which directory is the current local cache directory, see
/// [`local_cache_dir`].
#[tracing::instrument(level = "DEBUG", skip_all, err)]
pub fn clean_local_cache_dir(dry_run: bool) -> Result<()> {
    let lcd = local_cache_dir();
    tracing::debug!("attempting to clear local cache directory ({})", lcd.display());

    // If the directory doesn't exist, we have nothing to do.
    if !lcd.is_dir() {
        tracing::debug!("cache directory does not exist, skipping...");
        return Ok(());
    }

    let cached_packages = std::fs::read_dir(lcd)?;

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
