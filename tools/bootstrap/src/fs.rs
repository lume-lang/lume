use std::path::Path;

use lume_errors::{MapDiagnostic, Result};

/// Creates a directory recursively.
///
/// If the directory already exists, it is ignored.
pub fn create_dir(path: impl AsRef<Path>) -> Result<()> {
    std::fs::create_dir_all(&path).map_cause(format!("could not create directory {}", path.as_ref().display()))
}

/// Removes a directory recursively.
///
/// If the directory does not exist, it is ignored.
pub fn remove_dir(path: impl AsRef<Path>) -> Result<()> {
    std::fs::remove_dir_all(&path).map_cause(format!("could not delete directory {}", path.as_ref().display()))
}

/// Copies a file or directory to the destination.
///
/// If the source is a directory, it will be copied recursively.
pub fn copy(src: impl AsRef<Path>, dst: impl AsRef<Path>) -> std::io::Result<()> {
    if src.as_ref().is_dir() {
        copy_dir(src, dst)
    } else {
        std::fs::copy(src, dst).map(|_| ())
    }
}

/// Copies a directory recursively.
fn copy_dir(src: impl AsRef<Path>, dst: impl AsRef<Path>) -> std::io::Result<()> {
    std::fs::create_dir_all(&dst)?;

    for entry in std::fs::read_dir(src)? {
        let entry = entry?;

        if entry.file_type()?.is_dir() {
            copy_dir(entry.path(), dst.as_ref().join(entry.file_name()))?;
        } else {
            std::fs::copy(entry.path(), dst.as_ref().join(entry.file_name()))?;
        }
    }
    Ok(())
}

/// Creates a new symbolic link on the file-system, linking `link` to `target`.
///
/// Depending on the current platform, this function will call different native
/// functions.
pub fn link(target: impl AsRef<Path>, link: impl AsRef<Path>) -> Result<()> {
    #[cfg(unix)]
    {
        std::os::unix::fs::symlink(target, link).map_diagnostic()
    }

    #[cfg(windows)]
    {
        std::os::windows::fs::symlink_dir(target, link).map_diagnostic()
    }
}
