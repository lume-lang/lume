pub(crate) mod run;

use crate::error::*;
use diag::Result;
use std::env::current_dir;
use std::path::{Path, PathBuf};

/// Gets the absolute path of the given project directory, or current working directory if not specified.
pub fn project_or_cwd(path: Option<&str>) -> Result<String> {
    let cwd: PathBuf = match current_dir() {
        Ok(cwd) => cwd,
        Err(err) => return Err(CouldNotDetermineBuildPath { inner: err }.into()),
    };

    // If no path is specified, use the current working directory.
    if let None = path {
        return Ok(cwd.to_string_lossy().into_owned());
    }

    // If the path is absolute, return it as-is.
    let path = Path::new(path.unwrap());
    if path.is_absolute() {
        return Ok(path.to_string_lossy().into_owned());
    }

    // Otherwise, resolve it from the current working directory.
    let path = match cwd.join(path).canonicalize() {
        Ok(path) => path,
        Err(err) => return Err(CouldNotDetermineBuildPath { inner: err }.into()),
    };

    Ok(path.to_string_lossy().into_owned())
}
