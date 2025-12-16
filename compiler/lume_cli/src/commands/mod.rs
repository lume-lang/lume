pub(crate) mod arc;
pub(crate) mod build;
pub(crate) mod fmt;
#[cfg(feature = "lsp")]
pub(crate) mod lsp;
pub(crate) mod run;

use std::env::current_dir;
use std::path::PathBuf;

pub(crate) use arc::ArcCommand;
pub(crate) use build::BuildCommand;
pub(crate) use fmt::FormatCommand;
#[cfg(feature = "lsp")]
pub(crate) use lsp::LanguageServerCommand;
use lume_errors::{IntoDiagnostic, Result};
pub(crate) use run::RunCommand;

use crate::error::*;

/// Gets the absolute path of the given project directory, or current working
/// directory if not specified.
pub fn project_or_cwd(path: Option<&PathBuf>) -> Result<String> {
    let cwd: PathBuf = match current_dir() {
        Ok(cwd) => cwd,
        Err(err) => {
            return Err(CouldNotDetermineBuildPath {
                inner: err.into_diagnostic(),
            }
            .into());
        }
    };

    // If no path is specified, use the current working directory.
    let Some(path) = path else {
        return Ok(cwd.to_string_lossy().into_owned());
    };

    // If the path is absolute, return it as-is.
    if path.is_absolute() {
        return Ok(path.to_string_lossy().into_owned());
    }

    // Otherwise, resolve it from the current working directory.
    let path = match cwd.join(path).canonicalize() {
        Ok(path) => path,
        Err(err) => {
            return Err(CouldNotDetermineBuildPath {
                inner: err.into_diagnostic(),
            }
            .into());
        }
    };

    Ok(path.to_string_lossy().into_owned())
}
