pub mod deps;
pub mod errors;
pub mod fetch;
pub(crate) mod parser;

use lume_session::DependencyMap;
use parser::PackageParser;

pub use crate::fetch::{clean_local_cache_dir, local_cache_dir};

use error_snippet::Result;
use std::path::PathBuf;

/// Locates the [`Package`] in the given root directory.
///
/// # Errors
///
/// This method may fail if:
/// - the given path has no `Arcfile` stored within it
/// - or the located `Arcfile` doesn't refer to a file.
pub fn locate_package(root: &PathBuf) -> Result<DependencyMap> {
    deps::build_dependency_tree(root)
}
