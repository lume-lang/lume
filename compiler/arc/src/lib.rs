pub mod deps;
pub mod errors;
pub mod fetch;
pub(crate) mod parser;

use std::path::Path;

use error_snippet::Result;
use lume_errors::DiagCtxHandle;
use lume_session::{DependencyMap, FileLoader};
use parser::PackageParser;

pub use crate::fetch::{clean_local_cache_dir, local_cache_dir};

/// Locates the [`lume_session::Package`] in the given root directory.
///
/// # Errors
///
/// This method may fail if:
/// - the given path has no `Arcfile` stored within it
/// - or the located `Arcfile` doesn't refer to a file.
pub fn locate_package<L: FileLoader>(root: &Path, loader: &L, dcx: DiagCtxHandle) -> Result<DependencyMap> {
    deps::build_dependency_tree(root, loader, dcx)
}
