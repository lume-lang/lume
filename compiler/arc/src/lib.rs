pub mod deps;
pub mod errors;
pub mod fetch;
pub(crate) mod parser;
pub(crate) mod serializer;

use crate::deps::DependencyResolver;
pub use crate::fetch::{clean_local_cache_dir, local_cache_dir};
use crate::serializer::PackageParser;

use error_snippet::Result;
use lume_errors::DiagCtxHandle;
use lume_session::Package;
use std::path::PathBuf;

/// Locates the [`Package`] in the given root directory.
///
/// # Errors
///
/// This method may fail if:
/// - the given path has no `Arcfile` stored within it
/// - or the located `Arcfile` doesn't refer to a file.
pub fn locate_package(root: &PathBuf, dcx: DiagCtxHandle) -> Result<Package> {
    DependencyResolver::resolve(root, dcx)
}
