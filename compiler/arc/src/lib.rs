pub mod cache;
pub mod errors;
pub(crate) mod manifest;
pub(crate) mod parser;
pub(crate) mod resolve;

use std::path::Path;

use error_snippet::Result;
use lume_errors::DiagCtxHandle;
use lume_session::{DependencyMap, FileLoader};

pub use crate::cache::{clean_local_cache_dir, local_cache_dir};

pub const DEFAULT_ARCFILE: &str = "Arcfile";

/// Locates the [`lume_session::Package`] in the given root directory.
///
/// # Errors
///
/// This method may fail if:
/// - the given path has no `Arcfile` stored within it
/// - or the located `Arcfile` doesn't refer to a file.
pub fn locate_package<L: FileLoader>(root: &Path, loader: &L, dcx: DiagCtxHandle) -> Result<DependencyMap> {
    let resolver = resolve::Resolver::new(loader, dcx);
    resolve::resolve(root, resolver)
}
