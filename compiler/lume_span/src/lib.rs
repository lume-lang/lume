//! Source files and spans within them, which are used heavily within the Lume compiler.
//!
//! This module is used by most other packages within the Lume compiler, since spans
//! are required to print useful diagnostics to the user - at least if source code is needed.

pub mod id;
pub mod intern;
pub mod source;

use std::{hash::Hash, sync::Arc};

pub use id::*;
pub use intern::*;
pub use source::{FileName, SourceFile, SourceFileId, SourceMap};

/// Hashes some ID using the `FxHasher` algorithm, which was extracted
/// from the Rustc compiler.
///
/// The reason for using this instead of the [`std::hash::DefaultHasher`] is that
/// we require some deterministic hashing algorithm for consistent results, so
/// we can use it for incremental builds and caching. The default hasher does not
/// have any specific algorithm defined, so it cannot be relied upon to create
/// the same hash given the same input.
pub fn hash_id<T: Hash + ?Sized>(id: &T) -> usize {
    fxhash::hash(id)
}

/// Represents some marked location within a source file.
///
/// This structure is used heavily throughout the compiler, to define
/// locations of statements and expressions, which are used to create more useful
/// diagnostics and error messages.
pub type Location = Interned<source::Location>;

impl Location {
    pub fn empty() -> Self {
        source::Location::empty().intern()
    }
}

impl From<Location> for Arc<dyn error_snippet::Source> {
    fn from(value: Location) -> Self {
        value.file.clone()
    }
}

impl From<Location> for error_snippet::SpanRange {
    fn from(value: Location) -> Self {
        error_snippet::SpanRange(value.index.start..value.index.end)
    }
}
