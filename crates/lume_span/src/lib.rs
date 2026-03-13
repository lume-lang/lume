//! Source files and spans within them, which are used heavily within the Lume
//! compiler.
//!
//! This module is used by most other packages within the Lume compiler, since
//! spans are required to print useful diagnostics to the user - at least if
//! source code is needed.

pub mod id;
pub mod intern;
pub mod source;

use std::sync::Arc;

pub use id::*;
pub use intern::*;
pub use source::{FileName, SourceFile, SourceFileId, SourceMap};

/// Represents some marked location within a source file.
///
/// This structure is used heavily throughout the compiler, to define
/// locations of statements and expressions, which are used to create more
/// useful diagnostics and error messages.
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

impl Default for Location {
    fn default() -> Self {
        Self::empty()
    }
}

impl PartialOrd for Location {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Location {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.file.package.cmp(&other.file.package) {
            std::cmp::Ordering::Less => return std::cmp::Ordering::Less,
            std::cmp::Ordering::Greater => return std::cmp::Ordering::Greater,
            std::cmp::Ordering::Equal => {}
        }

        match self.file.id.1.cmp(&other.file.id.1) {
            std::cmp::Ordering::Less => std::cmp::Ordering::Less,
            std::cmp::Ordering::Greater => std::cmp::Ordering::Greater,
            std::cmp::Ordering::Equal => self.index.start.cmp(&other.index.start),
        }
    }
}
