//! Source files and spans within them, which are used heavily within the Lume compiler.
//!
//! This module is used by most other packages within the Lume compiler, since spans
//! are required to print useful diagnostics to the user - at least if source code is needed.

use std::hash::Hash;
use std::ops::Range;
use std::path::PathBuf;
use std::sync::Arc;

use fxhash::hash64;
use indexmap::IndexMap;

/// Hashes some ID using the FxHasher algorithm, which was extracted
/// from the Rustc compiler.
///
/// The reason for using this instead of the [`std::hash::DefaultHasher`] is that
/// we require some deterministic hashing algorithm for consistent results, so
/// we can use it for incremental builds and caching. The default hasher does not
/// have any specific algorithm defined, so it cannot be relied upon to create
/// the same hash given the same input.
pub fn hash_id<T: Hash + ?Sized>(id: &T) -> u64 {
    hash64(id)
}

/// Uniquely identifies a package.
///
/// Packages are identified by a unique ID, which is used to locate the package's source files.
/// The ID is generated from the name of the package using a hash function.
#[derive(serde::Serialize, Default, Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct PackageId(pub u64);

impl PackageId {
    /// Creates a new empty [`PackageId`].
    pub fn empty() -> Self {
        Self(0)
    }

    /// Creates a new [`PackageId`] with the given name.
    pub fn new<'a>(name: impl Into<&'a str>) -> Self {
        Self(hash_id(name.into()))
    }
}

#[derive(serde::Serialize, Debug, Eq, PartialEq, Clone)]
pub enum FileName {
    /// A file name which is defined by some internal process,
    /// such as in testing or defined on the command line.
    Internal,

    /// A file name which physically exists on the file system.
    Real(PathBuf),
}

impl std::fmt::Display for FileName {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FileName::Internal => write!(fmt, "<internal>"),
            FileName::Real(name) => write!(fmt, "{}", name.to_string_lossy()),
        }
    }
}

/// Uniquely identifies a source file.
///
/// Each source file has a parent [`PackageId`], which defines which package it belongs to.
#[derive(serde::Serialize, Hash, Clone, Copy, PartialEq, Eq)]
pub struct SourceFileId(pub PackageId, pub u64);

impl SourceFileId {
    /// Creates a new empty [`SourceFileId`].
    pub fn empty() -> Self {
        Self(PackageId(0), 0)
    }

    /// Creates a new [`SourceFileId`] with the given parent package ID and name.
    pub fn new<'a>(package: PackageId, name: impl Into<&'a str>) -> Self {
        Self(package, hash_id(name.into()))
    }
}

/// A single source file within a package.
#[derive(serde::Serialize, PartialEq, Eq)]
pub struct SourceFile {
    /// Defines the unique identifier of the source file.
    pub id: SourceFileId,

    /// Defines where the name of the source file came from.
    pub name: FileName,

    /// Defines the content of the source file.
    pub content: String,

    /// Defines the ID of the parent package, which the file belongs to.
    pub package: PackageId,
}

/// Represents some marked location within a source file.
///
/// This structure is used heavily throughout the compiler, to define
/// locations of statements and expressions, which are used to create more useful
/// diagnostics and error messages.
#[derive(Clone, PartialEq, Eq)]
pub struct Location {
    /// Defines the original source code.
    pub file: Arc<SourceFile>,

    /// Defines the marked index range within the source file.
    pub index: Range<usize>,
}

/// Defines a source map, which maps source file IDs to their corresponding source files.
#[derive(Default)]
pub struct SourceMap {
    /// Defines all the source files within the mapping.
    files: IndexMap<SourceFileId, Arc<SourceFile>>,
}

impl SourceMap {
    /// Creates a new empty [`SourceMap`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Gets a source file from the mapping with the given ID, if any.
    pub fn get(&self, idx: SourceFileId) -> Option<&Arc<SourceFile>> {
        self.files.get(&idx)
    }

    /// Inserts a new source file into the mapping.
    pub fn insert(&mut self, file: Arc<SourceFile>) {
        self.files.insert(file.id, file);
    }
}
