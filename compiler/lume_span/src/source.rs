//! Source files and spans within them, which are used heavily within the Lume compiler.
//!
//! This module is used by most other packages within the Lume compiler, since spans
//! are required to print useful diagnostics to the user - at least if source code is needed.

use std::hash::Hash;
use std::ops::Range;
use std::path::PathBuf;
use std::sync::Arc;

use error_snippet::Result;
use error_snippet_derive::Diagnostic;
use indexmap::IndexMap;

use crate::{PackageId, hash_id};

#[derive(Hash, Debug, Eq, PartialEq, Clone)]
pub enum FileName {
    /// A file name which is defined by some internal process,
    /// such as in testing or defined on the command line.
    Internal,

    /// A file name which physically exists on the file system.
    Real(PathBuf),
}

impl FileName {
    /// Attempts to convert the [`FileName`] to a [`PathBuf`] instance.
    pub fn to_pathbuf(&self) -> &PathBuf {
        static EMPTY_BUF: std::sync::LazyLock<PathBuf> = std::sync::LazyLock::new(PathBuf::new);

        match self {
            FileName::Real(buf) => buf,
            FileName::Internal => &EMPTY_BUF,
        }
    }
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
#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceFileId(pub PackageId, pub usize);

impl SourceFileId {
    /// Creates a new empty [`SourceFileId`].
    #[inline]
    #[must_use]
    pub fn empty() -> Self {
        Self(PackageId::empty(), 0)
    }

    /// Creates a new [`SourceFileId`] with the given parent package ID and name.
    pub fn new(package: PackageId, name: impl Into<String>) -> Self {
        Self(package, hash_id(&name.into()))
    }
}

/// A single source file within a package.
#[derive(Debug, PartialEq, Eq)]
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

impl SourceFile {
    /// Creates a new [`SourceFile`] with the given parent package ID and name.
    pub fn new(package: PackageId, path: impl Into<PathBuf>, content: impl Into<String>) -> Self {
        let path: PathBuf = path.into();

        Self {
            id: SourceFileId::new(package, path.display().to_string()),
            name: FileName::Real(path),
            content: content.into(),
            package,
        }
    }

    /// Creates a new empty [`SourceFile`] with no content.
    #[inline]
    #[must_use]
    pub fn empty() -> Self {
        Self::internal("")
    }

    /// Creates a new internal [`SourceFile`] with the given content.
    pub fn internal(content: impl Into<String>) -> Self {
        Self {
            id: SourceFileId::empty(),
            name: FileName::Internal,
            content: content.into(),
            package: PackageId::empty(),
        }
    }
}

impl error_snippet::Source for SourceFile {
    fn name(&self) -> Option<&str> {
        match &self.name {
            FileName::Internal => None,
            FileName::Real(name) => name.as_os_str().to_str(),
        }
    }

    fn content(&self) -> Box<&str> {
        Box::new(&self.content)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Location {
    /// Defines the original source code.
    pub file: Arc<SourceFile>,

    /// Defines the marked index range within the source file.
    pub index: Range<usize>,
}

impl Location {
    #[inline]
    #[must_use]
    pub fn empty() -> Self {
        Self {
            file: Arc::new(SourceFile::empty()),
            index: 0..0,
        }
    }

    #[inline]
    #[must_use]
    pub fn start(&self) -> usize {
        self.index.start
    }

    #[inline]
    #[must_use]
    pub fn end(&self) -> usize {
        self.index.end
    }

    #[inline]
    #[must_use]
    pub fn len(&self) -> usize {
        self.end() - self.start()
    }

    #[inline]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[must_use]
    pub fn coordinates(&self) -> (usize, usize) {
        if self.start() > self.len() {
            return (0, 0);
        }

        let src = &self.file.content;

        let mut line = 0;
        let mut col = 0;
        let mut last_line_start = 0;

        for (i, c) in src.char_indices() {
            if i == self.start() {
                return (line, col);
            }

            if c == '\n' {
                line += 1;
                last_line_start = i + 1;
            }

            if i >= last_line_start {
                col = i - last_line_start;
            }
        }

        (line, src[last_line_start..].chars().count())
    }
}

impl std::fmt::Debug for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file.name, self.start(), self.end())
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file.name, self.start(), self.end())
    }
}

impl From<Location> for Arc<dyn error_snippet::Source> {
    fn from(value: Location) -> Self {
        value.file
    }
}

impl From<Location> for error_snippet::SpanRange {
    fn from(value: Location) -> Self {
        value.index.into()
    }
}

impl std::hash::Hash for Location {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.file.name.hash(state);
        self.index.hash(state);
    }
}

#[derive(Debug, Diagnostic)]
#[diagnostic(message = "could not find source file with ID {id:?}")]
pub struct InvalidSourceFile {
    pub id: SourceFileId,
}

/// Defines a source map, which maps source file IDs to their corresponding source files.
#[derive(Default, Debug)]
pub struct SourceMap {
    /// Defines all the source files within the mapping.
    files: IndexMap<SourceFileId, Arc<SourceFile>>,
}

impl SourceMap {
    /// Creates a new empty [`SourceMap`].
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Gets a source file from the mapping with the given ID, if any.
    #[inline]
    #[must_use]
    pub fn get(&self, idx: SourceFileId) -> Option<Arc<SourceFile>> {
        self.files.get(&idx).cloned()
    }

    /// Gets a source file from the mapping with the given ID, if any.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the given ID was not found within the source map.
    /// For a non-failing method, see [`SourceMap::get()`].
    #[inline]
    pub fn get_or_err(&self, idx: SourceFileId) -> Result<Arc<SourceFile>> {
        match self.get(idx) {
            Some(v) => Ok(v),
            None => Err(InvalidSourceFile { id: idx }.into()),
        }
    }

    /// Inserts a new source file into the mapping.
    pub fn insert(&mut self, file: Arc<SourceFile>) {
        self.files.insert(file.id, file);
    }
}
