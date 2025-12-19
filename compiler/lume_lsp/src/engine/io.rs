use std::path::PathBuf;

use indexmap::IndexMap;
use lume_span::FileName;

/// Uniquely identifies a source file.
///
/// Each source file has a parent [`PackageId`], which defines which package it
/// belongs to.
#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceFileId(usize);

impl From<&PathBuf> for SourceFileId {
    fn from(value: &PathBuf) -> Self {
        Self(lume_span::hash_id(value))
    }
}

#[derive(Default)]
pub(crate) struct IO {
    mapped_files: IndexMap<SourceFileId, MappedSourceFile>,
}

impl IO {
    /// Writes the given content to the source file identified by the given
    /// path. If the source file already exists, its content is overwritten.
    pub fn map(&mut self, path: PathBuf, content: String) {
        let source_id = SourceFileId::from(&path);
        let source_file = MappedSourceFile { path, content };

        self.mapped_files.insert(source_id, source_file);
    }

    /// Unmaps the source file identified by the given path, so it no longer
    /// has any in-memory representation.
    pub fn unmap(&mut self, path: &PathBuf) {
        let source_id = SourceFileId::from(path);

        self.mapped_files.swap_remove(&source_id);
    }

    /// Builds the overrides of source files which we currently have in-memory
    /// in the language server.
    ///
    /// Some of these might not need to be overwritten, as they are the same as
    /// they are on the disk. But, since the operation is a
    /// [`IndexMap::extend`]-call, it's a relatively quick operation.
    pub(crate) fn build_source_overrides(&self, root: &PathBuf) -> IndexMap<FileName, String> {
        let mut source_overrides = IndexMap::new();

        for MappedSourceFile { path, content } in self.mapped_files.values() {
            let relative_path = if path.starts_with(root) {
                FileName::Real(path.strip_prefix(root).unwrap().to_path_buf())
            } else {
                FileName::Real(path.clone())
            };

            source_overrides.insert(relative_path, content.clone());
        }

        source_overrides
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MappedSourceFile {
    pub(crate) path: PathBuf,
    pub(crate) content: String,
}
