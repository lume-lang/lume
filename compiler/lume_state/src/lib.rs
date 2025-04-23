use std::hash::Hash;

use arc::ProjectId;
use fxhash::hash64;
use indexmap::IndexMap;
use lume_diag::{Result, source::NamedSource};
use lume_types::TypeDatabaseContext;

mod errors;

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

/// Uniquely identifies a module within a compilation job.
#[derive(serde::Serialize, Hash, Clone, Copy, PartialEq, Eq)]
pub struct ModuleId(pub u64);

impl ModuleId {
    pub fn empty() -> Self {
        Self(0)
    }
}

impl From<ProjectId> for ModuleId {
    fn from(value: ProjectId) -> Self {
        ModuleId(value.0)
    }
}

impl std::fmt::Debug for ModuleId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "mod({})", self.0)
    }
}

/// Uniquely identifies a source file within a module.
#[derive(serde::Serialize, Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct ModuleFileId(pub ModuleId, pub u64);

impl ModuleFileId {
    pub fn empty() -> Self {
        Self(ModuleId::empty(), 0)
    }

    /// Creates a new [`ModuleFileId`] from a string, by taking it's hash value.
    pub fn from(module: ModuleId, value: String) -> ModuleFileId {
        ModuleFileId(module, hash_id(value.as_bytes()))
    }
}

#[derive(serde::Serialize, Default, Debug, Clone, PartialEq)]
pub struct SourceMap {
    /// Defines all the source files which are part of the module.
    pub mapping: IndexMap<ModuleFileId, NamedSource>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(serde::Serialize, Default, Debug)]
pub struct State {
    pub source_map: SourceMap,
    types: TypeDatabaseContext,
}

impl State {
    /// Gets the source of the module file with the given ID.
    pub fn source_of(&self, id: ModuleFileId) -> Result<&NamedSource> {
        self.source_map
            .mapping
            .get(&id)
            .ok_or(errors::MissingSourceFile { id }.into())
    }

    /// Retrieves the type context from the build context.
    pub fn tcx(&self) -> &TypeDatabaseContext {
        &self.types
    }

    /// Retrieves the type context from the build context.
    pub fn tcx_mut(&mut self) -> &mut TypeDatabaseContext {
        &mut self.types
    }
}
