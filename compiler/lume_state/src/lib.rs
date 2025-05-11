use std::sync::Arc;

use error_snippet::Result;
use lume_span::{SourceFile, SourceFileId, SourceMap};
use lume_types::TypeDatabaseContext;

mod errors;

#[derive(Default, Debug)]
pub struct State {
    /// Defines the global source map for all source files.
    pub source_map: SourceMap,

    types: TypeDatabaseContext,
}

impl State {
    /// Gets the source of the module file with the given ID.
    pub fn source_of(&self, id: SourceFileId) -> Result<Arc<SourceFile>> {
        self.source_map.get(id).ok_or(errors::MissingSourceFile { id }.into())
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
