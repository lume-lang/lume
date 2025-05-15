use std::sync::Arc;

use error_snippet::Result;
use lume_errors::DiagCtx;
use lume_span::{SourceFile, SourceFileId, SourceMap};
use lume_types::TypeDatabaseContext;

mod errors;

pub struct State {
    /// Defines the global source map for all source files.
    pub source_map: SourceMap,

    /// Defines the current built type context from the build context.
    types: TypeDatabaseContext,

    /// Defines the diagnostics context from the build context.
    dcx: DiagCtx,
}

impl State {
    /// Creates a new [`State`] instance.
    pub fn new(dcx: DiagCtx) -> State {
        Self {
            dcx,
            source_map: SourceMap::default(),
            types: TypeDatabaseContext::default(),
        }
    }

    /// Gets the source of the module file with the given ID.
    pub fn source_of(&self, id: SourceFileId) -> Result<Arc<SourceFile>> {
        self.source_map.get(id).ok_or(errors::MissingSourceFile { id }.into())
    }

    /// Retrieves the diagnostics context from the build context.
    pub fn dcx(&self) -> &DiagCtx {
        &self.dcx
    }

    /// Retrieves the diagnostics context from the build context.
    pub fn dcx_mut(&mut self) -> &mut DiagCtx {
        &mut self.dcx
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
