use indexmap::IndexMap;
use lume_diag::source::NamedSource;

use crate::*;

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Map {
    /// Defines which module this map belongs to.
    pub(crate) module: ModuleId,

    /// Defines all the source files which are part of the module.
    pub(crate) files: IndexMap<ModuleFileId, NamedSource>,

    /// Defines all the top-level items within the module.
    pub(crate) items: IndexMap<ItemId, Symbol>,

    /// Defines all the local statements within the current scope.
    pub(crate) statements: IndexMap<StatementId, Statement>,

    /// Defines all the local expressions within the current scope.
    pub(crate) expressions: IndexMap<ExpressionId, Expression>,
}

impl Map {
    /// Creates a new HIR map, without any content.
    pub fn empty(module: ModuleId) -> Self {
        Self {
            module,
            files: IndexMap::new(),
            items: IndexMap::new(),
            statements: IndexMap::new(),
            expressions: IndexMap::new(),
        }
    }

    /// Gets all the files within the HIR map.
    pub fn files(&self) -> &IndexMap<ModuleFileId, NamedSource> {
        &self.files
    }

    /// Gets the file within the HIR map with the given ID.
    pub fn file(&self, id: ModuleFileId) -> Option<&NamedSource> {
        self.files.get(&id)
    }

    /// Gets all the items within the HIR map.
    pub fn items(&self) -> &IndexMap<ItemId, Symbol> {
        &self.items
    }

    /// Gets all the statements within the HIR map.
    pub fn statements(&self) -> &IndexMap<StatementId, Statement> {
        &self.statements
    }

    /// Gets all the expressions within the HIR map.
    pub fn expressions(&self) -> &IndexMap<ExpressionId, Expression> {
        &self.expressions
    }
}
