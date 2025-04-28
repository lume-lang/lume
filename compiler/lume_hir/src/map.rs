use indexmap::IndexMap;

use crate::*;

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Map {
    /// Defines which module this map belongs to.
    pub(crate) module: ModuleId,

    /// Defines all the top-level items within the module.
    pub items: IndexMap<ItemId, Symbol>,

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
            items: IndexMap::new(),
            statements: IndexMap::new(),
            expressions: IndexMap::new(),
        }
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
