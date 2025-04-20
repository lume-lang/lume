use indexmap::IndexMap;

use crate::hir::*;

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Map {
    /// Defines which module this map belongs to.
    pub(crate) module: ModuleId,

    /// Defines all the top-level items within the module.
    pub(crate) items: IndexMap<ItemId, Symbol>,

    /// Defines all the local statements within the current scope.
    pub(crate) statements: IndexMap<StatementId, Statement>,

    /// Defines all the local expressions within the current scope.
    pub(crate) expressions: IndexMap<ExpressionId, Expression>,
}

#[allow(dead_code)]
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

    /// Gets the item with the given ItemID within the map.
    pub(crate) fn item(&self, item: ItemId) -> &Symbol {
        match self.items.get(&item) {
            Some(item) => item,
            _ => panic!("Could not find item within type map: {:?}", item),
        }
    }

    /// Gets the type definition with the given ItemID within the map.
    pub(crate) fn item_type(&self, item: ItemId) -> &TypeDefinition {
        match self.item(item) {
            Symbol::Type(def) => def,
            k => bug!("Expected type definition, found {:?} instead: {:?}", k, item),
        }
    }

    /// Gets the location of the given item.
    pub(crate) fn item_span(&self, item: ItemId) -> Location {
        *self.item(item).location()
    }
}
