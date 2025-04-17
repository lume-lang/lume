use std::collections::HashMap;

use crate::hir::*;

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Map {
    /// Defines which source file this map belongs to.
    pub(crate) file: ModuleFileId,

    /// Defines all the top-level items within the module.
    pub(crate) items: HashMap<ItemId, Symbol>,

    /// Defines all the local statements within the current scope.
    pub(crate) statements: HashMap<NodeId, Statement>,

    /// Defines all the local expressions within the current scope.
    pub(crate) expressions: HashMap<NodeId, Expression>,
}

#[allow(dead_code)]
impl Map {
    /// Creates a new HIR map, without any content.
    pub fn empty(file: ModuleFileId) -> Self {
        Self {
            file,
            items: HashMap::new(),
            statements: HashMap::new(),
            expressions: HashMap::new(),
        }
    }

    /// Gets the item with the given ItemID within the map.
    pub(crate) fn item(&self, item: ItemId) -> &Symbol {
        match self.items.get(&item) {
            Some(item) => item,
            _ => panic!("Could not find item within type map: {:?}", item),
        }
    }

    /// Gets the location of the given item.
    pub(crate) fn item_span(&self, item: ItemId) -> Location {
        *self.item(item).location()
    }

    /// Gets the location for the identifier of the given item.
    pub(crate) fn item_ident_span(&self, item: ItemId) -> Location {
        self.item(item).ident().location
    }
}
