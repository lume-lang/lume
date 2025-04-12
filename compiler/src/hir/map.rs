use std::collections::HashMap;

use crate::hir::*;

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Map {
    /// Defines all the top-level items within the module.
    pub(crate) items: HashMap<ItemId, Symbol>,

    /// Defines all the local expressions within the current scope.
    pub(crate) expressions: HashMap<LocalId, Expression>,
}

#[allow(dead_code)]
impl Map {
    /// Creates a new HIR map, without any content.
    pub fn empty() -> Self {
        Self {
            items: HashMap::new(),
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
