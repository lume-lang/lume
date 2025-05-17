use indexmap::IndexMap;

use crate::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Map {
    /// Defines which package this map belongs to.
    pub package: PackageId,

    /// Defines all the top-level items within the module.
    pub items: IndexMap<ItemId, Symbol>,

    /// Defines all the local statements within the current scope.
    pub statements: IndexMap<StatementId, Statement>,

    /// Defines all the local expressions within the current scope.
    pub expressions: IndexMap<ExpressionId, Expression>,
}

impl Map {
    /// Creates a new HIR map, without any content.
    pub fn empty(package: PackageId) -> Self {
        Self {
            package,
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

    /// Gets the expression with the given ID.
    pub fn expression(&self, id: ExpressionId) -> Option<&Expression> {
        self.expressions.get(&id)
    }

    /// Gets the expression with the given ID.
    pub fn expression_mut(&mut self, id: ExpressionId) -> Option<&mut Expression> {
        self.expressions.get_mut(&id)
    }

    /// Gets all the expressions within the HIR map.
    pub fn expressions(&self) -> &IndexMap<ExpressionId, Expression> {
        &self.expressions
    }

    /// Gets all the expressions within the HIR map.
    pub fn expressions_mut(&mut self) -> &mut IndexMap<ExpressionId, Expression> {
        &mut self.expressions
    }
}
