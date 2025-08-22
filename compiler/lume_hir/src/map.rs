use std::fmt::Debug;

use error_snippet::SimpleDiagnostic;
use indexmap::{IndexMap, IndexSet};
use lume_errors::Result;

use crate::*;
use lume_span::{ExpressionId, ItemId, PatternId, StatementId};

#[derive(Default, Clone, PartialEq)]
pub struct Map {
    /// Defines which package this map belongs to.
    pub package: PackageId,

    /// Defines all the items within the module.
    pub items: IndexMap<ItemId, Item>,

    /// Defines all the local statements within the current scope.
    pub statements: IndexMap<StatementId, Statement>,

    /// Defines all the local expressions within the current scope.
    pub expressions: IndexMap<ExpressionId, Expression>,

    /// Defines all the local patterns within the current scope.
    pub patterns: IndexMap<PatternId, Pattern>,

    /// Defines all the imported paths within the HIR map.
    pub imports: IndexSet<Path>,
}

impl Map {
    /// Creates a new HIR map, without any content.
    pub fn empty(package: PackageId) -> Self {
        Self {
            package,
            items: IndexMap::new(),
            statements: IndexMap::new(),
            expressions: IndexMap::new(),
            patterns: IndexMap::new(),
            imports: IndexSet::new(),
        }
    }

    /// Gets all the items within the HIR map.
    pub fn items(&self) -> &IndexMap<ItemId, Item> {
        &self.items
    }

    /// Gets all the statements within the HIR map.
    pub fn statements(&self) -> &IndexMap<StatementId, Statement> {
        &self.statements
    }

    /// Gets the statement with the given ID.
    pub fn statement(&self, id: StatementId) -> Option<&Statement> {
        self.statements.get(&id)
    }

    /// Gets the statement with the given ID.
    pub fn statement_mut(&mut self, id: StatementId) -> Option<&mut Statement> {
        self.statements.get_mut(&id)
    }

    /// Gets the statement with the given ID.
    ///
    /// # Errors
    ///
    /// Returns `Err` if no `Statement` with the given ID was found in the map.
    pub fn expect_statement(&self, id: StatementId) -> Result<&Statement> {
        self.statement(id)
            .ok_or_else(|| SimpleDiagnostic::new(format!("expected statement with ID {id:?}, found none")).into())
    }

    /// Gets the expression with the given ID.
    pub fn expression(&self, id: ExpressionId) -> Option<&Expression> {
        self.expressions.get(&id)
    }

    /// Gets the expression with the given ID.
    pub fn expression_mut(&mut self, id: ExpressionId) -> Option<&mut Expression> {
        self.expressions.get_mut(&id)
    }

    /// Gets the expression with the given ID.
    ///
    /// # Errors
    ///
    /// Returns `Err` if no `Expression` with the given ID was found in the map.
    pub fn expect_expression(&self, id: ExpressionId) -> Result<&Expression> {
        self.expression(id)
            .ok_or_else(|| SimpleDiagnostic::new(format!("expected expression with ID {id:?}, found none")).into())
    }

    /// Gets the expressions with the given IDs.
    ///
    /// # Errors
    ///
    /// Returns `Err` if one-or-more IDs have no associated `Expression` within the map.
    pub fn expect_expressions(&self, id: &[ExpressionId]) -> Result<Vec<&Expression>> {
        id.iter()
            .map(|id| self.expect_expression(*id))
            .collect::<Result<Vec<_>>>()
    }

    /// Gets all the expressions within the HIR map.
    pub fn expressions(&self) -> &IndexMap<ExpressionId, Expression> {
        &self.expressions
    }

    /// Gets all the expressions within the HIR map.
    pub fn expressions_mut(&mut self) -> &mut IndexMap<ExpressionId, Expression> {
        &mut self.expressions
    }

    /// Determines whether the given name has been imported.
    pub fn get_imported(&self, name: &Path) -> Option<&Path> {
        self.imports.iter().find(|i| i.is_name_match(name))
    }
}

impl Debug for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_fmt(f)
    }
}
