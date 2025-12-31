use std::fmt::Debug;

use error_snippet::SimpleDiagnostic;
use indexmap::{IndexMap, IndexSet};
use lume_errors::Result;
use serde::{Deserialize, Serialize};

use crate::*;

#[derive(Serialize, Deserialize, Default, Clone, PartialEq)]
pub struct Map {
    /// Defines which package this map belongs to.
    pub package: PackageId,

    /// Defines all the items within the module.
    pub nodes: IndexMap<NodeId, Node>,

    /// Defines all the named types within the module.
    pub types: IndexMap<TypeId, Type>,

    /// Defines all the defined language items within the HIR.
    pub lang_items: lang::LanguageItems,

    /// Defines all the imported paths within the HIR map.
    #[serde(skip)]
    pub imports: IndexSet<Path>,
}

impl Map {
    /// Creates a new HIR map, without any content.
    pub fn empty(package: PackageId) -> Self {
        Self {
            package,
            nodes: IndexMap::new(),
            types: IndexMap::new(),
            lang_items: lang::LanguageItems::new(),
            imports: IndexSet::new(),
        }
    }

    /// Gets all the nodes within the HIR map.
    pub fn nodes(&self) -> &IndexMap<NodeId, Node> {
        &self.nodes
    }

    /// Gets the node with the given ID.
    pub fn node(&self, id: NodeId) -> Option<&Node> {
        self.nodes.get(&id)
    }

    /// Gets the node with the given ID.
    pub fn node_mut(&mut self, id: NodeId) -> Option<&mut Node> {
        self.nodes.get_mut(&id)
    }

    /// Gets the node with the given ID.
    ///
    /// # Errors
    ///
    /// Returns `Err` if no `Node` with the given ID was found in the map.
    pub fn expect_node(&self, id: NodeId) -> Result<&Node> {
        self.node(id)
            .ok_or_else(|| SimpleDiagnostic::new(format!("expected node with ID {id:?}, found none")).into())
    }

    /// Gets the type with the given ID.
    pub fn type_(&self, id: TypeId) -> Option<&Type> {
        self.types.get(&id)
    }

    /// Gets the type with the given ID.
    ///
    /// # Errors
    ///
    /// Returns `Err` if no `Type` with the given ID was found in the map.
    pub fn expect_type(&self, id: TypeId) -> Result<&Type> {
        self.type_(id)
            .ok_or_else(|| SimpleDiagnostic::new(format!("expected type with ID {id:?}, found none")).into())
    }

    /// Gets the type definition with the given ID.
    pub fn type_definition(&self, id: NodeId) -> Option<&TypeDefinition> {
        if let Node::Type(type_def) = self.node(id)? {
            Some(type_def)
        } else {
            None
        }
    }

    /// Gets the type definition with the given ID.
    ///
    /// # Errors
    ///
    /// Returns `Err` if no `TypeDefinition` with the given ID was found in the
    /// map.
    pub fn expect_type_definition(&self, id: NodeId) -> Result<&TypeDefinition> {
        self.type_definition(id)
            .ok_or_else(|| SimpleDiagnostic::new(format!("expected type definition with ID {id:?}, found none")).into())
    }

    /// Gets the type parameter with the given ID.
    pub fn type_parameter(&self, id: NodeId) -> Option<&TypeParameter> {
        if let TypeDefinition::TypeParameter(type_param) = self.type_definition(id)? {
            Some(type_param)
        } else {
            None
        }
    }

    /// Gets the type parameter with the given ID.
    ///
    /// # Errors
    ///
    /// Returns `Err` if no `TypeParameter` with the given ID was found in the
    /// map.
    pub fn expect_type_parameter(&self, id: NodeId) -> Result<&TypeParameter> {
        self.type_parameter(id)
            .ok_or_else(|| SimpleDiagnostic::new(format!("expected type parameter with ID {id:?}, found none")).into())
    }

    /// Gets all the statements within the HIR map.
    pub fn statements(&self) -> impl Iterator<Item = &Statement> {
        self.nodes.values().filter_map(|node| {
            if let Node::Statement(stmt) = node {
                Some(stmt)
            } else {
                None
            }
        })
    }

    /// Gets all the statements within the HIR map.
    pub fn statements_mut(&mut self) -> impl Iterator<Item = &mut Statement> {
        self.nodes.values_mut().filter_map(|node| {
            if let Node::Statement(stmt) = node {
                Some(stmt)
            } else {
                None
            }
        })
    }

    /// Gets the statement with the given ID.
    pub fn statement(&self, id: NodeId) -> Option<&Statement> {
        if let Node::Statement(stmt) = self.node(id)? {
            Some(stmt)
        } else {
            None
        }
    }

    /// Gets the statement with the given ID.
    pub fn statement_mut(&mut self, id: NodeId) -> Option<&mut Statement> {
        if let Node::Statement(stmt) = self.node_mut(id)? {
            Some(stmt)
        } else {
            None
        }
    }

    /// Gets the statement with the given ID.
    ///
    /// # Errors
    ///
    /// Returns `Err` if no `Statement` with the given ID was found in the map.
    pub fn expect_statement(&self, id: NodeId) -> Result<&Statement> {
        self.statement(id)
            .ok_or_else(|| SimpleDiagnostic::new(format!("expected statement with ID {id:?}, found none")).into())
    }

    /// Gets the expression with the given ID.
    pub fn expression(&self, id: NodeId) -> Option<&Expression> {
        if let Node::Expression(expr) = self.node(id)? {
            Some(expr)
        } else {
            None
        }
    }

    /// Gets the expression with the given ID.
    pub fn expression_mut(&mut self, id: NodeId) -> Option<&mut Expression> {
        if let Node::Expression(expr) = self.node_mut(id)? {
            Some(expr)
        } else {
            None
        }
    }

    /// Gets the expression with the given ID.
    ///
    /// # Errors
    ///
    /// Returns `Err` if no `Expression` with the given ID was found in the map.
    pub fn expect_expression(&self, id: NodeId) -> Result<&Expression> {
        self.expression(id)
            .ok_or_else(|| SimpleDiagnostic::new(format!("expected expression with ID {id:?}, found none")).into())
    }

    /// Gets the expressions with the given IDs.
    ///
    /// # Errors
    ///
    /// Returns `Err` if one-or-more IDs have no associated `Expression` within
    /// the map.
    pub fn expect_expressions(&self, id: &[NodeId]) -> Result<Vec<&Expression>> {
        id.iter()
            .map(|id| self.expect_expression(*id))
            .collect::<Result<Vec<_>>>()
    }

    /// Gets all the expressions within the HIR map.
    pub fn expressions(&self) -> impl Iterator<Item = &Expression> {
        self.nodes.values().filter_map(|node| {
            if let Node::Expression(expr) = node {
                Some(expr)
            } else {
                None
            }
        })
    }

    /// Gets all the expressions within the HIR map.
    pub fn expressions_mut(&mut self) -> impl Iterator<Item = &mut Expression> {
        self.nodes.values_mut().filter_map(|node| {
            if let Node::Expression(expr) = node {
                Some(expr)
            } else {
                None
            }
        })
    }

    /// Determines whether the given name has been imported.
    pub fn get_imported(&self, name: &Path) -> Option<&Path> {
        self.imports.iter().find(|i| i.is_name_match(name))
    }

    /// Determines whether the given node is part of the current package.
    pub fn is_local_node(&self, node: NodeId) -> bool {
        node.package == self.package
    }
}

impl Debug for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_fmt(f)
    }
}
