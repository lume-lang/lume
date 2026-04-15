//! This file contains lookup functions for HIR nodes and other utility
//! functions, which are useful when querying from the HIR map directly.
//!
//! Many of the methods in this file, especially the ones named `expect_*`, will
//! panic when pre-conditions are unmet - even on non-debug builds. For example,
//! [`TyInferCtx::hir_expect_field`] will panic if the node with the given ID is
//! not a [`Node::Field`] node. For a non-panicking version, use
//! [`TyInferCtx::hir_field`].

use lume_architect::cached_query;
use lume_errors::Result;
use lume_hir::{Node, Path};
use lume_span::*;

use crate::TyInferCtx;
use crate::query::CallReference;

/// An iterator over the elements of a linked [`NodeId`]s.
///
/// This `struct` is created by [`TyInferCtx::hir_parent_id_iter()`].
pub struct ParentHirIterator<'a> {
    tcx: &'a TyInferCtx,
    current: Option<NodeId>,
}

impl Iterator for ParentHirIterator<'_> {
    type Item = NodeId;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.current;

        self.current = match self.current {
            Some(c) => self.tcx.hir_parent_of(c),
            None => None,
        };

        item
    }
}

impl TyInferCtx {
    pub fn hir_nodes(&self) -> impl Iterator<Item = &Node> {
        self.hir.nodes.values()
    }

    /// Returns an iterator of all HIR nodes which are local to the current HIR
    /// map package.
    pub fn hir_local_nodes(&self) -> impl Iterator<Item = &Node> {
        self.hir_nodes().filter(|node| node.id().package == self.hir.package)
    }

    /// Returns the [`lume_hir::Node`] with the given ID, if any.
    #[tracing::instrument(level = "TRACE", skip_all, fields(id = %id))]
    pub fn hir_node(&self, id: NodeId) -> Option<&Node> {
        self.hir.node(id)
    }

    /// Returns the [`lume_hir::Node`] with the given ID, if any.
    #[tracing::instrument(level = "TRACE", skip_all, fields(id = %id))]
    pub fn hir_expect_node(&self, id: NodeId) -> &Node {
        match self.hir_node(id) {
            Some(item) => item,
            None => panic!("expected HIR node with ID of {id:?}"),
        }
    }

    /// Returns the [`lume_hir::Statement`] with the given ID, if any.
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_stmt(&self, id: NodeId) -> Option<&lume_hir::Statement> {
        self.hir.statement(id)
    }

    /// Returns the [`lume_hir::Statement`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::Statement`] with the given ID was found.
    #[track_caller]
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_expect_stmt(&self, id: NodeId) -> &lume_hir::Statement {
        match self.hir_stmt(id) {
            Some(item) => item,
            None => panic!("expected HIR statement with ID of {id:?}"),
        }
    }

    /// Returns the [`lume_hir::Expression`] with the given ID, if any.
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_expr(&self, id: NodeId) -> Option<&lume_hir::Expression> {
        self.hir.expression(id)
    }

    /// Returns the [`lume_hir::Expression`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::Expression`] with the given ID was found.
    #[track_caller]
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_expect_expr(&self, id: NodeId) -> &lume_hir::Expression {
        match self.hir_expr(id) {
            Some(item) => item,
            None => panic!("expected HIR expression with ID of {id:?}"),
        }
    }

    /// Returns the [`lume_hir::Pattern`] with the given ID, if any.
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_pat(&self, id: NodeId) -> Option<&lume_hir::Pattern> {
        if let lume_hir::Node::Pattern(pattern) = self.hir.nodes.get(&id)? {
            Some(pattern)
        } else {
            None
        }
    }

    /// Returns the [`lume_hir::Pattern`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::Pattern`] with the given ID was found.
    #[track_caller]
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_expect_pattern(&self, id: NodeId) -> &lume_hir::Pattern {
        match self.hir_expect_node(id) {
            lume_hir::Node::Pattern(pat) => pat,
            _ => panic!("expected HIR pattern with ID of {id:?}"),
        }
    }

    /// Returns the [`lume_hir::CallExpression`] with the given ID, if any.
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_call_expr(&self, id: NodeId) -> Option<lume_hir::CallExpression<'_>> {
        match &self.hir_expr(id)?.kind {
            lume_hir::ExpressionKind::InstanceCall(call) => Some(lume_hir::CallExpression::Instanced(call)),
            lume_hir::ExpressionKind::IntrinsicCall(call) => Some(lume_hir::CallExpression::Intrinsic(call)),
            lume_hir::ExpressionKind::StaticCall(call) => Some(lume_hir::CallExpression::Static(call)),
            _ => None,
        }
    }

    /// Returns the [`lume_hir::Field`] with the given ID, if any.
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_field(&self, id: NodeId) -> Option<&lume_hir::Field> {
        match self.hir_node(id)? {
            lume_hir::Node::Field(field) => Some(field),
            _ => None,
        }
    }

    /// Returns the [`lume_hir::Field`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::Field`] with the given ID was found.
    #[track_caller]
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_expect_field(&self, id: NodeId) -> &lume_hir::Field {
        let Some(field) = self.hir_field(id) else {
            panic!("expected HIR field with ID of {id:?}")
        };

        field
    }

    /// Returns the [`lume_hir::TypeDefinition`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::TypeDefinition`] with the given ID was found.
    #[track_caller]
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_expect_type(&self, id: NodeId) -> &lume_hir::TypeDefinition {
        let lume_hir::Node::Type(ty) = self.hir_expect_node(id) else {
            panic!("expected HIR type with ID of {id:?}")
        };

        ty
    }

    /// Returns the [`lume_hir::TypeParameter`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::TypeParameter`] with the given ID was found.
    #[track_caller]
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_expect_type_parameter(&self, id: NodeId) -> &lume_hir::TypeParameter {
        let lume_hir::TypeDefinition::TypeParameter(type_param) = self.hir_expect_type(id) else {
            panic!("expected HIR type parameter with ID of {id:?}")
        };

        type_param
    }

    /// Returns the [`lume_hir::TypeVariable`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::TypeVariable`] with the given ID was found.
    #[track_caller]
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_expect_type_variable(&self, id: NodeId) -> &lume_hir::TypeVariable {
        let lume_hir::Node::TypeVariable(type_var) = self.hir_expect_node(id) else {
            panic!("expected HIR type variable with ID of {id:?}")
        };

        type_var
    }

    /// Returns the [`lume_hir::StructDefinition`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::StructDefinition`] with the given ID was found.
    #[track_caller]
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_expect_struct(&self, id: NodeId) -> &lume_hir::StructDefinition {
        let lume_hir::TypeDefinition::Struct(ty) = self.hir_expect_type(id) else {
            panic!("expected HIR struct with ID of {id:?}")
        };

        ty
    }

    /// Returns the [`lume_hir::EnumDefinition`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::EnumDefinition`] with the given ID was found.
    #[track_caller]
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_expect_enum(&self, id: NodeId) -> &lume_hir::EnumDefinition {
        let lume_hir::TypeDefinition::Enum(ty) = self.hir_expect_type(id) else {
            panic!("expected HIR enum with ID of {id:?}")
        };

        ty
    }

    /// Returns the [`lume_hir::TraitDefinition`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::TraitDefinition`] with the given ID was found.
    #[track_caller]
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_expect_trait(&self, id: NodeId) -> &lume_hir::TraitDefinition {
        let lume_hir::TypeDefinition::Trait(ty) = self.hir_expect_type(id) else {
            panic!("expected HIR trait with ID of {id:?}")
        };

        ty
    }

    /// Returns the parent of the given HIR element, if any is found.
    #[cached_query]
    #[track_caller]
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_parent_of(&self, id: NodeId) -> Option<NodeId> {
        self.ancestry.get(&id).copied()
    }

    /// Returns the parent of the given HIR element, if any is found.
    #[track_caller]
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_parent_node_of(&self, id: NodeId) -> Option<&Node> {
        self.hir_parent_of(id).and_then(|id| self.hir.node(id))
    }

    /// Returns an iterator for the IDs in the ancestry tree above
    /// the given HIR [`NodeId`] node.
    #[track_caller]
    pub fn hir_parent_id_iter(&self, def: NodeId) -> ParentHirIterator<'_> {
        ParentHirIterator {
            tcx: self,
            current: Some(def),
        }
    }

    /// Returns the parent of the given HIR element, if any is found.
    #[track_caller]
    pub fn hir_parent_iter(&self, def: NodeId) -> impl Iterator<Item = &Node> {
        self.hir_parent_id_iter(def).filter_map(move |id| self.hir_node(id))
    }

    /// Attempts to get the callable which contains the given [`NodeId`].
    ///
    /// Otherwise, returns [`None`].
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_parent_callable(&self, id: NodeId) -> Option<CallReference> {
        for parent in self.hir_parent_iter(id) {
            match parent {
                lume_hir::Node::Method(method) => return Some(CallReference::Function(method.id)),
                lume_hir::Node::TraitMethodDef(func) => return Some(CallReference::Function(func.id)),
                lume_hir::Node::TraitMethodImpl(func) => return Some(CallReference::Function(func.id)),
                lume_hir::Node::Function(func) => return Some(CallReference::Function(func.id)),
                _ => {}
            }
        }

        None
    }

    /// Attempts to get the body of the given [`NodeId`], if it contains a body.
    ///
    /// Otherwise, returns [`None`].
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_body_of_node(&self, id: NodeId) -> Option<&lume_hir::Block> {
        match self.hir_node(id)? {
            lume_hir::Node::Method(method) => method.block.as_ref(),
            lume_hir::Node::TraitMethodDef(func) => func.block.as_ref(),
            lume_hir::Node::TraitMethodImpl(func) => func.block.as_ref(),
            lume_hir::Node::Function(func) => func.block.as_ref(),
            ty => panic!("bug!: item type cannot contain a body: {ty:?}"),
        }
    }

    /// Returns the path of the HIR definition with the given ID.
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_path_of_node(&self, def: NodeId) -> Path {
        match self.hir_expect_node(def) {
            Node::Type(type_def) => match type_def {
                lume_hir::TypeDefinition::Struct(def) => def.name().clone(),
                lume_hir::TypeDefinition::Trait(def) => def.name().clone(),
                lume_hir::TypeDefinition::Enum(def) => def.name().clone(),
                lume_hir::TypeDefinition::TypeParameter(def) => match self.hir_parent_of(def.id) {
                    Some(parent) => {
                        let parent_name = self.hir_path_of_node(parent);
                        Path::with_root(parent_name, lume_hir::PathSegment::ty(def.name.clone()))
                    }
                    None => Path::rooted(lume_hir::PathSegment::ty(def.name.clone())),
                },
            },
            Node::Impl(implementation) => implementation.target.name.clone(),
            Node::TraitImpl(trait_impl) => {
                let target_name = trait_impl.target.name.clone();

                Path::with_root(target_name, trait_impl.name.name.name.clone())
            }
            Node::Function(func) => func.path().clone(),
            Node::Method(method) => method.signature.name.clone(),
            Node::TraitMethodDef(method) => method.signature.name.clone(),
            Node::TraitMethodImpl(method) => {
                let parent = self
                    .hir_parent_of(method.id)
                    .expect("expected parent of method definition");

                let parent_name = self.hir_path_of_node(parent);
                Path::with_root(parent_name, method.signature.name.name.clone())
            }
            Node::Field(field) => {
                let parent = self
                    .hir_parent_of(field.id)
                    .expect("expected parent of field definition");

                let parent_name = self.hir_path_of_node(parent);
                Path::with_root(parent_name, lume_hir::PathSegment::callable(field.name.clone()))
            }
            Node::Parameter(parameter) => {
                let parent = self
                    .hir_parent_of(parameter.id)
                    .expect("expected parent of parameter definition");

                let parent_name = self.hir_path_of_node(parent);
                Path::with_root(parent_name, lume_hir::PathSegment::callable(parameter.name.clone()))
            }
            Node::TypeVariable(type_variable) => Path::rooted(lume_hir::PathSegment::ty(format!(
                "T?{:X}",
                type_variable.id.as_usize()
            ))),
            Node::Statement(_) | Node::Expression(_) | Node::Pattern(_) => panic!("bug!: cannot get path name of node"),
        }
    }

    /// Returns the span of the HIR definition with the given ID.
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_span_of_node(&self, def: NodeId) -> Location {
        self.hir_expect_node(def).location()
    }

    /// Determines whether the given node is part of the current package.
    pub fn hir_is_local_node(&self, node: NodeId) -> bool {
        self.hir.is_local_node(node)
    }

    /// Determines whether the given callable is declared as external.
    pub fn hir_is_callable_external(&self, id: NodeId) -> bool {
        match self.hir_expect_node(id) {
            Node::Function(func) => func.block.is_none(),
            Node::Method(method) => method.block.is_none(),
            Node::TraitMethodImpl(method) => method.block.is_none(),
            _ => false,
        }
    }

    /// Returns the type parameter binding of the given type variable.
    #[cached_query]
    #[track_caller]
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_tyvar_binding_of(&self, type_var_id: NodeId) -> Option<lume_hir::TypeId> {
        self.hir().type_variable(type_var_id).map(|type_var| type_var.binding)
    }

    /// Returns the canonical type parameter binding of the given type variable.
    #[cached_query]
    #[track_caller]
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn hir_tyvar_canonical_of(&self, type_var_id: NodeId) -> Option<lume_hir::TypeId> {
        self.hir().type_variable(type_var_id).map(|type_var| type_var.canonical)
    }

    /// Returns the canonical type parameter ID of the given type parameter.
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub fn hir_canonical_type_of(
        &self,
        type_parameter_id: lume_hir::TypeId,
        owner: NodeId,
    ) -> Result<Option<lume_hir::TypeId>> {
        tracing::trace!(
            "{:+} => {:+}",
            self.hir_path_of_node(type_parameter_id.as_node_id()),
            self.hir_path_of_node(owner),
        );

        match self.hir().expect_node(owner)? {
            Node::Type(
                lume_hir::TypeDefinition::Struct(_)
                | lume_hir::TypeDefinition::Enum(_)
                | lume_hir::TypeDefinition::Trait(_),
            )
            | Node::Function(_) => Ok(Some(type_parameter_id)),
            Node::Impl(implementation) => {
                let Some(param_idx) = implementation
                    .target
                    .bound_types()
                    .iter()
                    .position(|ty| ty.id == type_parameter_id)
                else {
                    return Ok(None);
                };

                let target_type = self.mk_type_ref_from(&implementation.target, owner)?;
                let type_params = self.type_params_of(target_type.instance_of)?;

                Ok(type_params.get(param_idx).map(|ty| lume_hir::TypeId::from(*ty)))
            }
            Node::TraitImpl(trait_impl) => {
                if let Some(param_idx) = trait_impl
                    .target
                    .bound_types()
                    .iter()
                    .position(|ty| ty.id == type_parameter_id)
                {
                    let target_type = self.mk_type_ref_from(&trait_impl.target, owner)?;
                    let type_params = self.type_params_of(target_type.instance_of)?;

                    return Ok(type_params.get(param_idx).map(|ty| lume_hir::TypeId::from(*ty)));
                }

                if let Some(param_idx) = trait_impl
                    .name
                    .bound_types()
                    .iter()
                    .position(|ty| ty.id == type_parameter_id)
                {
                    let target_type = self.mk_type_ref_from(&trait_impl.name, owner)?;
                    let type_params = self.type_params_of(target_type.instance_of)?;

                    return Ok(type_params.get(param_idx).map(|ty| lume_hir::TypeId::from(*ty)));
                }

                Ok(None)
            }
            _ => panic!("invalid owner node; expected item"),
        }
    }

    /// Determines whether the given node is a child of an unsafe scope block.
    #[cached_query]
    #[tracing::instrument(level = "TRACE", skip_all)]
    pub fn hir_in_unsafe_block(&self, id: NodeId) -> bool {
        self.hir_parent_iter(id).any(|node| {
            if let lume_hir::Node::Expression(expr) = node
                && let lume_hir::ExpressionKind::Scope(scope_expr) = &expr.kind
            {
                scope_expr.unsafe_
            } else {
                false
            }
        })
    }
}
