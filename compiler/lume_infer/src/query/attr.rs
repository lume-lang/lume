use lume_architect::cached_query;
use lume_hir::{Node, TypeDefinition};
use lume_span::NodeId;

use crate::TyInferCtx;

impl TyInferCtx {
    /// Gets an iterator of all HIR attributes of the given node.
    pub fn hir_attr_of(&self, id: NodeId) -> Option<impl Iterator<Item = &lume_hir::Attribute>> {
        match self.hir_node(id)? {
            Node::Function(node) => Some(node.attrs.iter()),
            Node::Type(node) => match node {
                TypeDefinition::Struct(node) => Some(node.attrs.iter()),
                TypeDefinition::Enum(node) => Some(node.attrs.iter()),
                TypeDefinition::Trait(node) => Some(node.attrs.iter()),
                TypeDefinition::TypeParameter(_) => None,
            },
            Node::TraitImpl(node) => Some(node.attrs.iter()),
            Node::Impl(node) => Some(node.attrs.iter()),
            Node::Field(node) => Some(node.attrs.iter()),
            Node::Method(node) => Some(node.attrs.iter()),
            Node::TraitMethodDef(node) => Some(node.attrs.iter()),
            Node::TraitMethodImpl(node) => Some(node.attrs.iter()),
            Node::Parameter(_)
            | Node::Pattern(_)
            | Node::Statement(_)
            | Node::Expression(_)
            | Node::TypeVariable(_) => None,
        }
    }

    /// Returns `true` if the given node is an intrinsic HIR method.
    #[cached_query]
    #[tracing::instrument(level = "TRACE", skip_all, fields(?id), ret)]
    pub fn hir_is_instrinsic(&self, id: NodeId) -> bool {
        self.hir_attr_of(id)
            .is_some_and(|mut attrs| attrs.any(|attr| attr.name.as_str() == "intrinsic"))
    }
}
