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

    /// Returns a reference to the attribute with the given name, on the given
    /// HIR node.
    #[tracing::instrument(level = "TRACE", skip_all, fields(?id))]
    pub fn hir_find_attr<'tcx>(&'tcx self, id: NodeId, name: &str) -> Option<&'tcx lume_hir::Attribute> {
        self.hir_attr_of(id)?.find(|attr| attr.name.as_str() == name)
    }

    /// Returns a reference to the attribute argument value with the given name,
    /// on the given HIR node.
    #[tracing::instrument(level = "TRACE", skip_all, fields(?id))]
    pub fn hir_find_attr_arg<'tcx>(
        &'tcx self,
        id: NodeId,
        attr_name: &str,
        arg_name: &str,
    ) -> Option<&'tcx lume_hir::Literal> {
        self.hir_find_attr(id, attr_name)?
            .arguments
            .iter()
            .find_map(|arg| (arg.name.as_str() == arg_name).then_some(&arg.value))
    }

    /// Returns `true` if the given node is an intrinsic HIR method.
    #[cached_query]
    #[tracing::instrument(level = "TRACE", skip_all, fields(?id), ret)]
    pub fn hir_is_instrinsic(&self, id: NodeId) -> bool {
        self.hir_find_attr(id, "intrinsic").is_some()
    }

    /// Returns the export name given by the method with the given ID, inside a
    /// `![external]` attribute.
    #[tracing::instrument(level = "TRACE", skip_all, fields(?id), ret)]
    pub fn hir_external_name<'tcx>(&'tcx self, id: NodeId) -> Option<&'tcx str> {
        self.hir_find_attr_arg(id, "external", "name")
            .and_then(|lit| match &lit.kind {
                lume_hir::LiteralKind::String(str) => Some(str.value.as_str()),
                _ => None,
            })
    }
}
