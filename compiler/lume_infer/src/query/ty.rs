use lume_errors::Result;
use lume_span::NodeId;

use crate::TyInferCtx;

impl TyInferCtx {
    /// Gets a slice of the type parameters defined on the given node.
    ///
    /// # Errors
    ///
    /// If the given node is missing or cannot hold type parameters, [`Err`] is
    /// returned.
    pub fn type_params_of(&self, id: NodeId) -> Result<&[NodeId]> {
        let Some(node) = self.hir_node(id) else {
            return Ok(&[]);
        };

        match node {
            lume_hir::Node::Type(type_def) => match type_def {
                lume_hir::TypeDefinition::Struct(struct_def) => Ok(&struct_def.type_parameters),
                lume_hir::TypeDefinition::Trait(trait_def) => Ok(&trait_def.type_parameters),
                lume_hir::TypeDefinition::Enum(enum_def) => Ok(&enum_def.type_parameters),
                lume_hir::TypeDefinition::TypeParameter(_) => Ok(&[]),
            },
            lume_hir::Node::Function(func) => Ok(&func.type_parameters),
            lume_hir::Node::Impl(implementation) => Ok(&implementation.type_parameters),
            lume_hir::Node::TraitImpl(trait_impl) => Ok(&trait_impl.type_parameters),
            lume_hir::Node::TraitMethodDef(method_def) => Ok(&method_def.type_parameters),
            lume_hir::Node::TraitMethodImpl(method_impl) => Ok(&method_impl.type_parameters),
            lume_hir::Node::Method(method) => Ok(&method.type_parameters),
            _ => Err(crate::query::diagnostics::CannotHoldTypeParams { id }.into()),
        }
    }

    /// Return the [`lume_hir::TypeParameter`], which correspond to the
    /// given ID, if it refers to a type parameter. Otherwise, returns [`None`].
    pub fn as_type_param(&self, id: NodeId) -> Option<&lume_hir::TypeParameter> {
        if let lume_hir::Node::Type(lume_hir::TypeDefinition::TypeParameter(type_param)) = self.hir_node(id)? {
            Some(type_param.as_ref())
        } else {
            None
        }
    }

    /// Returns a list of [`lume_hir::TypeParameter`], which correspond to the
    /// IDs within the given slice.
    pub fn as_type_params(&self, type_param_ids: &[NodeId]) -> Result<Vec<&lume_hir::TypeParameter>> {
        let mut type_params = Vec::with_capacity(type_param_ids.len());

        for type_param_id in type_param_ids {
            let lume_hir::Node::Type(lume_hir::TypeDefinition::TypeParameter(type_param)) =
                self.hir_expect_node(*type_param_id)
            else {
                panic!("bug!: expected type parameter");
            };

            type_params.push(type_param.as_ref());
        }

        Ok(type_params)
    }

    /// Returns a list of [`lume_hir::TypeParameter`], which correspond to the
    /// IDs within the given slice, which have been converted into instances of
    /// [`lume_hir::Type`].
    pub fn type_params_as_types(&self, type_param_ids: &[NodeId]) -> Result<Vec<lume_hir::Type>> {
        Ok(self
            .as_type_params(type_param_ids)?
            .into_iter()
            .map(|type_param| lume_hir::Type {
                id: lume_hir::TypeId::from(type_param.id),
                name: lume_hir::Path::rooted(lume_hir::PathSegment::ty(type_param.name.clone())),
                location: type_param.location,
            })
            .collect::<Vec<_>>())
    }
}
