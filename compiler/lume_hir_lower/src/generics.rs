use crate::*;

impl LoweringContext<'_> {
    pub(crate) fn type_parameters(&mut self, ast_params: Vec<lume_ast::TypeParameter>) -> Result<Vec<NodeId>> {
        let mut hir_params = Vec::with_capacity(ast_params.len());

        for ast_param in ast_params {
            let id = self.next_node_id();
            let location = self.location(ast_param.name.location.clone());
            let name = self.identifier(ast_param.name);

            let mut constraints = Vec::with_capacity(ast_param.constraints.len());
            for constraint in ast_param.constraints {
                constraints.push(self.hir_type(*constraint)?);
            }

            self.current_type_params
                .define(name.name.clone(), lume_hir::TypeId::from(id));

            self.map.nodes.insert(
                id,
                lume_hir::Node::Type(lume_hir::TypeDefinition::TypeParameter(Box::new(
                    lume_hir::TypeParameter {
                        id,
                        name,
                        constraints,
                        location,
                    },
                ))),
            );

            hir_params.push(id);
        }

        let named_type_params = hir_params
            .iter()
            .map(|&id| self.map.expect_type_parameter(id))
            .collect::<Result<Vec<_>>>()?;

        self.ensure_unique_series(&named_type_params, |duplicate, existing| {
            crate::errors::DuplicateTypeParameter {
                duplicate_range: duplicate.name.location,
                original_range: existing.name.location,
                name: existing.name.to_string(),
            }
            .into()
        })?;

        Ok(hir_params)
    }
}
