use crate::*;

impl LoweringContext<'_> {
    pub(crate) fn type_parameters<I>(&mut self, bound_types: I) -> Vec<NodeId>
    where
        I: IntoIterator<Item = lume_ast::BoundType>,
    {
        let mut hir_params = Vec::new();

        for ast_param in bound_types {
            let id = self.next_node_id();
            let location = self.location(ast_param.location());
            let name = self.ident_opt(ast_param.name());

            let mut constraints = Vec::new();

            if let Some(constraint_list) = ast_param.constraints() {
                for constraint in constraint_list.ty() {
                    constraints.push(self.hir_type(constraint));
                }
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
            .filter_map(|&id| match self.map.expect_type_parameter(id) {
                Ok(id) => Some(id),
                Err(err) => {
                    self.dcx.emit_and_push(err);
                    None
                }
            })
            .collect::<Vec<_>>();

        self.ensure_unique_series(&named_type_params, |duplicate, existing| {
            crate::errors::DuplicateTypeParameter {
                duplicate_range: duplicate.name.location,
                original_range: existing.name.location,
                name: existing.name.to_string(),
            }
            .into()
        });

        hir_params
    }
}
