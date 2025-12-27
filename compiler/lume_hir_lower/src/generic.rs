use error_snippet::Result;
use lume_span::NodeId;

use crate::LowerModule;

impl LowerModule {
    #[libftrace::traced(level = Debug)]
    pub(crate) fn type_parameters(&mut self, params: Vec<lume_ast::TypeParameter>) -> Result<Vec<NodeId>> {
        let mut type_params = Vec::with_capacity(params.len());

        self.add_type_param_scope();

        for param in params {
            let id = self.next_node_id();
            let location = self.location(param.name.location.clone());
            let name = self.identifier(param.name);

            let mut constraints = Vec::with_capacity(param.constraints.len());
            for constraint in param.constraints {
                constraints.push(self.type_ref(*constraint)?);
            }

            let id = self.add_type_param(lume_hir::TypeParameter {
                id,
                name,
                constraints,
                location,
            });

            type_params.push(id);
        }

        let named_type_params = type_params
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

        Ok(type_params)
    }
}
