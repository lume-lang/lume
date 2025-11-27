use std::collections::HashSet;

use error_snippet::Result;
use lume_span::NodeId;

use crate::LowerModule;

impl LowerModule<'_> {
    #[libftrace::traced(level = Debug)]
    pub(crate) fn type_parameters(&mut self, params: Vec<lume_ast::TypeParameter>) -> Result<Vec<NodeId>> {
        let mut names: HashSet<lume_hir::Identifier> = HashSet::with_capacity(params.len());
        let mut type_params = Vec::with_capacity(params.len());

        self.add_type_param_scope();

        for param in params {
            let id = self.next_node_id();

            let location = self.location(param.name.location.clone());
            let name = self.identifier(param.name);

            if let Some(existing) = names.get(&name) {
                return Err(crate::errors::DuplicateTypeParameter {
                    source: self.file.clone(),
                    duplicate_range: location.index.clone(),
                    original_range: existing.location.index.clone(),
                    name: name.name.clone(),
                }
                .into());
            }

            names.insert(name.clone());

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

        Ok(type_params)
    }
}
