use std::collections::HashSet;

use error_snippet::Result;

use crate::LowerModule;

use lume_ast::{self as ast};
use lume_hir::{self as hir};

impl LowerModule<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(crate) fn type_parameters(&mut self, params: Vec<ast::TypeParameter>) -> Result<hir::TypeParameters> {
        let mut names: HashSet<lume_hir::Identifier> = HashSet::with_capacity(params.len());
        let mut type_params = Vec::with_capacity(params.len());

        for param in params {
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

            type_params.push(hir::TypeParameter {
                type_id: None,
                type_param_id: None,
                name,
                constraints,
                location,
            });
        }

        Ok(hir::TypeParameters { inner: type_params })
    }
}
