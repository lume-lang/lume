use error_snippet::Result;

use crate::LowerModule;

use lume_ast::{self as ast, Node};
use lume_hir::{self as hir};

impl LowerModule<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(crate) fn type_parameters(&self, params: Vec<ast::TypeParameter>) -> Result<hir::TypeParameters> {
        let mut type_params = Vec::with_capacity(params.len());

        for param in params {
            let location = self.location(param.name.location.clone());
            let name = self.identifier(param.name);

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

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(crate) fn type_arguments(&self, params: Vec<ast::Type>) -> Result<hir::TypeArguments> {
        let type_args = params
            .into_iter()
            .map(|param| {
                let location = self.location(param.location().clone());
                let ty = self.type_ref(param)?;

                Ok(hir::TypeArgument::Named { ty, location })
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(hir::TypeArguments { inner: type_args })
    }
}
