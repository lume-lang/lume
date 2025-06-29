use error_snippet::Result;

use crate::LowerModule;

use lume_ast::{self as ast, Node};
use lume_hir::{self as hir};

impl LowerModule<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(crate) fn type_parameters(&self, params: Vec<ast::TypeParameter>) -> Result<hir::TypeParameters> {
        let type_params = params
            .into_iter()
            .map(|param| {
                let location = self.location(param.name.location.clone());
                let name = self.identifier(param.name);

                let constraints = param
                    .constraints
                    .into_iter()
                    .map(|ty| Ok(Box::new(self.type_ref(*ty)?)))
                    .collect::<Result<Vec<_>>>()?;

                Ok(hir::TypeParameter {
                    type_id: None,
                    type_param_id: None,
                    name,
                    constraints,
                    location,
                })
            })
            .collect::<Result<Vec<_>>>()?;

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
