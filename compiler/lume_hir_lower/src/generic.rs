use error_snippet::Result;

use crate::LowerModule;

use lume_ast::{self as ast, Node};
use lume_hir::{self as hir};

impl LowerModule<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(crate) fn type_parameters(&self, params: Vec<ast::TypeParameter>) -> Result<Vec<hir::TypeParameter>> {
        params
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
            .collect::<Result<Vec<_>>>()
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(crate) fn type_arguments(&self, params: Vec<ast::TypeArgument>) -> Result<Vec<hir::TypeArgument>> {
        params
            .into_iter()
            .map(|param| {
                let location = self.location(param.ty.location().clone());
                let ty = self.type_ref(param.ty)?;

                Ok(hir::TypeArgument::Named { ty, location })
            })
            .collect::<Result<Vec<_>>>()
    }
}
