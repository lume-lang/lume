use error_snippet::Result;

use crate::{self as hir, lower::LowerModule};
use lume_ast::{self as ast, Node};

impl LowerModule<'_> {
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
                    name,
                    type_id: None,
                    type_param_id: None,
                    constraints,
                    location,
                })
            })
            .collect::<Result<Vec<_>>>()
    }

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
