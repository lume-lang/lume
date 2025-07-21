use lume_tir::TypeParameter;

use crate::LowerFunction;

impl LowerFunction<'_> {
    pub(crate) fn type_parameters(&mut self, type_params: &[lume_hir::TypeParameterId]) -> lume_tir::TypeParameters {
        let type_params = type_params
            .iter()
            .map(|id| {
                let variable_id = self.mark_variable(lume_tir::VariableSource::Parameter);
                let type_param = self.lower.tcx.db().type_parameter(*id).unwrap();

                TypeParameter {
                    var: variable_id,
                    name: type_param.name.clone(),
                    constraints: type_param.constraints.clone(),
                }
            })
            .collect();

        lume_tir::TypeParameters { inner: type_params }
    }
}
