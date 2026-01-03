use lume_tir::TypeParameter;
use lume_types::TypeRef;

use crate::LowerFunction;

impl LowerFunction<'_> {
    pub(crate) fn type_parameters(&mut self, type_params: &[lume_span::NodeId]) -> Vec<lume_tir::TypeParameter> {
        type_params
            .iter()
            .copied()
            .map(|id| {
                let type_ref = TypeRef::new(id, self.lower.tcx.hir_span_of_node(id));
                let variable_id = self.mark_variable(lume_tir::VariableSource::Parameter);
                let type_param = self.lower.tcx.hir_expect_type_parameter(id);
                let constraints = self
                    .lower
                    .tcx
                    .mk_type_refs_from(&type_param.constraints, type_param.id)
                    .unwrap();

                TypeParameter {
                    id,
                    var: variable_id,
                    name: type_param.name.to_string(),
                    type_ref,
                    constraints,
                }
            })
            .collect()
    }
}
