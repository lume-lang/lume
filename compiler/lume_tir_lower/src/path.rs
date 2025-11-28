use error_snippet::Result;

use crate::LowerFunction;

impl LowerFunction<'_> {
    pub(crate) fn path(
        &self,
        path: &lume_hir::Path,
        type_params: &[&lume_hir::TypeParameter],
    ) -> Result<lume_tir::Path> {
        let root = path
            .root
            .iter()
            .map(|r| self.path_segment(r, type_params))
            .collect::<Result<Vec<_>>>()?;

        let name = self.path_segment(&path.name, type_params)?;

        Ok(lume_tir::Path { root, name })
    }

    pub(crate) fn path_hir(&self, path: &lume_hir::Path, hir_id: lume_span::NodeId) -> Result<lume_tir::Path> {
        let type_params = self.lower.tcx.available_type_params_at(hir_id);
        let type_params = self.lower.tcx.as_type_params(&type_params)?;

        self.path(path, &type_params)
    }

    fn path_segment(
        &self,
        path: &lume_hir::PathSegment,
        type_params: &[&lume_hir::TypeParameter],
    ) -> Result<lume_tir::PathSegment> {
        match path {
            lume_hir::PathSegment::Namespace { name } => {
                Ok(lume_tir::PathSegment::Namespace { name: name.to_string() })
            }
            lume_hir::PathSegment::Type { name, bound_types, .. } => {
                let type_arguments = self.lower.tcx.mk_type_refs_generic(bound_types, type_params)?;

                Ok(lume_tir::PathSegment::Type {
                    name: name.to_string(),
                    type_arguments,
                })
            }
            lume_hir::PathSegment::Callable { name, bound_types, .. } => {
                let type_arguments = self.lower.tcx.mk_type_refs_generic(bound_types, type_params)?;

                Ok(lume_tir::PathSegment::Callable {
                    name: name.to_string(),
                    type_arguments,
                })
            }
            lume_hir::PathSegment::Variant { name, .. } => {
                Ok(lume_tir::PathSegment::Variant { name: name.to_string() })
            }
        }
    }
}
