use std::fmt::Debug;

use error_snippet::Result;

use crate::LowerFunction;

impl LowerFunction<'_> {
    pub(crate) fn path(&self, path: &lume_hir::Path) -> Result<lume_tir::Path> {
        let params: &[&lume_hir::TypeParameter] = &[];

        self.path_generic(path, params)
    }

    pub(crate) fn path_generic<T: AsRef<lume_hir::TypeParameter> + Debug>(
        &self,
        path: &lume_hir::Path,
        type_params: &[T],
    ) -> Result<lume_tir::Path> {
        let root = path
            .root
            .iter()
            .map(|r| self.path_segment(r, type_params))
            .collect::<Result<Vec<_>>>()?;

        let name = self.path_segment(&path.name, type_params)?;

        Ok(lume_tir::Path { root, name })
    }

    fn path_segment<T: AsRef<lume_hir::TypeParameter> + Debug>(
        &self,
        path: &lume_hir::PathSegment,
        type_params: &[T],
    ) -> Result<lume_tir::PathSegment> {
        match path {
            lume_hir::PathSegment::Namespace { name } => {
                Ok(lume_tir::PathSegment::Namespace { name: name.to_string() })
            }
            lume_hir::PathSegment::Type {
                name, type_arguments, ..
            } => {
                let type_arguments = self.lower.tcx.mk_type_refs_generic(type_arguments, type_params)?;

                Ok(lume_tir::PathSegment::Type {
                    name: name.to_string(),
                    type_arguments,
                })
            }
            lume_hir::PathSegment::Callable {
                name, type_arguments, ..
            } => {
                let type_arguments = self.lower.tcx.mk_type_refs_generic(type_arguments, type_params)?;

                Ok(lume_tir::PathSegment::Callable {
                    name: name.to_string(),
                    type_arguments,
                })
            }
        }
    }
}
