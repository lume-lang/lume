use lume_diag::Result;
use lume_types::TypeRef;

use crate::ThirBuildCtx;

pub(crate) mod errors;

pub trait TypeCheckerPass<'a> {
    fn run(tcx: &'a mut ThirBuildCtx, hir: &'a lume_hir::map::Map) -> Result<()>
    where
        Self: Sized;
}

impl ThirBuildCtx<'_> {
    pub fn typecheck(&mut self, _hir: &lume_hir::map::Map) -> Result<()> {
        Ok(())
    }

    /// Checks whether the given type references are compatible.
    pub(crate) fn check_type_compatibility(&self, _from: &TypeRef, _to: &TypeRef) -> Result<()> {
        Ok(())
    }
}
