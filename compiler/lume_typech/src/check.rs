use lume_diag::Result;
use lume_types::TypeRef;

use crate::ThirBuildCtx;
use stmt::VarDeclTypeChecker;

pub(crate) mod errors;
mod stmt;

pub trait TypeCheckerPass<'a> {
    fn run(hir: &'a lume_hir::map::Map, tcx: &'a mut ThirBuildCtx) -> Result<()>
    where
        Self: Sized;
}

impl ThirBuildCtx {
    pub fn typecheck(&mut self, hir: &lume_hir::map::Map) -> Result<()> {
        VarDeclTypeChecker::run(hir, self)?;

        Ok(())
    }

    /// Checks whether the given type references are compatible.
    pub(crate) fn check_type_compatibility(&self, _from: &TypeRef, _to: &TypeRef) -> Result<()> {
        Ok(())
    }
}
