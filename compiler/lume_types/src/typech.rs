use lume_diag::Result;

use crate::{ThirBuildCtx, TypeDatabaseContext};
use stmt::VarDeclTypeChecker;

pub(crate) mod errors;
mod stmt;

pub(self) trait TypeCheckerPass<'a> {
    fn run(hir: &'a lume_hir::map::Map, tcx: &'a mut TypeDatabaseContext) -> Result<()>
    where
        Self: Sized;
}

impl ThirBuildCtx {
    pub fn typecheck(&mut self, hir: &lume_hir::map::Map) -> Result<()> {
        VarDeclTypeChecker::run(hir, self.tcx_mut())?;

        Ok(())
    }
}
