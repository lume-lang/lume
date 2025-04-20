use diag::Result;

use crate::thir::ThirBuildCtx;

impl ThirBuildCtx {
    pub(crate) fn typecheck(&self) -> Result<()> {
        for (id, _) in &self.hir().expressions {
            let _resolved_type = self.tcx().type_of_expr(*id);
        }

        Ok(())
    }
}
