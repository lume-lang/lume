use lume_diag::Result;

use crate::ThirBuildCtx;

impl ThirBuildCtx {
    pub fn typecheck(&self) -> Result<()> {
        for (id, _) in self.hir().expressions() {
            let _resolved_type = self.tcx().type_of_expr(*id);
        }

        Ok(())
    }
}
