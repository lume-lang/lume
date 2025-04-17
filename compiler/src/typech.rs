use diag::Result;

use crate::thir::ThirBuildCtx;

impl ThirBuildCtx {
    pub(crate) fn typecheck(&self) -> Result<()> {
        for (_, hir) in &self.hir {
            for (_, _) in &hir.expressions {
                todo!()
            }
        }

        Ok(())
    }
}
