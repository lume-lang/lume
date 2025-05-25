use error_snippet::Result;
use lume_hir::{self};

use crate::ThirBuildCtx;

pub(super) struct DefineUse<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineUse<'_> {
    pub(super) fn run_all(ctx: &mut ThirBuildCtx, hir: &mut lume_hir::map::Map) -> Result<()> {
        let mut define = DefineUse { ctx };

        for (_, symbol) in &mut hir.items {
            if let lume_hir::Symbol::Use(u) = symbol {
                define.define_use(u)?;
            }
        }

        Ok(())
    }

    fn define_use(&mut self, trait_impl: &mut lume_hir::TraitImplementation) -> Result<()> {
        let trait_ref = self.ctx.mk_type_ref(trait_impl.name.as_ref())?;
        let target_ref = self.ctx.mk_type_ref(trait_impl.target.as_ref())?;

        trait_impl.use_id = Some(self.ctx.tcx_mut().use_alloc(trait_ref, target_ref));

        Ok(())
    }
}
