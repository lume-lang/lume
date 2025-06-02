use error_snippet::Result;
use lume_hir::{self};

use crate::ThirBuildCtx;

pub(super) fn define_trait_impl(ctx: &mut ThirBuildCtx) -> Result<()> {
    let mut hir = std::mem::take(&mut ctx.hir);

    for (_, symbol) in &mut hir.items {
        if let lume_hir::Item::Use(trait_impl) = symbol {
            let trait_ref = ctx.mk_type_ref(trait_impl.name.as_ref())?;
            let target_ref = ctx.mk_type_ref(trait_impl.target.as_ref())?;

            trait_impl.use_id = Some(ctx.tcx_mut().use_alloc(trait_ref, target_ref));
        }
    }

    ctx.hir = hir;

    Ok(())
}
