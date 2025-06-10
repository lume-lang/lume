use error_snippet::Result;
use lume_hir::{self};

use crate::ThirBuildCtx;

#[tracing::instrument(level = "DEBUG", name = "define_trait_impl", skip_all, err)]
pub(super) fn define_trait_impl(ctx: &mut ThirBuildCtx) -> Result<()> {
    let mut hir = std::mem::take(&mut ctx.hir);

    for (_, symbol) in &mut hir.items {
        if let lume_hir::Item::Use(trait_impl) = symbol {
            trait_impl.use_id = Some(ctx.tcx_mut().use_alloc());
        }
    }

    ctx.hir = hir;

    Ok(())
}
