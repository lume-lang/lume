use lume_hir::{self};

use crate::ThirBuildCtx;

pub(super) fn define_impl(ctx: &mut ThirBuildCtx) {
    let mut hir = std::mem::take(&mut ctx.hir);

    for (_, symbol) in &mut hir.items {
        if let lume_hir::Symbol::Impl(implementation) = symbol {
            let target = implementation.target.name.clone();
            let impl_id = ctx.tcx_mut().impl_alloc(target);

            implementation.impl_id = Some(impl_id);
        }
    }

    ctx.hir = hir;
}
