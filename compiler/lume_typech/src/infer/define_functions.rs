use lume_hir::{self};

use crate::ThirBuildCtx;

pub(super) struct DefineFunctions;

impl DefineFunctions {
    pub(super) fn run_all(ctx: &mut ThirBuildCtx) {
        let mut hir = std::mem::take(&mut ctx.hir);

        for (_, item) in &mut hir.items {
            if let lume_hir::Item::Function(func) = item {
                let name = func.name.clone();
                let visibility = func.visibility;
                let func_id = ctx.tcx_mut().func_alloc(name, visibility);

                func.func_id = Some(func_id);
            }
        }

        ctx.hir = hir;
    }
}
