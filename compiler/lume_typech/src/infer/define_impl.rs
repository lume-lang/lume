use error_snippet::Result;
use lume_hir::{self};
use lume_types::Method;

use crate::ThirBuildCtx;

pub(super) struct DefineImpl<'a, 'b> {
    ctx: &'a mut ThirBuildCtx<'b>,
}

impl DefineImpl<'_, '_> {
    pub(super) fn run_all(ctx: &mut ThirBuildCtx<'_>, hir: &lume_hir::map::Map) -> Result<()> {
        let mut define = DefineImpl { ctx };

        for (_, symbol) in hir.items() {
            if let lume_hir::Symbol::Impl(t) = symbol {
                define.define_impl(t)?;
            }
        }

        Ok(())
    }

    fn define_impl(&mut self, implementation: &lume_hir::Implementation) -> Result<()> {
        let type_ref = self
            .ctx
            .mk_type_ref_generic(implementation.target.as_ref(), &implementation.type_parameters)?;

        for method_def in &implementation.methods {
            let method_name = method_def.name.name.clone();
            let alloc_method = Method::alloc(
                self.ctx.tcx_mut(),
                type_ref.instance_of(),
                method_def.name.clone(),
                method_def.visibility,
            );

            let ty = type_ref.get_mut(self.ctx.tcx_mut());
            ty.methods.insert(method_name, alloc_method);
        }

        Ok(())
    }
}
