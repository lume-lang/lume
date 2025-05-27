use error_snippet::Result;
use lume_hir::{self, PathSegment, SymbolName};

use crate::ThirBuildCtx;

pub(super) struct DefineImpl<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineImpl<'_> {
    pub(super) fn run_all(ctx: &mut ThirBuildCtx, hir: &mut lume_hir::map::Map) -> Result<()> {
        let mut define = DefineImpl { ctx };

        for (_, symbol) in &mut hir.items {
            if let lume_hir::Symbol::Impl(t) = symbol {
                define.define_impl(t)?;
            }
        }

        Ok(())
    }

    fn define_impl(&mut self, implementation: &mut lume_hir::Implementation) -> Result<()> {
        let type_ref = self
            .ctx
            .mk_type_ref_generic(implementation.target.as_ref(), &implementation.type_parameters)?;

        for method in &mut implementation.methods {
            let method_name = method.name.clone();
            let mut qualified_name = SymbolName::with_root(
                implementation.target.name.clone(),
                PathSegment::Named(method_name.clone()),
            );

            qualified_name.location = method_name.location.clone();

            let method_id = self
                .ctx
                .tcx_mut()
                .method_alloc(type_ref.clone(), qualified_name, method.visibility)?;

            for param in &method.parameters {
                let name = param.name.name.clone();
                let type_ref = self.ctx.mk_type_ref_generic(
                    &param.param_type,
                    &[&implementation.type_parameters[..], &method.type_parameters[..]].concat(),
                )?;

                self.ctx
                    .tcx_mut()
                    .method_mut(method_id)
                    .unwrap()
                    .parameters
                    .push(name, type_ref);
            }

            if let Some(ret) = &method.return_type {
                let return_type = self.ctx.mk_type_ref_generic(
                    ret,
                    &[&implementation.type_parameters[..], &method.type_parameters[..]].concat(),
                )?;

                self.ctx.tcx_mut().method_mut(method_id).unwrap().return_type = return_type;
            }

            method.method_id = Some(method_id);
        }

        Ok(())
    }
}
