use error_snippet::Result;
use lume_hir::{self, PathSegment, SymbolName, TypeId, TypeParameterId};
use lume_types::TypeKindRef;

use crate::ThirBuildCtx;

pub(super) struct DefineImplementationMethods<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineImplementationMethods<'_> {
    pub(super) fn run_all(ctx: &mut ThirBuildCtx) -> Result<()> {
        let mut hir = std::mem::take(&mut ctx.hir);
        let mut define = DefineImplementationMethods { ctx };

        for (_, symbol) in &mut hir.items {
            if let lume_hir::Item::Impl(t) = symbol {
                define.define_impl(t)?;
            }
        }

        ctx.hir = hir;

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

            method.method_id = Some(method_id);

            for type_param in &mut method.type_parameters {
                let type_param_id = self.ctx.tcx_mut().type_param_alloc(type_param.name.name.clone());

                type_param.type_param_id = Some(type_param_id);
                type_param.type_id = Some(self.wrap_type_param(type_param_id));

                self.ctx.tcx_mut().push_type_param(method_id, type_param_id)?;
            }
        }

        Ok(())
    }

    fn wrap_type_param(&mut self, type_param_id: TypeParameterId) -> TypeId {
        let name = self.ctx.tcx().type_parameter(type_param_id).unwrap().name.clone();
        let symbol_name = SymbolName {
            name: lume_hir::PathSegment::from(name),
            namespace: None,
            location: lume_span::Location::empty(),
        };

        self.ctx
            .tcx_mut()
            .type_alloc(symbol_name, TypeKindRef::TypeParameter(type_param_id))
    }
}
