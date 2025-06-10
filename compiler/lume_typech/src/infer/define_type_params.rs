use error_snippet::Result;
use lume_hir::{self, PathSegment, SymbolName, TypeId, TypeParameterId};
use lume_types::*;

use crate::ThirBuildCtx;

pub(super) struct DefineTypeParameters<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineTypeParameters<'_> {
    #[tracing::instrument(level = "DEBUG", name = "DefineTypeParameters::run_all", skip_all, err)]
    pub(super) fn run_all(ctx: &mut ThirBuildCtx) -> Result<()> {
        let mut hir = std::mem::take(&mut ctx.hir);
        let mut define = DefineTypeParameters { ctx };

        for (_, symbol) in &mut hir.items {
            match symbol {
                lume_hir::Item::Type(t) => define.define_type(t)?,
                lume_hir::Item::Impl(i) => define.define_impl(i)?,
                lume_hir::Item::Use(u) => define.define_use(u)?,
                lume_hir::Item::Function(f) => define.define_function(f)?,
                _ => (),
            }
        }

        ctx.hir = hir;

        Ok(())
    }

    fn define_type(&mut self, ty: &mut lume_hir::TypeDefinition) -> Result<()> {
        match ty {
            lume_hir::TypeDefinition::Struct(struct_def) => {
                let type_id = struct_def.type_id.unwrap();

                for type_param in &mut struct_def.type_parameters {
                    let type_param_id = self.ctx.tcx_mut().type_param_alloc(type_param.name.name.clone());

                    type_param.type_param_id = Some(type_param_id);
                    type_param.type_id = Some(self.wrap_type_param(type_param_id));

                    self.ctx.tcx_mut().push_type_param(type_id, type_param_id)?;
                }

                for method in &mut struct_def.methods_mut() {
                    let method_id = method.method_id.unwrap();

                    for type_param in &mut method.type_parameters {
                        let type_param_id = self.ctx.tcx_mut().type_param_alloc(type_param.name.name.clone());

                        type_param.type_param_id = Some(type_param_id);
                        type_param.type_id = Some(self.wrap_type_param(type_param_id));

                        self.ctx.tcx_mut().push_type_param(method_id, type_param_id)?;
                    }
                }
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                let type_id = trait_def.type_id.unwrap();

                for type_param in &mut trait_def.type_parameters {
                    let type_param_id = self.ctx.tcx_mut().type_param_alloc(type_param.name.name.clone());

                    type_param.type_param_id = Some(type_param_id);
                    type_param.type_id = Some(self.wrap_type_param(type_param_id));

                    self.ctx.tcx_mut().push_type_param(type_id, type_param_id)?;
                }

                for method in &mut trait_def.methods {
                    let method_id = method.method_id.unwrap();

                    for type_param in &mut method.type_parameters {
                        let type_param_id = self.ctx.tcx_mut().type_param_alloc(type_param.name.name.clone());

                        type_param.type_param_id = Some(type_param_id);
                        type_param.type_id = Some(self.wrap_type_param(type_param_id));

                        self.ctx.tcx_mut().push_type_param(method_id, type_param_id)?;
                    }
                }
            }
            _ => {}
        }

        Ok(())
    }

    fn define_impl(&mut self, implementation: &mut lume_hir::Implementation) -> Result<()> {
        let impl_id = implementation.impl_id.unwrap();

        for type_param in &mut implementation.type_parameters {
            let type_param_id = self.ctx.tcx_mut().type_param_alloc(type_param.name.name.clone());

            type_param.type_param_id = Some(type_param_id);
            type_param.type_id = Some(self.wrap_type_param(type_param_id));

            self.ctx.tcx_mut().push_type_param(impl_id, type_param_id)?;
        }

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

    fn define_use(&mut self, trait_impl: &mut lume_hir::TraitImplementation) -> Result<()> {
        let use_id = trait_impl.use_id.unwrap();

        for type_param in &mut trait_impl.type_parameters {
            let type_param_id = self.ctx.tcx_mut().type_param_alloc(type_param.name.name.clone());

            type_param.type_param_id = Some(type_param_id);
            type_param.type_id = Some(self.wrap_type_param(type_param_id));

            self.ctx.tcx_mut().push_type_param(use_id, type_param_id)?;
        }

        let trait_ref = self
            .ctx
            .mk_type_ref_generic(trait_impl.name.as_ref(), &trait_impl.type_parameters)?;

        let target_ref = self
            .ctx
            .mk_type_ref_generic(trait_impl.target.as_ref(), &trait_impl.type_parameters)?;

        let trait_impl_ref = self.ctx.tcx_mut().use_mut(use_id).unwrap();
        trait_impl_ref.trait_ = trait_ref;
        trait_impl_ref.target = target_ref;

        for method in &mut trait_impl.methods {
            let method_id = method.method_id.unwrap();

            for type_param in &mut method.type_parameters {
                let type_param_id = self.ctx.tcx_mut().type_param_alloc(type_param.name.name.clone());

                type_param.type_param_id = Some(type_param_id);
                type_param.type_id = Some(self.wrap_type_param(type_param_id));

                self.ctx.tcx_mut().push_type_param(method_id, type_param_id)?;
            }
        }

        Ok(())
    }

    fn define_function(&mut self, func: &mut lume_hir::FunctionDefinition) -> Result<()> {
        let func_id = func.func_id.unwrap();

        for type_param in &mut func.type_parameters {
            let type_param_id = self.ctx.tcx_mut().type_param_alloc(type_param.name.name.clone());

            type_param.type_param_id = Some(type_param_id);
            type_param.type_id = Some(self.wrap_type_param(type_param_id));

            self.ctx.tcx_mut().push_type_param(func_id, type_param_id)?;
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
