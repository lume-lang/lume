use error_snippet::Result;
use lume_hir::{self};
use lume_types::TypeRef;

use crate::ThirBuildCtx;

pub(super) struct DefineMethodBodies<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineMethodBodies<'_> {
    #[tracing::instrument(level = "DEBUG", name = "DefineMethodBodies::run_all", skip_all, err)]
    pub(super) fn run_all(ctx: &mut ThirBuildCtx) -> Result<()> {
        let hir = std::mem::take(&mut ctx.hir);
        let mut define = DefineMethodBodies { ctx };

        for (_, symbol) in &hir.items {
            match symbol {
                lume_hir::Item::Type(t) => define.define_type(t)?,
                lume_hir::Item::Function(f) => define.define_function(f)?,
                lume_hir::Item::Use(u) => define.define_use(u)?,
                lume_hir::Item::Impl(i) => define.define_impl(i)?,
                _ => (),
            }
        }

        ctx.hir = hir;

        Ok(())
    }

    fn define_type(&mut self, ty: &lume_hir::TypeDefinition) -> Result<()> {
        match &ty {
            lume_hir::TypeDefinition::Struct(struct_def) => {
                for method in struct_def.methods() {
                    let method_id = method.method_id.unwrap();

                    for param in &method.parameters {
                        let name = param.name.name.clone();
                        let type_ref = self.ctx.mk_type_ref_generic(
                            &param.param_type,
                            &[&struct_def.type_parameters[..], &method.type_parameters[..]].concat(),
                        )?;

                        self.ctx
                            .tcx_mut()
                            .method_mut(method_id)
                            .unwrap()
                            .parameters
                            .push(name, type_ref, param.vararg);
                    }

                    self.ctx.tcx_mut().method_mut(method_id).unwrap().return_type =
                        if let Some(ret) = &method.return_type {
                            self.ctx.mk_type_ref_generic(
                                ret,
                                &[&struct_def.type_parameters[..], &method.type_parameters[..]].concat(),
                            )?
                        } else {
                            TypeRef::void()
                        }
                }
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                for method in &trait_def.methods {
                    let method_id = method.method_id.unwrap();

                    for param in &method.parameters {
                        let name = param.name.name.clone();
                        let type_ref = self.ctx.mk_type_ref_generic(
                            &param.param_type,
                            &[&trait_def.type_parameters[..], &method.type_parameters[..]].concat(),
                        )?;

                        self.ctx
                            .tcx_mut()
                            .method_mut(method_id)
                            .unwrap()
                            .parameters
                            .push(name, type_ref, param.vararg);
                    }

                    self.ctx.tcx_mut().method_mut(method_id).unwrap().return_type =
                        if let Some(ret) = &method.return_type {
                            self.ctx.mk_type_ref_generic(
                                ret,
                                &[&trait_def.type_parameters[..], &method.type_parameters[..]].concat(),
                            )?
                        } else {
                            TypeRef::void()
                        }
                }
            }
            _ => (),
        }

        Ok(())
    }

    fn define_function(&mut self, func: &lume_hir::FunctionDefinition) -> Result<()> {
        let func_id = func.func_id.unwrap();

        for param in &func.parameters {
            let name = param.name.name.clone();
            let type_ref = self.ctx.mk_type_ref_generic(&param.param_type, &func.type_parameters)?;

            self.ctx
                .tcx_mut()
                .function_mut(func_id)
                .unwrap()
                .parameters
                .push(name, type_ref, param.vararg);
        }

        self.ctx.tcx_mut().function_mut(func_id).unwrap().return_type = if let Some(ret) = &func.return_type {
            self.ctx.mk_type_ref_generic(ret, &func.type_parameters)?
        } else {
            TypeRef::void()
        };

        Ok(())
    }

    fn define_use(&mut self, trait_impl: &lume_hir::TraitImplementation) -> Result<()> {
        for method in &trait_impl.methods {
            let method_id = method.method_id.unwrap();

            for param in &method.parameters {
                let name = param.name.name.clone();
                let type_ref = self.ctx.mk_type_ref_generic(
                    &param.param_type,
                    &[&trait_impl.type_parameters[..], &method.type_parameters[..]].concat(),
                )?;

                self.ctx
                    .tcx_mut()
                    .method_mut(method_id)
                    .unwrap()
                    .parameters
                    .push(name, type_ref, param.vararg);
            }

            self.ctx.tcx_mut().method_mut(method_id).unwrap().return_type = if let Some(ret) = &method.return_type {
                self.ctx.mk_type_ref_generic(ret, &method.type_parameters)?
            } else {
                TypeRef::void()
            };
        }

        Ok(())
    }

    fn define_impl(&mut self, implementation: &lume_hir::Implementation) -> Result<()> {
        for method in &implementation.methods {
            let method_id = method.method_id.unwrap();

            for param in &method.parameters {
                let name = param.name.name.clone();
                let type_ref = self
                    .ctx
                    .mk_type_ref_generic(&param.param_type, &implementation.type_parameters)?;

                self.ctx
                    .tcx_mut()
                    .method_mut(method_id)
                    .unwrap()
                    .parameters
                    .push(name, type_ref, param.vararg);
            }

            self.ctx.tcx_mut().method_mut(method_id).unwrap().return_type = if let Some(ret) = &method.return_type {
                self.ctx.mk_type_ref_generic(ret, &method.type_parameters)?
            } else {
                TypeRef::void()
            };
        }

        Ok(())
    }
}
