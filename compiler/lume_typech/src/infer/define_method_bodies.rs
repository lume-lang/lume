use error_snippet::Result;
use lume_hir::{self};
use lume_types::TypeRef;

use crate::ThirBuildCtx;

pub(super) struct DefineMethodBodies<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineMethodBodies<'_> {
    pub(super) fn run_all(ctx: &mut ThirBuildCtx, hir: &mut lume_hir::map::Map) -> Result<()> {
        let mut define = DefineMethodBodies { ctx };

        define.run(hir)?;

        Ok(())
    }

    fn run(&mut self, hir: &mut lume_hir::map::Map) -> Result<()> {
        for (_, symbol) in hir.items() {
            match symbol {
                lume_hir::Symbol::Type(t) => self.define_type(t)?,
                lume_hir::Symbol::Function(f) => self.define_function(f)?,
                lume_hir::Symbol::Use(u) => self.define_use(u)?,
                lume_hir::Symbol::Impl(i) => self.define_impl(i)?,
            }
        }

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
                            .push(name, type_ref);
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
                            .push(name, type_ref);
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
                .push(name, type_ref);
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
                let type_ref = self.ctx.mk_type_ref(&param.param_type)?;

                self.ctx
                    .tcx_mut()
                    .method_mut(method_id)
                    .unwrap()
                    .parameters
                    .push(name, type_ref);
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
                let type_ref = self.ctx.mk_type_ref(&param.param_type)?;

                self.ctx
                    .tcx_mut()
                    .method_mut(method_id)
                    .unwrap()
                    .parameters
                    .push(name, type_ref);
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
