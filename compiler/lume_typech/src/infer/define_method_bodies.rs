use error_snippet::Result;
use lume_hir::{self};

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
                _ => (),
            }
        }

        Ok(())
    }

    fn define_type(&mut self, ty: &lume_hir::TypeDefinition) -> Result<()> {
        match &ty {
            lume_hir::TypeDefinition::Struct(struct_def) => {
                for property in struct_def.properties() {
                    let property_id = property.prop_id.unwrap();

                    let type_ref = self
                        .ctx
                        .mk_type_ref_generic(&property.property_type, &struct_def.type_parameters)?;

                    self.ctx.tcx_mut().property_mut(property_id).unwrap().property_type = type_ref;
                }

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

                    if let Some(ret) = &method.return_type {
                        let return_type = self.ctx.mk_type_ref_generic(
                            ret,
                            &[&struct_def.type_parameters[..], &method.type_parameters[..]].concat(),
                        )?;

                        self.ctx.tcx_mut().method_mut(method_id).unwrap().return_type = return_type;
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

                    if let Some(ret) = &method.return_type {
                        let return_type = self.ctx.mk_type_ref_generic(
                            ret,
                            &[&trait_def.type_parameters[..], &method.type_parameters[..]].concat(),
                        )?;

                        self.ctx.tcx_mut().method_mut(method_id).unwrap().return_type = return_type;
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

        if let Some(ret) = &func.return_type {
            let return_type = self.ctx.mk_type_ref_generic(ret, &func.type_parameters)?;

            self.ctx.tcx_mut().function_mut(func_id).unwrap().return_type = return_type;
        }

        Ok(())
    }
}
