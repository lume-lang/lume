use error_snippet::Result;
use lume_hir::{self};
use lume_types::*;

use crate::ThirBuildCtx;

pub(super) struct DefineTypeParameters<'a, 'b> {
    ctx: &'a mut ThirBuildCtx<'b>,
}

impl DefineTypeParameters<'_, '_> {
    pub(super) fn run_all<'a>(ctx: &mut ThirBuildCtx<'a>, hir: &mut lume_hir::map::Map) -> Result<()> {
        let mut define = DefineTypeParameters { ctx };

        define.run(hir)?;

        Ok(())
    }

    fn run(&mut self, hir: &mut lume_hir::map::Map) -> Result<()> {
        for (_, symbol) in hir.items.iter_mut() {
            match symbol {
                lume_hir::Symbol::Type(t) => self.define_type(t)?,
                lume_hir::Symbol::Impl(i) => self.define_impl(i)?,
                lume_hir::Symbol::Function(f) => self.define_function(f)?,
                _ => (),
            }
        }

        Ok(())
    }

    fn define_type(&mut self, ty: &mut lume_hir::TypeDefinition) -> Result<()> {
        match ty {
            lume_hir::TypeDefinition::Struct(struct_def) => {
                let type_id = struct_def.type_id.unwrap();

                for type_param in &mut struct_def.type_parameters {
                    let type_param_id = type_id.add_type_param(self.ctx.tcx_mut(), type_param.name.name.clone());

                    type_param.type_param_id = Some(type_param_id);
                    type_param.type_id = Some(Type::type_parameter(
                        self.ctx.tcx_mut(),
                        type_param_id,
                        type_param.name.clone(),
                    ));
                }

                for method in &mut struct_def.methods_mut() {
                    let method_id = method.method_id.unwrap();

                    for type_param in &mut method.type_parameters {
                        let type_param_id = method_id.add_type_param(self.ctx.tcx_mut(), type_param.name.name.clone());

                        type_param.type_param_id = Some(type_param_id);
                        type_param.type_id = Some(Type::type_parameter(
                            self.ctx.tcx_mut(),
                            type_param_id,
                            type_param.name.clone(),
                        ));
                    }
                }
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                let type_id = trait_def.type_id.unwrap();

                for type_param in &mut trait_def.type_parameters {
                    let type_param_id = type_id.add_type_param(self.ctx.tcx_mut(), type_param.name.name.clone());

                    type_param.type_param_id = Some(type_param_id);
                    type_param.type_id = Some(Type::type_parameter(
                        self.ctx.tcx_mut(),
                        type_param_id,
                        type_param.name.clone(),
                    ));
                }

                for method in &mut trait_def.methods {
                    let method_id = method.method_id.unwrap();

                    for type_param in &mut method.type_parameters {
                        let type_param_id = method_id.add_type_param(self.ctx.tcx_mut(), type_param.name.name.clone());

                        type_param.type_param_id = Some(type_param_id);
                        type_param.type_id = Some(Type::type_parameter(
                            self.ctx.tcx_mut(),
                            type_param_id,
                            type_param.name.clone(),
                        ));
                    }
                }
            }
            _ => {}
        };

        Ok(())
    }

    fn define_impl(&mut self, implementation: &mut lume_hir::Implementation) -> Result<()> {
        let impl_id = implementation.impl_id.unwrap();

        for type_param in &mut implementation.type_parameters {
            let type_param_id = impl_id.add_type_param(self.ctx.tcx_mut(), type_param.name.name.clone());

            type_param.type_param_id = Some(type_param_id);
            type_param.type_id = Some(Type::type_parameter(
                self.ctx.tcx_mut(),
                type_param_id,
                type_param.name.clone(),
            ));
        }

        Ok(())
    }

    fn define_function(&mut self, func: &mut lume_hir::FunctionDefinition) -> Result<()> {
        let func_id = func.func_id.unwrap();

        for type_param in &mut func.type_parameters {
            let type_param_id = func_id.add_type_param(self.ctx.tcx_mut(), type_param.name.name.clone());

            type_param.type_param_id = Some(type_param_id);
            type_param.type_id = Some(Type::type_parameter(
                self.ctx.tcx_mut(),
                type_param_id,
                type_param.name.clone(),
            ));
        }

        Ok(())
    }
}
