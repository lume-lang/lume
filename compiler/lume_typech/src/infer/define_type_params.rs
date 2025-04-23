use lume_diag::Result;
use lume_hir::{self};
use lume_types::*;

use crate::*;

pub(super) struct DefineTypeParameters<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineTypeParameters<'_> {
    pub(super) fn run_all<'a>(hir: &'a mut lume_hir::map::Map, ctx: &'a mut ThirBuildCtx) -> Result<()> {
        let mut define = DefineTypeParameters { ctx };

        define.run(hir)?;

        Ok(())
    }

    fn run(&mut self, hir: &mut lume_hir::map::Map) -> Result<()> {
        for (_, symbol) in hir.items.iter_mut() {
            match symbol {
                lume_hir::Symbol::Type(t) => self.define_type(t)?,
                lume_hir::Symbol::Function(f) => self.define_function(f)?,
                _ => (),
            }
        }

        Ok(())
    }

    fn define_type(&mut self, ty: &mut lume_hir::TypeDefinition) -> Result<()> {
        match ty {
            lume_hir::TypeDefinition::Class(class) => {
                let type_id = class.type_id.unwrap();

                for type_param in &mut class.type_parameters {
                    let type_param_id = type_id.add_type_param(&mut self.ctx.tcx, type_param.name.name.clone());

                    type_param.type_param_id = Some(type_param_id);
                    type_param.type_id = Some(Type::type_parameter(
                        &mut self.ctx.tcx,
                        type_param_id,
                        type_param.name.clone(),
                    ));
                }

                for method in &mut class.methods_mut() {
                    let method_id = method.method_id.unwrap();

                    for type_param in &mut method.type_parameters {
                        let type_param_id = method_id.add_type_param(&mut self.ctx.tcx, type_param.name.name.clone());

                        type_param.type_param_id = Some(type_param_id);
                        type_param.type_id = Some(Type::type_parameter(
                            &mut self.ctx.tcx,
                            type_param_id,
                            type_param.name.clone(),
                        ));
                    }
                }

                for method in &mut class.external_methods_mut() {
                    let method_id = method.method_id.unwrap();

                    for type_param in &mut method.type_parameters {
                        let type_param_id = method_id.add_type_param(&mut self.ctx.tcx, type_param.name.name.clone());

                        type_param.type_param_id = Some(type_param_id);
                        type_param.type_id = Some(Type::type_parameter(
                            &mut self.ctx.tcx,
                            type_param_id,
                            type_param.name.clone(),
                        ));
                    }
                }
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                let type_id = trait_def.type_id.unwrap();

                for type_param in &mut trait_def.type_parameters {
                    let type_param_id = type_id.add_type_param(&mut self.ctx.tcx, type_param.name.name.clone());

                    type_param.type_param_id = Some(type_param_id);
                    type_param.type_id = Some(Type::type_parameter(
                        &mut self.ctx.tcx,
                        type_param_id,
                        type_param.name.clone(),
                    ));
                }

                for method in &mut trait_def.methods {
                    let method_id = method.method_id.unwrap();

                    for type_param in &mut method.type_parameters {
                        let type_param_id = method_id.add_type_param(&mut self.ctx.tcx, type_param.name.name.clone());

                        type_param.type_param_id = Some(type_param_id);
                        type_param.type_id = Some(Type::type_parameter(
                            &mut self.ctx.tcx,
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

    fn define_function(&mut self, func: &lume_hir::FunctionDefinition) -> Result<()> {
        let func_id = func.func_id.unwrap();

        for type_param in &func.type_parameters {
            func_id.add_type_param(&mut self.ctx.tcx, type_param.name.name.clone());
        }

        Ok(())
    }
}
