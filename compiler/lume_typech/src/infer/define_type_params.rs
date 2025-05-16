use lume_hir::{self};
use lume_types::*;

use crate::ThirBuildCtx;

pub(super) struct DefineTypeParameters<'a, 'b> {
    ctx: &'a mut ThirBuildCtx<'b>,
}

impl DefineTypeParameters<'_, '_> {
    pub(super) fn run_all(ctx: &mut ThirBuildCtx<'_>, hir: &mut lume_hir::map::Map) {
        let mut define = DefineTypeParameters { ctx };

        define.run(hir);
    }

    fn run(&mut self, hir: &mut lume_hir::map::Map) {
        for (_, symbol) in &mut hir.items {
            match symbol {
                lume_hir::Symbol::Type(t) => self.define_type(t),
                lume_hir::Symbol::Impl(i) => self.define_impl(i),
                lume_hir::Symbol::Function(f) => self.define_function(f),
                lume_hir::Symbol::Use(_) => (),
            }
        }
    }

    fn define_type(&mut self, ty: &mut lume_hir::TypeDefinition) {
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
        }
    }

    fn define_impl(&mut self, implementation: &mut lume_hir::Implementation) {
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
    }

    fn define_function(&mut self, func: &mut lume_hir::FunctionDefinition) {
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
    }
}
