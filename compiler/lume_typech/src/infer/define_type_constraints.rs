use error_snippet::Result;
use lume_hir::{self, TypeParameterId};
use lume_types::WithTypeParameters;

use crate::ThirBuildCtx;

pub(super) struct DefineTypeConstraints<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineTypeConstraints<'_> {
    pub(super) fn run_all(ctx: &mut ThirBuildCtx) -> Result<()> {
        let hir = std::mem::take(&mut ctx.hir);
        let mut define = DefineTypeConstraints { ctx };

        for (_, symbol) in &hir.items {
            match symbol {
                lume_hir::Item::Type(t) => define.define_type(t)?,
                lume_hir::Item::Impl(i) => define.define_impl(i)?,
                lume_hir::Item::Function(f) => define.define_function(f)?,
                _ => (),
            }
        }

        ctx.hir = hir;

        Ok(())
    }

    fn define_type(&mut self, ty: &lume_hir::TypeDefinition) -> Result<()> {
        match ty {
            lume_hir::TypeDefinition::Struct(struct_def) => {
                let type_id = struct_def.type_id.unwrap();
                let type_params = self.ctx.tcx().type_params_of(type_id)?.to_owned();

                self.define_type_constraints(&struct_def.type_parameters, &type_params)?;

                for method in struct_def.methods() {
                    let method_id = method.method_id.unwrap();
                    let type_params = method_id.type_params(self.ctx.tcx())?.to_owned();

                    self.define_type_constraints(&method.type_parameters, &type_params)?;
                }
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                let type_id = trait_def.type_id.unwrap();
                let type_params = type_id.type_params(self.ctx.tcx())?.to_owned();

                self.define_type_constraints(&trait_def.type_parameters, &type_params)?;

                for method in &trait_def.methods {
                    let method_id = method.method_id.unwrap();
                    let type_params = self.ctx.tcx().type_params_of(method_id)?.to_owned();

                    self.define_type_constraints(&method.type_parameters, &type_params)?;
                }
            }
            _ => {}
        }

        Ok(())
    }

    fn define_impl(&mut self, implementation: &lume_hir::Implementation) -> Result<()> {
        let impl_id = implementation.impl_id.unwrap();
        let type_params = self.ctx.tcx().type_params_of(impl_id)?.to_owned();

        self.define_type_constraints(&implementation.type_parameters, &type_params)?;

        for method in &implementation.methods {
            let method_id = method.method_id.unwrap();
            let type_params = self.ctx.tcx().type_params_of(method_id)?.to_owned();

            self.define_type_constraints(&method.type_parameters, &type_params)?;
        }

        Ok(())
    }

    fn define_function(&mut self, func: &lume_hir::FunctionDefinition) -> Result<()> {
        let func_id = func.func_id.unwrap();
        let type_params = self.ctx.tcx().function(func_id).unwrap().type_parameters.clone();

        self.define_type_constraints(&func.type_parameters, &type_params)?;

        Ok(())
    }

    fn define_type_constraints(&mut self, hir: &[lume_hir::TypeParameter], ids: &[TypeParameterId]) -> Result<()> {
        for (param_id, hir_param) in ids.iter().zip(hir.iter()) {
            for type_constraint in &hir_param.constraints {
                let lowered_constraint = self.ctx.mk_type_ref(type_constraint)?;

                self.ctx
                    .tcx_mut()
                    .type_parameter_mut(*param_id)
                    .unwrap()
                    .constraints
                    .push(lowered_constraint);
            }
        }

        Ok(())
    }
}
