use error_snippet::Result;
use lume_hir::{self, WithTypeParameters};
use lume_types::*;

use crate::ThirBuildCtx;

pub(super) struct DefineTypeConstraints<'a, 'b> {
    ctx: &'a mut ThirBuildCtx<'b>,
}

impl DefineTypeConstraints<'_, '_> {
    pub(super) fn run_all(ctx: &mut ThirBuildCtx<'_>, hir: &mut lume_hir::map::Map) -> Result<()> {
        let mut define = DefineTypeConstraints { ctx };

        define.run(hir)?;

        Ok(())
    }

    fn run(&mut self, hir: &lume_hir::map::Map) -> Result<()> {
        for (_, symbol) in &hir.items {
            match symbol {
                lume_hir::Symbol::Type(t) => self.define_type(t)?,
                lume_hir::Symbol::Impl(i) => self.define_impl(i)?,
                lume_hir::Symbol::Function(f) => self.define_function(f)?,
                lume_hir::Symbol::Use(_) => (),
            }
        }

        Ok(())
    }

    fn define_type(&mut self, ty: &lume_hir::TypeDefinition) -> Result<()> {
        match ty {
            lume_hir::TypeDefinition::Struct(struct_def) => {
                let type_id = struct_def.type_id.unwrap();
                let type_params = type_id.type_params(self.ctx.tcx()).clone();

                self.define_type_constraints(&**struct_def, &type_params)?;

                for method in struct_def.methods() {
                    let method_id = method.method_id.unwrap();
                    let type_params = method_id.type_params(self.ctx.tcx()).clone();

                    self.define_type_constraints(method, &type_params)?;
                }
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                let type_id = trait_def.type_id.unwrap();
                let type_params = type_id.type_params(self.ctx.tcx()).clone();

                self.define_type_constraints(&**trait_def, &type_params)?;

                for method in &trait_def.methods {
                    let method_id = method.method_id.unwrap();
                    let type_params = method_id.type_params(self.ctx.tcx()).clone();

                    self.define_type_constraints(method, &type_params)?;
                }
            }
            _ => {}
        }

        Ok(())
    }

    fn define_impl(&mut self, implementation: &lume_hir::Implementation) -> Result<()> {
        let impl_id = implementation.impl_id.unwrap();
        let type_params = impl_id.type_params(self.ctx.tcx()).clone();

        self.define_type_constraints(implementation, &type_params)?;

        Ok(())
    }

    fn define_function(&mut self, func: &lume_hir::FunctionDefinition) -> Result<()> {
        let func_id = func.func_id.unwrap();
        let type_params = func_id.type_params(self.ctx.tcx()).clone();

        self.define_type_constraints(func, &type_params)?;

        Ok(())
    }

    fn define_type_constraints(&mut self, ty: &impl WithTypeParameters, params: &[TypeParameterId]) -> Result<()> {
        let type_param_ids = params
            .iter()
            .enumerate()
            .map(|(idx, id)| (idx, *id))
            .collect::<Vec<(usize, TypeParameterId)>>();

        for (idx, type_param_id) in type_param_ids {
            let type_param = &ty.type_params()[idx];

            for type_constraint in &type_param.constraints {
                let lowered_type_constraint = self.ctx.mk_type_ref(type_constraint)?;

                type_param_id.add_constraint(self.ctx.tcx_mut(), lowered_type_constraint);
            }
        }

        Ok(())
    }
}
