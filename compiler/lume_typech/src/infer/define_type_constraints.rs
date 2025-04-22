use lume_diag::Result;
use lume_hir::{self, WithTypeParameters};
use lume_types::*;

use crate::*;

pub(super) struct DefineTypeConstraints<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineTypeConstraints<'_> {
    pub(super) fn run_all<'a>(hir: &'a mut lume_hir::map::Map, ctx: &'a mut ThirBuildCtx) -> Result<()> {
        let mut define = DefineTypeConstraints { ctx };

        define.run(hir)?;

        Ok(())
    }

    fn run(&mut self, hir: &lume_hir::map::Map) -> Result<()> {
        for (_, symbol) in &hir.items {
            match symbol {
                lume_hir::Symbol::Type(t) => self.define_type(t)?,
                lume_hir::Symbol::Function(f) => self.define_function(&*f)?,
                _ => (),
            }
        }

        Ok(())
    }

    fn define_type(&mut self, ty: &lume_hir::TypeDefinition) -> Result<()> {
        match ty {
            lume_hir::TypeDefinition::Class(class_def) => {
                let type_id = class_def.type_id.unwrap();
                let type_params = type_id.type_params(&self.ctx.tcx).clone();

                self.define_type_constraints(&**class_def, &type_params)?;

                for method in class_def.methods() {
                    let method_id = method.method_id.unwrap();
                    let type_params = method_id.type_params(&self.ctx.tcx).clone();

                    self.define_type_constraints(method, &type_params)?;
                }

                for method in class_def.external_methods() {
                    let method_id = method.method_id.unwrap();
                    let type_params = method_id.type_params(&self.ctx.tcx).clone();

                    self.define_type_constraints(method, &type_params)?;
                }
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                let type_id = trait_def.type_id.unwrap();
                let type_params = type_id.type_params(&self.ctx.tcx).clone();

                self.define_type_constraints(&**trait_def, &type_params)?;

                for method in &trait_def.methods {
                    let method_id = method.method_id.unwrap();
                    let type_params = method_id.type_params(&self.ctx.tcx).clone();

                    self.define_type_constraints(method, &type_params)?;
                }
            }
            _ => {}
        };

        Ok(())
    }

    fn define_function(&mut self, func: &lume_hir::FunctionDefinition) -> Result<()> {
        let func_id = func.func_id.unwrap();
        let type_params = func_id.type_params(&self.ctx.tcx).clone();

        self.define_type_constraints(func, &type_params)?;

        Ok(())
    }

    fn define_type_constraints(&mut self, ty: &impl WithTypeParameters, params: &Vec<TypeParameterId>) -> Result<()> {
        let type_param_ids = params
            .iter()
            .enumerate()
            .map(|(idx, id)| (idx, *id))
            .collect::<Vec<(usize, TypeParameterId)>>();

        for (idx, type_param_id) in type_param_ids {
            let type_param = &ty.type_params()[idx];

            for type_constraint in &type_param.constraints {
                let lowered_type_constraint = self.ctx.mk_type_ref(&type_constraint)?;

                type_param_id.add_constraint(&mut self.ctx.tcx, lowered_type_constraint);
            }
        }

        Ok(())
    }
}
