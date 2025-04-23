use lume_diag::Result;
use lume_hir::{self};

use crate::ThirBuildCtx;

pub(super) struct DefineMethodBodies<'a, 'b> {
    ctx: &'a mut ThirBuildCtx<'b>,
}

impl DefineMethodBodies<'_, '_> {
    pub(super) fn run_all<'a>(ctx: &mut ThirBuildCtx<'a>, hir: &mut lume_hir::map::Map) -> Result<()> {
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
            lume_hir::TypeDefinition::Class(class) => {
                let type_id = class.type_id.unwrap();

                if class.builtin {
                    type_id.set_copied(self.ctx.tcx_mut());
                }

                for property in class.properties() {
                    let property_id = property.prop_id.unwrap();

                    let type_ref = self
                        .ctx
                        .mk_type_ref_generic(&property.property_type, &class.type_parameters)?;

                    property_id.set_property_type(self.ctx.tcx_mut(), type_ref);
                }

                for method in class.methods() {
                    let method_id = method.method_id.unwrap();

                    for param in &method.parameters {
                        let name = param.name.name.clone();
                        let type_ref = self.ctx.mk_type_ref_generic(
                            &param.param_type,
                            &[&class.type_parameters[..], &method.type_parameters[..]].concat(),
                        )?;

                        method_id.add_parameter(self.ctx.tcx_mut(), name, type_ref);
                    }

                    let return_type = self.ctx.mk_type_ref_generic(
                        &method.return_type,
                        &[&class.type_parameters[..], &method.type_parameters[..]].concat(),
                    )?;

                    method_id.set_return_type(self.ctx.tcx_mut(), return_type);
                }

                for method in class.external_methods() {
                    let method_id = method.method_id.unwrap();

                    for param in &method.parameters {
                        let name = param.name.name.clone();
                        let type_ref = self.ctx.mk_type_ref_generic(
                            &param.param_type,
                            &[&class.type_parameters[..], &method.type_parameters[..]].concat(),
                        )?;

                        method_id.add_parameter(self.ctx.tcx_mut(), name, type_ref);
                    }

                    let return_type = self.ctx.mk_type_ref_generic(
                        &method.return_type,
                        &[&class.type_parameters[..], &method.type_parameters[..]].concat(),
                    )?;

                    method_id.set_return_type(self.ctx.tcx_mut(), return_type);
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

                        method_id.add_parameter(self.ctx.tcx_mut(), name, type_ref);
                    }

                    let return_type = self.ctx.mk_type_ref_generic(
                        &method.return_type,
                        &[&trait_def.type_parameters[..], &method.type_parameters[..]].concat(),
                    )?;

                    method_id.set_return_type(self.ctx.tcx_mut(), return_type);
                }
            }
            _ => (),
        };

        Ok(())
    }

    fn define_function(&mut self, func: &lume_hir::FunctionDefinition) -> Result<()> {
        let function_id = func.func_id.unwrap();

        for param in &func.parameters {
            let name = param.name.name.clone();
            let type_ref = self.ctx.mk_type_ref_generic(&param.param_type, &func.type_parameters)?;

            function_id.add_parameter(self.ctx.tcx_mut(), name, type_ref);
        }

        let return_type = self.ctx.mk_type_ref_generic(&func.return_type, &func.type_parameters)?;
        function_id.set_return_type(self.ctx.tcx_mut(), return_type);

        Ok(())
    }
}
