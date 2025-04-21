use lume_diag::Result;
use lume_hir::{self};
use lume_types::*;

use crate::*;

impl ThirBuildCtx {
    pub(super) fn define_types(&mut self, hir: &mut lume_hir::map::Map) -> Result<()> {
        DefineTypes::run_all(hir, self)?;
        DefineMethodBodies::run_all(hir, self)?;

        Ok(())
    }
}

struct DefineTypes<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineTypes<'_> {
    fn run_all<'a>(hir: &'a mut lume_hir::map::Map, ctx: &'a mut ThirBuildCtx) -> Result<()> {
        let mut define = DefineTypes { ctx };

        define.run(hir)?;

        Ok(())
    }

    fn run(&mut self, hir: &mut lume_hir::map::Map) -> Result<()> {
        for (_, symbol) in hir.items.iter_mut() {
            match symbol {
                lume_hir::Symbol::Type(t) => self.define_type(t)?,
                _ => (),
            }
        }

        Ok(())
    }

    fn define_type(&mut self, ty: &mut lume_hir::TypeDefinition) -> Result<()> {
        match ty {
            lume_hir::TypeDefinition::Class(class) => {
                let name = class.name.clone();
                let kind = TypeKind::Class(Box::new(Class::new(name.clone())));
                let type_id = Type::alloc(&mut self.ctx.tcx, name, kind);

                class.type_id = Some(type_id);
            }
            lume_hir::TypeDefinition::Alias(_) => return Ok(()),
            _ => todo!(),
        };

        Ok(())
    }
}

struct DefineMethodBodies<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineMethodBodies<'_> {
    fn run_all<'a>(hir: &'a mut lume_hir::map::Map, ctx: &'a mut ThirBuildCtx) -> Result<()> {
        let mut define = DefineMethodBodies { ctx };

        define.run(hir)?;

        Ok(())
    }

    fn run(&mut self, hir: &mut lume_hir::map::Map) -> Result<()> {
        for (_, symbol) in hir.items() {
            match symbol {
                lume_hir::Symbol::Type(t) => self.define_type(&*t)?,
                lume_hir::Symbol::Function(f) => self.define_function(&*f)?,
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
                    type_id.set_copied(&mut self.ctx.tcx);
                }

                for property in class.properties() {
                    let property_id = Property::alloc(
                        &mut self.ctx.tcx,
                        type_id,
                        property.name.name.clone(),
                        property.visibility.clone(),
                    );

                    let property_type = self.ctx.lower_type_ref(&property.property_type);
                    property_id.set_property_type(&mut self.ctx.tcx, property_type);
                }

                for method in class.methods() {
                    let method_id = Method::alloc(
                        &mut self.ctx.tcx,
                        type_id,
                        method.name.name.clone(),
                        method.visibility.clone(),
                    );

                    for param in &method.parameters {
                        let name = param.name.name.clone();
                        let type_ref = self.ctx.lower_type_ref(&param.param_type);

                        method_id.add_parameter(&mut self.ctx.tcx, name, type_ref);
                    }

                    let return_type = self.ctx.lower_type_ref(&method.return_type);
                    method_id.set_return_type(&mut self.ctx.tcx, return_type);
                }

                for method in class.external_methods() {
                    let method_id = Method::alloc(
                        &mut self.ctx.tcx,
                        type_id,
                        method.name.name.clone(),
                        method.visibility.clone(),
                    );

                    for param in &method.parameters {
                        let name = param.name.name.clone();
                        let type_ref = self.ctx.lower_type_ref(&param.param_type);

                        method_id.add_parameter(&mut self.ctx.tcx, name, type_ref);
                    }

                    let return_type = self.ctx.lower_type_ref(&method.return_type);
                    method_id.set_return_type(&mut self.ctx.tcx, return_type);
                }
            }
            lume_hir::TypeDefinition::Alias(_) => return Ok(()),
            _ => todo!(),
        };

        Ok(())
    }

    fn define_function(&mut self, func: &lume_hir::FunctionDefinition) -> Result<()> {
        let function_id = Function::alloc(&mut self.ctx.tcx, func.name.clone(), func.visibility.clone());

        for param in &func.parameters {
            let name = param.name.name.clone();
            let type_ref = self.ctx.lower_type_ref(&param.param_type);

            function_id.add_parameter(&mut self.ctx.tcx, name, type_ref);
        }

        let return_type = self.ctx.lower_type_ref(&func.return_type);
        function_id.set_return_type(&mut self.ctx.tcx, return_type);

        Ok(())
    }
}
