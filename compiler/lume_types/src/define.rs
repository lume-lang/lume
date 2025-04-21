use lume_diag::Result;
use lume_hir::{self};

use crate::*;

impl ThirBuildCtx {
    pub(super) fn define_types(&mut self, hir: &lume_hir::map::Map) -> Result<()> {
        let mut define = DefineTypes {
            hir,
            ctx: &mut self.tcx,
        };

        define.run()?;

        Ok(())
    }
}

struct DefineTypes<'a> {
    hir: &'a lume_hir::map::Map,
    ctx: &'a mut TypeDatabaseContext,
}

impl DefineTypes<'_> {
    fn run(&mut self) -> Result<()> {
        for (_, symbol) in self.hir.items() {
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
                let name = class.name.clone();
                let location = class.location.clone();

                let kind = TypeKind::Class(Box::new(Class::new(name.clone(), location)));
                let type_id = Type::alloc(self.ctx, name, kind, location);

                if class.builtin {
                    type_id.set_copied(&mut self.ctx);
                }

                for property in class.properties() {
                    let property_id = Property::alloc(
                        &mut self.ctx,
                        type_id,
                        property.name.clone(),
                        property.visibility.clone(),
                        property.location.clone(),
                    );

                    let property_type = TypeRef::lower_from(&self.ctx, &property.property_type);
                    property_id.set_property_type(&mut self.ctx, property_type);
                }

                for method in class.methods() {
                    let method_id = Method::alloc(
                        &mut self.ctx,
                        type_id,
                        method.name.clone(),
                        method.visibility.clone(),
                        method.location.clone(),
                    );

                    for param in &method.parameters {
                        let name = param.name.name.clone();
                        let type_ref = TypeRef::lower_from(&self.ctx, &param.param_type);

                        method_id.add_parameter(&mut self.ctx, name, type_ref);
                    }

                    let return_type = TypeRef::lower_from(&self.ctx, &*method.return_type);
                    method_id.set_return_type(&mut self.ctx, return_type);
                }

                for method in class.external_methods() {
                    let method_id = Method::alloc(
                        &mut self.ctx,
                        type_id,
                        method.name.clone(),
                        method.visibility.clone(),
                        method.location.clone(),
                    );

                    for param in &method.parameters {
                        let name = param.name.name.clone();
                        let type_ref = TypeRef::lower_from(&self.ctx, &param.param_type);

                        method_id.add_parameter(&mut self.ctx, name, type_ref);
                    }

                    let return_type = TypeRef::lower_from(&self.ctx, &method.return_type);
                    method_id.set_return_type(&mut self.ctx, return_type);
                }
            }
            lume_hir::TypeDefinition::Alias(_) => return Ok(()),
            _ => todo!(),
        };

        Ok(())
    }

    fn define_function(&mut self, func: &lume_hir::FunctionDefinition) -> Result<()> {
        let function_id = Function::alloc(
            &mut self.ctx,
            func.name.clone(),
            func.visibility.clone(),
            func.location.clone(),
        );

        for param in &func.parameters {
            let name = param.name.name.clone();
            let type_ref = TypeRef::lower_from(&self.ctx, &param.param_type);

            function_id.add_parameter(&mut self.ctx, name, type_ref);
        }

        let return_type = TypeRef::lower_from(&self.ctx, &func.return_type);
        function_id.set_return_type(&mut self.ctx, return_type);

        Ok(())
    }
}
