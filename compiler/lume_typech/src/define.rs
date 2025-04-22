use lume_diag::Result;
use lume_hir::{self, WithTypeParameters};
use lume_types::*;

use crate::*;

impl ThirBuildCtx {
    pub(super) fn define_types(&mut self, hir: &mut lume_hir::map::Map) -> Result<()> {
        DefineTypes::run_all(hir, self)?;
        DefineTypeParameters::run_all(hir, self)?;
        DefineTypeConstraints::run_all(hir, self)?;
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
                lume_hir::Symbol::Function(f) => self.define_function(f)?,
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

                for property in &mut class.properties_mut() {
                    let property_name = property.name.name.clone();
                    let visibility = property.visibility;
                    let property_id = Property::alloc(&mut self.ctx.tcx, type_id, property_name, visibility);

                    property.prop_id = Some(property_id);
                }

                for method in &mut class.methods_mut() {
                    let method_name = method.name.name.clone();
                    let visibility = method.visibility;
                    let method_id = Method::alloc(&mut self.ctx.tcx, type_id, method_name, visibility);

                    method.method_id = Some(method_id);
                }

                for method in &mut class.external_methods_mut() {
                    let method_name = method.name.name.clone();
                    let visibility = method.visibility;
                    let method_id = Method::alloc(&mut self.ctx.tcx, type_id, method_name, visibility);

                    method.method_id = Some(method_id);
                }

                class.type_id = Some(type_id);
            }
            lume_hir::TypeDefinition::Alias(alias) => {
                let name = alias.name.clone();
                let kind = TypeKind::Alias(Box::new(Alias::new(name.clone())));
                let type_id = Type::alloc(&mut self.ctx.tcx, name, kind);

                alias.type_id = Some(type_id);
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                let name = trait_def.name.clone();
                let kind = TypeKind::Trait(Box::new(Trait::new(name.clone())));
                let type_id = Type::alloc(&mut self.ctx.tcx, name, kind);

                for method in &mut trait_def.methods {
                    let method_name = method.name.name.clone();
                    let visibility = method.visibility;
                    let method_id = Method::alloc(&mut self.ctx.tcx, type_id, method_name, visibility);

                    method.method_id = Some(method_id);
                }

                trait_def.type_id = Some(type_id);
            }
            lume_hir::TypeDefinition::Enum(enum_def) => {
                let name = enum_def.name.clone();
                let kind = TypeKind::Enum(Box::new(Enum::new(name.clone())));
                let type_id = Type::alloc(&mut self.ctx.tcx, name, kind);

                enum_def.type_id = Some(type_id);
            }
        };

        Ok(())
    }

    fn define_function(&mut self, func: &mut lume_hir::FunctionDefinition) -> Result<()> {
        let name = func.name.clone();
        let visibility = func.visibility;
        let type_id = Function::alloc(&mut self.ctx.tcx, name, visibility);

        func.func_id = Some(type_id);

        Ok(())
    }
}

struct DefineTypeParameters<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineTypeParameters<'_> {
    fn run_all<'a>(hir: &'a mut lume_hir::map::Map, ctx: &'a mut ThirBuildCtx) -> Result<()> {
        let mut define = DefineTypeParameters { ctx };

        define.run(hir)?;

        Ok(())
    }

    fn run(&mut self, hir: &mut lume_hir::map::Map) -> Result<()> {
        for (_, symbol) in hir.items.iter_mut() {
            match symbol {
                lume_hir::Symbol::Type(t) => self.define_type(t)?,
                lume_hir::Symbol::Function(f) => self.define_function(&*f)?,
                _ => (),
            }
        }

        Ok(())
    }

    fn define_type(&mut self, ty: &mut lume_hir::TypeDefinition) -> Result<()> {
        match ty {
            lume_hir::TypeDefinition::Class(class) => {
                let type_id = class.type_id.unwrap();

                for type_param in &class.type_parameters {
                    type_id.add_type_param(&mut self.ctx.tcx, type_param.name.name.clone());
                }

                for method in &class.methods() {
                    let method_id = method.method_id.unwrap();

                    for type_param in &method.type_parameters {
                        method_id.add_type_param(&mut self.ctx.tcx, type_param.name.name.clone());
                    }
                }

                for method in &class.external_methods() {
                    let method_id = method.method_id.unwrap();

                    for type_param in &method.type_parameters {
                        method_id.add_type_param(&mut self.ctx.tcx, type_param.name.name.clone());
                    }
                }
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                let type_id = trait_def.type_id.unwrap();

                for type_param in &trait_def.type_parameters {
                    type_id.add_type_param(&mut self.ctx.tcx, type_param.name.name.clone());
                }

                for method in &trait_def.methods {
                    let method_id = method.method_id.unwrap();

                    for type_param in &method.type_parameters {
                        method_id.add_type_param(&mut self.ctx.tcx, type_param.name.name.clone());
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

struct DefineTypeConstraints<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineTypeConstraints<'_> {
    fn run_all<'a>(hir: &'a mut lume_hir::map::Map, ctx: &'a mut ThirBuildCtx) -> Result<()> {
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

        for type_param in &func.type_parameters {
            func_id.add_type_param(&mut self.ctx.tcx, type_param.name.name.clone());
        }

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
                let lowered_type_constraint = self.ctx.lower_type_ref(&type_constraint);

                type_param_id.add_constraint(&mut self.ctx.tcx, lowered_type_constraint);
            }
        }

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
                    let property_id = property.prop_id.unwrap();

                    let property_type = self.ctx.lower_type_ref(&property.property_type);
                    property_id.set_property_type(&mut self.ctx.tcx, property_type);
                }

                for method in class.methods() {
                    let method_id = method.method_id.unwrap();

                    for param in &method.parameters {
                        let name = param.name.name.clone();
                        let type_ref = self.ctx.lower_type_ref(&param.param_type);

                        method_id.add_parameter(&mut self.ctx.tcx, name, type_ref);
                    }

                    let return_type = self.ctx.lower_type_ref(&method.return_type);
                    method_id.set_return_type(&mut self.ctx.tcx, return_type);
                }

                for method in class.external_methods() {
                    let method_id = method.method_id.unwrap();

                    for param in &method.parameters {
                        let name = param.name.name.clone();
                        let type_ref = self.ctx.lower_type_ref(&param.param_type);

                        method_id.add_parameter(&mut self.ctx.tcx, name, type_ref);
                    }

                    let return_type = self.ctx.lower_type_ref(&method.return_type);
                    method_id.set_return_type(&mut self.ctx.tcx, return_type);
                }
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                for method in &trait_def.methods {
                    let method_id = method.method_id.unwrap();

                    for param in &method.parameters {
                        let name = param.name.name.clone();
                        let type_ref = self.ctx.lower_type_ref(&param.param_type);

                        method_id.add_parameter(&mut self.ctx.tcx, name, type_ref);
                    }

                    let return_type = self.ctx.lower_type_ref(&method.return_type);
                    method_id.set_return_type(&mut self.ctx.tcx, return_type);
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
            let type_ref = self.ctx.lower_type_ref(&param.param_type);

            function_id.add_parameter(&mut self.ctx.tcx, name, type_ref);
        }

        let return_type = self.ctx.lower_type_ref(&func.return_type);
        function_id.set_return_type(&mut self.ctx.tcx, return_type);

        Ok(())
    }
}
