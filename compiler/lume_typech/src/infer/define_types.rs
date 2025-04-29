use lume_diag::Result;
use lume_hir::{self};
use lume_types::*;

use crate::ThirBuildCtx;

pub(super) struct DefineTypes<'a, 'b> {
    ctx: &'a mut ThirBuildCtx<'b>,
}

impl DefineTypes<'_, '_> {
    pub(super) fn run_all<'a>(ctx: &mut ThirBuildCtx<'a>, hir: &mut lume_hir::map::Map) -> Result<()> {
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
                let type_id = Type::alloc(self.ctx.tcx_mut(), name, kind);

                for property in &mut class.properties_mut() {
                    let property_name = property.name.name.clone();
                    let visibility = property.visibility;
                    let property_id = Property::alloc(self.ctx.tcx_mut(), type_id, property_name, visibility);

                    property.prop_id = Some(property_id);
                }

                for method in &mut class.methods_mut() {
                    let method_name = &method.name;
                    let visibility = method.visibility;
                    let method_id = Method::alloc(self.ctx.tcx_mut(), type_id, method_name.clone(), visibility);

                    method.method_id = Some(method_id);

                    type_id
                        .get_mut(self.ctx.tcx_mut())
                        .methods
                        .insert(method_name.name.clone(), method_id);
                }

                for method in &mut class.external_methods_mut() {
                    let method_name = &method.name;
                    let visibility = method.visibility;
                    let method_id = Method::alloc(self.ctx.tcx_mut(), type_id, method_name.clone(), visibility);

                    method.method_id = Some(method_id);

                    type_id
                        .get_mut(self.ctx.tcx_mut())
                        .methods
                        .insert(method_name.name.clone(), method_id);
                }

                class.type_id = Some(type_id);
            }
            lume_hir::TypeDefinition::Alias(alias) => {
                let name = alias.name.clone();
                let kind = TypeKind::Alias(Box::new(Alias::new(name.clone())));
                let type_id = Type::alloc(self.ctx.tcx_mut(), name, kind);

                alias.type_id = Some(type_id);
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                let name = trait_def.name.clone();
                let kind = TypeKind::Trait(Box::new(Trait::new(name.clone())));
                let type_id = Type::alloc(self.ctx.tcx_mut(), name, kind);

                for method in &mut trait_def.methods {
                    let method_name = &method.name;
                    let visibility = method.visibility;
                    let method_id = Method::alloc(self.ctx.tcx_mut(), type_id, method_name.clone(), visibility);

                    method.method_id = Some(method_id);

                    type_id
                        .get_mut(self.ctx.tcx_mut())
                        .methods
                        .insert(method_name.name.clone(), method_id);
                }

                trait_def.type_id = Some(type_id);
            }
            lume_hir::TypeDefinition::Enum(enum_def) => {
                let name = enum_def.name.clone();
                let kind = TypeKind::Enum(Box::new(Enum::new(name.clone())));
                let type_id = Type::alloc(self.ctx.tcx_mut(), name, kind);

                enum_def.type_id = Some(type_id);
            }
        };

        Ok(())
    }

    fn define_function(&mut self, func: &mut lume_hir::FunctionDefinition) -> Result<()> {
        let name = func.name.clone();
        let visibility = func.visibility;
        let type_id = Function::alloc(self.ctx.tcx_mut(), name, visibility);

        func.func_id = Some(type_id);

        Ok(())
    }
}
