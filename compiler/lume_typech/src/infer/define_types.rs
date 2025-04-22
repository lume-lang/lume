use lume_diag::Result;
use lume_hir::{self};
use lume_types::*;

use crate::*;

pub(super) struct DefineTypes<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineTypes<'_> {
    pub(super) fn run_all<'a>(hir: &'a mut lume_hir::map::Map, ctx: &'a mut ThirBuildCtx) -> Result<()> {
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
