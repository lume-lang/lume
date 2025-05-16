use lume_hir::{self};
use lume_types::*;

use crate::ThirBuildCtx;

pub(super) struct DefineFields<'a, 'b> {
    ctx: &'a mut ThirBuildCtx<'b>,
}

impl DefineFields<'_, '_> {
    pub(super) fn run_all(ctx: &mut ThirBuildCtx<'_>, hir: &mut lume_hir::map::Map) {
        let mut define = DefineFields { ctx };

        define.run(hir);
    }

    fn run(&mut self, hir: &mut lume_hir::map::Map) {
        for (_, symbol) in &mut hir.items {
            if let lume_hir::Symbol::Type(t) = symbol {
                self.define_type(t);
            }
        }
    }

    fn define_type(&mut self, ty: &mut lume_hir::TypeDefinition) {
        match ty {
            lume_hir::TypeDefinition::Struct(struct_def) => {
                let type_id = struct_def.type_id.unwrap();

                for property in &mut struct_def.properties_mut() {
                    let property_name = property.name.name.clone();
                    let visibility = property.visibility;
                    let property_id = Property::alloc(self.ctx.tcx_mut(), type_id, property_name.clone(), visibility);

                    property.prop_id = Some(property_id);

                    type_id
                        .get_mut(self.ctx.tcx_mut())
                        .properties
                        .insert(property_name, property_id);
                }

                for method in &mut struct_def.methods_mut() {
                    let method_name = &method.name;
                    let visibility = method.visibility;
                    let method_id = Method::alloc(self.ctx.tcx_mut(), type_id, method_name.clone(), visibility);

                    method.method_id = Some(method_id);

                    type_id
                        .get_mut(self.ctx.tcx_mut())
                        .methods
                        .insert(method_name.name.clone(), method_id);
                }
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                let type_id = trait_def.type_id.unwrap();

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
            }
            _ => {}
        }
    }
}
