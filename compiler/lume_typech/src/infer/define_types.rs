use lume_hir::{self};
use lume_types::*;

use crate::ThirBuildCtx;

pub(super) struct DefineTypes<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineTypes<'_> {
    #[tracing::instrument(level = "DEBUG", name = "DefineTypes::run_all", skip_all)]
    pub(super) fn run_all(ctx: &mut ThirBuildCtx) {
        let mut hir = std::mem::take(&mut ctx.hir);
        let mut define = DefineTypes { ctx };

        for (_, symbol) in &mut hir.items {
            if let lume_hir::Item::Type(ty) = symbol {
                define.define_type(ty);
            }
        }

        ctx.hir = hir;
    }

    fn define_type(&mut self, ty: &mut lume_hir::TypeDefinition) {
        match ty {
            lume_hir::TypeDefinition::Struct(struct_def) => {
                let name = struct_def.name.clone();
                let kind = TypeKindRef::Struct(Box::new(Struct::new(struct_def.as_ref())));
                let type_id = self.ctx.tcx_mut().type_alloc(name, kind);

                struct_def.type_id = Some(type_id);

                if struct_def.builtin {
                    self.ctx.tcx_mut().type_mut(type_id).unwrap().transport = TypeTransport::Copy;
                }
            }
            lume_hir::TypeDefinition::Alias(alias) => {
                let name = alias.name.clone();
                let kind = TypeKindRef::Alias(Box::new(Alias::new(alias.as_ref())));
                let type_id = self.ctx.tcx_mut().type_alloc(name, kind);

                alias.type_id = Some(type_id);
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                let name = trait_def.name.clone();
                let kind = TypeKindRef::Trait(Box::new(Trait::new(trait_def.as_ref())));
                let type_id = self.ctx.tcx_mut().type_alloc(name, kind);

                trait_def.type_id = Some(type_id);
            }
            lume_hir::TypeDefinition::Enum(enum_def) => {
                let name = enum_def.name.clone();
                let kind = TypeKindRef::Enum(Box::new(Enum::new(enum_def.as_ref())));
                let type_id = self.ctx.tcx_mut().type_alloc(name, kind);

                enum_def.type_id = Some(type_id);
            }
        }
    }
}
