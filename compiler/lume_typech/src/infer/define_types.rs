use lume_hir::{self};
use lume_types::*;

use crate::ThirBuildCtx;

pub(super) struct DefineTypes<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineTypes<'_> {
    pub(super) fn run_all(ctx: &mut ThirBuildCtx, hir: &mut lume_hir::map::Map) {
        let mut define = DefineTypes { ctx };

        define.run(hir);
    }

    fn run(&mut self, hir: &mut lume_hir::map::Map) {
        for (_, symbol) in &mut hir.items {
            match symbol {
                lume_hir::Symbol::Type(t) => self.define_type(t),
                lume_hir::Symbol::Impl(i) => self.define_impl(i),
                lume_hir::Symbol::Function(f) => self.define_function(f),
                lume_hir::Symbol::Use(_) => (),
            }
        }
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

    fn define_impl(&mut self, implementation: &mut lume_hir::Implementation) {
        let target = implementation.target.name.clone();
        let impl_id = self.ctx.tcx_mut().impl_alloc(target);

        implementation.impl_id = Some(impl_id);
    }

    fn define_function(&mut self, func: &mut lume_hir::FunctionDefinition) {
        let name = func.name.clone();
        let visibility = func.visibility;
        let func_id = self.ctx.tcx_mut().func_alloc(name, visibility);

        func.func_id = Some(func_id);
    }
}
