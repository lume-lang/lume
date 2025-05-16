use lume_hir::{self};
use lume_types::*;

use crate::ThirBuildCtx;

pub(super) struct DefineTypes<'a, 'b> {
    ctx: &'a mut ThirBuildCtx<'b>,
}

impl DefineTypes<'_, '_> {
    pub(super) fn run_all(ctx: &mut ThirBuildCtx<'_>, hir: &mut lume_hir::map::Map) {
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
                let kind = TypeKind::Struct(Box::new(Struct::new(name.clone())));
                let type_id = Type::alloc(self.ctx.tcx_mut(), name, kind);

                struct_def.type_id = Some(type_id);

                if struct_def.builtin {
                    type_id.set_copied(self.ctx.tcx_mut());
                }
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

                trait_def.type_id = Some(type_id);
            }
            lume_hir::TypeDefinition::Enum(enum_def) => {
                let name = enum_def.name.clone();
                let kind = TypeKind::Enum(Box::new(Enum::new(name.clone())));
                let type_id = Type::alloc(self.ctx.tcx_mut(), name, kind);

                enum_def.type_id = Some(type_id);
            }
        }
    }

    fn define_impl(&mut self, implementation: &mut lume_hir::Implementation) {
        let target = implementation.target.name.clone();
        let impl_id = Implementation::alloc(self.ctx.tcx_mut(), target);

        implementation.impl_id = Some(impl_id);
    }

    fn define_function(&mut self, func: &mut lume_hir::FunctionDefinition) {
        let name = func.name.clone();
        let visibility = func.visibility;
        let type_id = Function::alloc(self.ctx.tcx_mut(), name, visibility);

        func.func_id = Some(type_id);
    }
}
