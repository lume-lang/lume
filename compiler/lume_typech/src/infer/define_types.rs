use error_snippet::Result;
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
                lume_hir::Symbol::Impl(i) => self.define_impl(i)?,
                lume_hir::Symbol::Function(f) => self.define_function(f)?,
                _ => (),
            }
        }

        Ok(())
    }

    fn define_type(&mut self, ty: &mut lume_hir::TypeDefinition) -> Result<()> {
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
        };

        Ok(())
    }

    fn define_impl(&mut self, implementation: &mut lume_hir::Implementation) -> Result<()> {
        let target = implementation.target.name.clone();
        let impl_id = Implementation::alloc(self.ctx.tcx_mut(), target);

        implementation.impl_id = Some(impl_id);

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
