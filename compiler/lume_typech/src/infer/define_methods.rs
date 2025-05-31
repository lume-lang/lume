use error_snippet::Result;
use lume_hir::{self, PathSegment, SymbolName};
use lume_types::*;

use crate::ThirBuildCtx;

pub(super) struct DefineMethods<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineMethods<'_> {
    pub(super) fn run_all(ctx: &mut ThirBuildCtx) -> Result<()> {
        let mut hir = std::mem::take(&mut ctx.hir);
        let mut define = DefineMethods { ctx };

        for (_, symbol) in &mut hir.items {
            match symbol {
                lume_hir::Symbol::Type(t) => define.define_type(t)?,
                lume_hir::Symbol::Use(t) => define.define_use(t)?,
                _ => (),
            }
        }

        ctx.hir = hir;

        Ok(())
    }

    fn run(&mut self, hir: &mut lume_hir::map::Map) -> Result<()> {
        let hir = std::mem::take(&mut ctx.hir);

        for (_, symbol) in &mut hir.items {
            match symbol {
                lume_hir::Symbol::Type(t) => self.define_type(t)?,
                lume_hir::Symbol::Use(t) => self.define_use(t)?,
                _ => (),
            }
        }

        Ok(())
    }

    fn define_type(&mut self, ty: &mut lume_hir::TypeDefinition) -> Result<()> {
        match ty {
            lume_hir::TypeDefinition::Struct(struct_def) => {
                let type_id = struct_def.type_id.unwrap();
                let type_ref = TypeRef::new(type_id);

                let struct_name = struct_def.name().clone();

                for method in &mut struct_def.methods_mut() {
                    let method_name = &method.name;
                    let visibility = method.visibility;
                    let qualified_name =
                        SymbolName::with_root(struct_name.clone(), PathSegment::Named(method_name.clone()));

                    let method_id = self
                        .ctx
                        .tcx_mut()
                        .method_alloc(type_ref.clone(), qualified_name, visibility)?;

                    method.method_id = Some(method_id);
                }
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                let type_id = trait_def.type_id.unwrap();
                let type_ref = TypeRef::new(type_id);

                for method in &mut trait_def.methods {
                    let method_name = &method.name;
                    let visibility = method.visibility;
                    let qualified_name =
                        SymbolName::with_root(trait_def.name.clone(), PathSegment::Named(method_name.clone()));

                    let method_id = self
                        .ctx
                        .tcx_mut()
                        .method_alloc(type_ref.clone(), qualified_name, visibility)?;

                    method.method_id = Some(method_id);
                }
            }
            _ => {}
        }

        Ok(())
    }

    fn define_use(&mut self, trait_impl: &mut lume_hir::TraitImplementation) -> Result<()> {
        let type_id = self.ctx.tcx().find_type(&trait_impl.target.name).unwrap().id;
        let type_ref = TypeRef::new(type_id);

        let target_name = trait_impl.target.name.clone();

        for method in &mut trait_impl.methods {
            let method_name = &method.name.name;

            let visibility = method.visibility;
            let qualified_name = SymbolName::with_root(target_name.clone(), method_name.clone());

            let method_id = self
                .ctx
                .tcx_mut()
                .method_alloc(type_ref.clone(), qualified_name, visibility)?;

            method.method_id = Some(method_id);
        }

        Ok(())
    }
}
