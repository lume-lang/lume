use error_snippet::Result;
use lume_hir::{self, PathSegment, SymbolName};
use lume_types::TypeRef;

use crate::ThirBuildCtx;

pub(super) struct DefineTraitMethods<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineTraitMethods<'_> {
    #[tracing::instrument(level = "DEBUG", name = "DefineTraitMethods::run_all", skip_all, err)]
    pub(super) fn run_all(ctx: &mut ThirBuildCtx) -> Result<()> {
        let mut hir = std::mem::take(&mut ctx.hir);
        let mut define = DefineTraitMethods { ctx };

        for (_, symbol) in &mut hir.items {
            match symbol {
                lume_hir::Item::Type(ty) => {
                    if let lume_hir::TypeDefinition::Trait(tr) = &mut **ty {
                        define.define_trait_def(tr)?;
                    }
                }
                lume_hir::Item::Use(u) => define.define_trait_impl(u)?,
                _ => (),
            }
            if let lume_hir::Item::Use(t) = symbol {
                define.define_trait_impl(t)?;
            }
        }

        ctx.hir = hir;

        Ok(())
    }

    fn define_trait_def(&mut self, trait_def: &mut lume_hir::TraitDefinition) -> Result<()> {
        let type_id = trait_def.type_id.unwrap();
        let type_ref = TypeRef::new(type_id, trait_def.location.clone());

        for method in &mut trait_def.methods {
            let method_name = method.name.clone();
            let mut qualified_name =
                SymbolName::with_root(trait_def.name.clone(), PathSegment::Named(method_name.clone()));

            qualified_name.location = method_name.location.clone();

            let method_id = self
                .ctx
                .tdb_mut()
                .method_alloc(type_ref.clone(), qualified_name, method.visibility)?;

            method.method_id = Some(method_id);
        }

        Ok(())
    }

    fn define_trait_impl(&mut self, trait_impl: &mut lume_hir::TraitImplementation) -> Result<()> {
        let type_ref = self.ctx.mk_type_ref(trait_impl.target.as_ref())?;

        for method in &mut trait_impl.methods {
            let method_name = method.name.clone();
            let mut qualified_name =
                SymbolName::with_root(trait_impl.target.name.clone(), PathSegment::Named(method_name.clone()));

            qualified_name.location = method_name.location.clone();

            let method_id = self
                .ctx
                .tdb_mut()
                .method_alloc(type_ref.clone(), qualified_name, method.visibility)?;

            method.method_id = Some(method_id);
        }

        Ok(())
    }
}
