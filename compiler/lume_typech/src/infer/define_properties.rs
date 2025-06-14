use error_snippet::Result;
use lume_hir::{self};

use crate::ThirBuildCtx;

pub(super) struct DefineProperties<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefineProperties<'_> {
    #[tracing::instrument(level = "DEBUG", name = "DefineProperties::run_all", skip_all, err)]
    pub(super) fn run_all(ctx: &mut ThirBuildCtx) -> Result<()> {
        let mut hir = std::mem::take(&mut ctx.hir);
        let mut pass = DefineProperties { ctx };

        for (_, symbol) in &mut hir.items {
            if let lume_hir::Item::Type(ty) = symbol {
                pass.define_type(ty)?;
            }
        }

        ctx.hir = hir;

        Ok(())
    }

    fn define_type(&mut self, ty: &mut lume_hir::TypeDefinition) -> Result<()> {
        if let lume_hir::TypeDefinition::Struct(struct_def) = ty {
            let type_id = struct_def.type_id.unwrap();

            for property in &mut struct_def.properties_mut() {
                let property_name = property.name.name.clone();
                let visibility = property.visibility;

                let property_id = self
                    .ctx
                    .tdb_mut()
                    .property_alloc(type_id, property_name.clone(), visibility)?;

                property.prop_id = Some(property_id);
            }
        }

        Ok(())
    }
}
