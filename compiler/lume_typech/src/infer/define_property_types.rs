use error_snippet::Result;
use lume_hir::{self};

use crate::ThirBuildCtx;

pub(super) struct DefinePropertyTypes<'a> {
    ctx: &'a mut ThirBuildCtx,
}

impl DefinePropertyTypes<'_> {
    pub(super) fn run_all(ctx: &mut ThirBuildCtx) -> Result<()> {
        let hir = std::mem::take(&mut ctx.hir);
        let mut define = DefinePropertyTypes { ctx };

        for (_, symbol) in &hir.items {
            if let lume_hir::Item::Type(ty) = symbol {
                define.define_type(ty)?;
            }
        }

        ctx.hir = hir;

        Ok(())
    }

    fn define_type(&mut self, ty: &lume_hir::TypeDefinition) -> Result<()> {
        if let lume_hir::TypeDefinition::Struct(struct_def) = ty {
            for property in struct_def.properties() {
                let property_id = property.prop_id.unwrap();

                let type_ref = self
                    .ctx
                    .mk_type_ref_generic(&property.property_type, &struct_def.type_parameters)?;

                self.ctx.tcx_mut().property_mut(property_id).unwrap().property_type = type_ref;
            }
        }

        Ok(())
    }
}
