use lume_diag::Result;
use lume_hir::{self};

use crate::*;

mod define_method_bodies;
mod define_type_constraints;
mod define_type_params;
mod define_types;

impl ThirBuildCtx {
    pub(super) fn define_types(&mut self, hir: &mut lume_hir::map::Map) -> Result<()> {
        infer::define_types::DefineTypes::run_all(hir, self)?;
        infer::define_type_params::DefineTypeParameters::run_all(hir, self)?;
        infer::define_type_constraints::DefineTypeConstraints::run_all(hir, self)?;
        infer::define_method_bodies::DefineMethodBodies::run_all(hir, self)?;

        Ok(())
    }
}
