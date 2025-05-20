use error_snippet::Result;
use lume_types::TypeRef;

use crate::ThirBuildCtx;

pub(crate) mod errors;

pub trait TypeCheckerPass<'a> {
    fn run(tcx: &'a mut ThirBuildCtx, hir: &'a lume_hir::map::Map) -> Result<()>
    where
        Self: Sized;
}

impl ThirBuildCtx {
    /// Performs type-checking on all the items in the context, after they've
    /// been inferred in a previous stage.
    ///
    /// # Errors
    ///
    /// Returns `Err` when either a language error occured, such as missing variables, missing methods,
    /// etc, or when expected items cannot be found within the context.
    pub fn typecheck(&mut self, _hir: &lume_hir::map::Map) -> Result<()> {
        Ok(())
    }

    /// Checks whether the given type references are compatible.
    ///
    /// # Errors
    ///
    /// Returns `Err` when expected items cannot be found within the context.
    #[expect(clippy::unused_self, clippy::unnecessary_wraps, reason = "unimplemented")]
    pub(crate) fn check_type_compatibility(&self, _from: &TypeRef, _to: &TypeRef) -> Result<bool> {
        Ok(true)
    }
}
