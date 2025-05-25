use error_snippet::Result;
use lume_types::TypeRef;

use crate::ThirBuildCtx;

pub(crate) mod errors;
mod lookup;

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
    pub(crate) fn check_type_compatibility(&self, from: &TypeRef, to: &TypeRef) -> Result<bool> {
        // If the two given types are exactly the same, both underlying instance and type arguments,
        // we can be sure they're compatible.
        if from == to {
            return Ok(true);
        }

        // Special case for `void` types, since they are always identical, no matter
        // whether they have different underlying IDs.
        if self.is_void(from)? && self.is_void(to)? {
            return Ok(true);
        }

        // If `to` refers to a trait where `from` implements `to`, they can
        // be downcast correctly.
        if self.is_trait(to)? && self.trait_impl_by(to, from)? {
            return Ok(true);
        }

        println!("typech: {from:#?} {to:#?}");

        Ok(false)
    }
}
