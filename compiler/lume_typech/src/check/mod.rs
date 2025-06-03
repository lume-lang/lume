use error_snippet::Result;
use lume_types::TypeRef;

use crate::ThirBuildCtx;

pub(crate) mod errors;
pub(crate) mod expressions;
mod lookup;

/// Represents a single pass which will be executed when invoking
/// the type checker on a given [`ThirBuildCtx`]-instance.
pub trait TypeCheckerPass {
    fn run(tcx: &mut ThirBuildCtx) -> Result<()>
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
    #[tracing::instrument(
        level = "INFO",
        name = "lume_typech::ThirBuildCtx::typecheck",
        parent = None,
        skip(self),
        err
    )]
    pub fn typecheck(&mut self) -> Result<()> {
        expressions::Expressions::run(self)?;

        Ok(())
    }

    /// Checks whether the given type references are compatible.
    ///
    /// # Errors
    ///
    /// Returns `Err` when expected items cannot be found within the context.
    #[tracing::instrument(level = "TRACE", skip(self), err, ret)]
    pub(crate) fn check_type_compatibility(&self, from: &TypeRef, to: &TypeRef) -> Result<bool> {
        // If the two given types are exactly the same, both underlying instance and type arguments,
        // we can be sure they're compatible.
        if from == to {
            return Ok(true);
        }

        // Special case for `void` types, since they are always identical, no matter
        // whether they have different underlying IDs.
        match (self.is_void(from)?, self.is_void(to)?) {
            // void => value OR value => void
            (false, true) | (true, false) => return Ok(false),

            // void == void
            (true, true) => return Ok(true),

            // value => value
            (false, false) => (),
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
