use error_snippet::Result;
use lume_types::TypeRef;

use crate::TyCheckCtx;

pub(crate) mod errors;
pub(crate) mod expressions;
pub(crate) mod lookup;
pub(crate) mod traits;

impl TyCheckCtx {
    /// Performs type-checking on all the items in the context, after they've
    /// been inferred in a previous stage.
    ///
    /// # Errors
    ///
    /// Returns `Err` when either a language error occured, such as missing variables, missing methods,
    /// etc, or when expected items cannot be found within the context.
    #[tracing::instrument(
        level = "INFO",
        name = "lume_typech::TyCheckCtx::typecheck",
        parent = None,
        skip(self),
        err
    )]
    pub fn typecheck(&mut self) -> Result<()> {
        self.typech_expressions()?;
        self.typech_traits()?;

        Ok(())
    }

    /// Ensures that the given type references are compatible.
    ///
    /// # Errors
    ///
    /// Returns `Err` when the given types are incompatible or
    /// if expected items cannot be found within the context.
    #[tracing::instrument(level = "TRACE", skip(self), err, ret)]
    pub(crate) fn ensure_type_compatibility(&self, from: &TypeRef, to: &TypeRef) -> Result<()> {
        // If the two given types are exactly the same, both underlying instance and type arguments,
        // we can be sure they're compatible.
        if from == to {
            return Ok(());
        }

        // Special case for `void` types, since they are always identical, no matter
        // whether they have different underlying IDs.
        match (self.is_void(from)?, self.is_void(to)?) {
            // void => value OR value => void
            (false, true) | (true, false) => {
                return Err(errors::MismatchedTypes {
                    reason_loc: to.location.clone(),
                    found_loc: from.location.clone(),
                    expected: self.infer.new_named_type(to)?,
                    found: self.infer.new_named_type(from)?,
                }
                .into());
            }

            // void == void
            (true, true) => return Ok(()),

            // value => value
            (false, false) => (),
        }

        // If `to` refers to a trait where `from` implements `to`, they can
        // be downcast correctly.
        if self.is_trait(to)? && self.trait_impl_by(to, from)? {
            if self.trait_impl_by(to, from)? {
                return Ok(());
            }

            return Err(errors::TraitNotImplemented {
                location: from.location.clone(),
                trait_name: self.infer.new_named_type(from)?,
                type_name: self.infer.new_named_type(to)?,
            }
            .into());
        }

        if log::log_enabled!(log::Level::Debug) {
            log::debug!(
                "type-checking failed: {} => {}",
                self.infer.new_named_type(from)?,
                self.infer.new_named_type(to)?
            );

            if log::log_enabled!(log::Level::Trace) {
                log::trace!("expanded type info: {from:?} => {to:?}");
            }
        }

        Err(errors::MismatchedTypes {
            reason_loc: to.location.clone(),
            found_loc: from.location.clone(),
            expected: self.infer.new_named_type(to)?,
            found: self.infer.new_named_type(from)?,
        }
        .into())
    }

    /// Checks whether the given type references are compatible.
    ///
    /// # Errors
    ///
    /// Returns `Err` when expected items cannot be found within the context.
    #[tracing::instrument(level = "TRACE", skip(self), err, ret)]
    pub(crate) fn check_type_compatibility(&self, from: &TypeRef, to: &TypeRef) -> Result<bool> {
        if let Err(err) = self.ensure_type_compatibility(from, to) {
            // Type errors will have a code attached - compiler errors will not.
            if err.code().is_none() {
                return Err(err);
            }

            Ok(false)
        } else {
            Ok(true)
        }
    }
}
