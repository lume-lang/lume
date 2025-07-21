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

        self.dcx().ensure_untainted()?;

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
                    reason_loc: to.location,
                    found_loc: from.location,
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
        if self.is_trait(to)? {
            if self.trait_impl_by(to, from)? {
                return Ok(());
            }

            tracing::debug!(target: "type_compat", ?from, ?to, "trait not implemented");

            return Err(errors::TraitNotImplemented {
                location: from.location,
                trait_name: self.new_named_type(to)?,
                type_name: self.new_named_type(from)?,
            }
            .into());
        }

        // If `to` refers to a type parameter, check if `from` satisfies the constraints.
        if let Some(to_arg) = self.as_type_parameter(to)? {
            tracing::debug!(target: "type_compat", ?from, ?to_arg, "checking type parameter constraints");

            for constraint in &to_arg.constraints {
                if !self.check_type_compatibility(from, constraint)? {
                    return Err(crate::query::diagnostics::TypeParameterConstraintUnsatisfied {
                        source: from.location,
                        constraint_loc: constraint.location,
                        param_name: to_arg.name.clone(),
                        type_name: self.new_named_type(from)?,
                        constraint_name: self.new_named_type(constraint)?,
                    }
                    .into());
                }
            }

            tracing::debug!(target: "type_compat", "type parameter constraints valid");
            return Ok(());
        }

        // If the two types share the same elemental type, the type arguments
        // may be compatible.
        if from.instance_of == to.instance_of && from.type_arguments.len() == to.type_arguments.len() {
            tracing::debug!(target: "type_compat", ?from, ?to, "checking type argument downcast");

            for (from_arg, to_arg) in from.type_arguments.iter().zip(to.type_arguments.iter()) {
                self.ensure_type_compatibility(from_arg, to_arg)?;
            }

            tracing::debug!(target: "type_compat", "type downcast to type parameter");
            return Ok(());
        }

        if tracing::event_enabled!(tracing::Level::DEBUG) {
            let named_from = self.new_named_type(from)?;
            let named_to = self.new_named_type(to)?;

            tracing::debug!(
                target: "type_compat",
                %named_from,
                %named_to,
                "type-checking failed",
            );

            if tracing::event_enabled!(tracing::Level::TRACE) {
                tracing::trace!(target: "type_compat", ?from, ?to, "expanded type info");
            }
        }

        Err(errors::MismatchedTypes {
            reason_loc: to.location,
            found_loc: from.location,
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
