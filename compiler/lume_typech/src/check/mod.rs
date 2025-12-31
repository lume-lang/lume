use error_snippet::Result;
use lume_types::FunctionSig;

use crate::TyCheckCtx;

pub(crate) mod errors;
pub(crate) mod expressions;
pub(crate) mod traits;

impl TyCheckCtx {
    /// Performs type-checking on all the items in the context, after they've
    /// been inferred in a previous stage.
    ///
    /// # Errors
    ///
    /// Returns `Err` when either a language error occured, such as missing
    /// variables, missing methods, etc, or when expected items cannot be
    /// found within the context.
    #[libftrace::traced(level = Info, err)]
    pub fn typecheck(&mut self) -> Result<()> {
        self.typech_expressions();
        self.typech_traits();

        self.dcx().ensure_untainted()?;

        Ok(())
    }

    /// Checks whether the given signatures are compatible. This method
    /// expects the signatures to already be instantiated.
    ///
    /// **Note:** since [`FunctionSig`] don't contain the names of the functions
    /// they represent, the equality of the names are not tested.
    ///
    /// # Errors
    ///
    /// Returns `Err` when expected items cannot be found within the context.
    #[libftrace::traced(level = Trace, err, ret)]
    pub(crate) fn check_signature_compatibility(&self, from: FunctionSig<'_>, to: FunctionSig<'_>) -> Result<bool> {
        // If the two given signatures are exactly the same, both underlying instance
        // and type arguments, we can be sure they're compatible.
        if from == to {
            return Ok(true);
        }

        Ok(self.check_type_parameter_compatibility(from, to)?
            && self.check_parameter_compatibility(from, to)?
            && self.check_return_type_compatibility(from, to)?)
    }

    #[libftrace::traced(level = Trace, err, ret)]
    fn check_type_parameter_compatibility(&self, from: FunctionSig<'_>, to: FunctionSig<'_>) -> Result<bool> {
        if from.type_params.len() != to.type_params.len() {
            return Ok(false);
        }

        for (from_param_id, to_param_id) in from.type_params.iter().zip(to.type_params.iter()) {
            let from_param = self.hir_expect_type_parameter(*from_param_id);
            let to_param = self.hir_expect_type_parameter(*to_param_id);

            if from_param.constraints.len() != to_param.constraints.len() {
                return Ok(false);
            }

            for (from_constraint, to_constraint) in from_param.constraints.iter().zip(to_param.constraints.iter()) {
                let from_constraint = self.mk_type_ref_from(from_constraint, from_param.id)?;
                let to_constraint = self.mk_type_ref_from(to_constraint, to_param.id)?;

                if !self.check_type_compatibility(&from_constraint, &to_constraint)? {
                    return Ok(false);
                }
            }
        }

        Ok(true)
    }

    #[libftrace::traced(level = Trace, err, ret)]
    fn check_parameter_compatibility(&self, from: FunctionSig<'_>, to: FunctionSig<'_>) -> Result<bool> {
        if from.params.len() != to.params.len() {
            return Ok(false);
        }

        for (from_param, to_param) in from.params.iter().zip(to.params.iter()) {
            // Types of `self` will almost always refer to separate types.
            if from_param.is_self() && to_param.is_self() {
                continue;
            }

            // If both return types refer to `Self` or are otherwise equivalent to `Self`,
            // return true.
            if self.is_self_type_within(&from_param.ty, from.id)? && self.is_self_type_within(&to_param.ty, to.id)? {
                return Ok(true);
            }

            if from_param.vararg != to_param.vararg || !self.check_type_compatibility(&from_param.ty, &to_param.ty)? {
                return Ok(false);
            }
        }

        Ok(true)
    }

    #[libftrace::traced(level = Trace, err, ret)]
    fn check_return_type_compatibility(&self, from: FunctionSig<'_>, to: FunctionSig<'_>) -> Result<bool> {
        // If both return types refer to `Self` or are otherwise equivalent to `Self`,
        // return true.
        if self.is_self_type_within(from.ret_ty, from.id)? && self.is_self_type_within(to.ret_ty, to.id)? {
            return Ok(true);
        }

        self.check_type_compatibility(from.ret_ty, to.ret_ty)
    }
}
