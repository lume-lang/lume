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
        self.typech_expressions()?;
        self.typech_traits()?;

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

        if from.type_params.len() != to.type_params.len() {
            return Ok(false);
        }

        for (from_param_id, to_param_id) in from.type_params.iter().zip(to.type_params.iter()) {
            let from_param = self.tdb().type_parameter(*from_param_id).unwrap();
            let to_param = self.tdb().type_parameter(*to_param_id).unwrap();

            if from_param.constraints.len() != to_param.constraints.len() {
                return Ok(false);
            }

            for (from_constraint, to_constraint) in from_param.constraints.iter().zip(to_param.constraints.iter()) {
                if !self.check_type_compatibility(from_constraint, to_constraint)? {
                    return Ok(false);
                }
            }
        }

        if from.params.len() != to.params.len() {
            return Ok(false);
        }

        for (from_param, to_param) in from.params.inner().iter().zip(to.params.inner().iter()) {
            if from_param.name == "self" && to_param.name == "self" {
                continue;
            }

            if from_param.vararg != to_param.vararg || !self.check_type_compatibility(&from_param.ty, &to_param.ty)? {
                return Ok(false);
            }
        }

        if !self.check_type_compatibility(from.ret_ty, to.ret_ty)? {
            return Ok(false);
        }

        Ok(true)
    }
}
