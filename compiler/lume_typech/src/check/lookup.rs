use error_snippet::Result;
use lume_types::{TypeKindRef, TypeRef};

use crate::ThirBuildCtx;

impl ThirBuildCtx {
    /// Determines whether the given [`TypeRef`] is a kind of [`TypeKindRef::Trait`].
    #[tracing::instrument(level = "TRACE", skip(self), err, ret)]
    pub(crate) fn is_trait(&self, ty: &TypeRef) -> Result<bool> {
        match self.tcx.ty_expect(ty.instance_of)?.kind {
            TypeKindRef::Trait(_) => Ok(true),
            _ => Ok(false),
        }
    }

    /// Determines whether the given [`TypeRef`] is a kind of [`TypeKindRef::Void`].
    #[tracing::instrument(level = "TRACE", skip(self), err, ret)]
    pub(crate) fn is_void(&self, ty: &TypeRef) -> Result<bool> {
        match self.tcx.ty_expect(ty.instance_of)?.kind {
            TypeKindRef::Void => Ok(true),
            _ => Ok(false),
        }
    }

    /// Determines whether the given [`TypeRef`], `ty`, implements the given trait, `trait_id`.
    #[tracing::instrument(level = "TRACE", skip(self), err, ret)]
    pub(crate) fn trait_impl_by(&self, trait_id: &TypeRef, ty: &TypeRef) -> Result<bool> {
        for use_ in self.tcx.uses_on(ty) {
            if &use_.trait_ == trait_id {
                // Make sure the given "trait" is actually a trait.
                self.tcx.ty_expect_trait(trait_id.instance_of)?;

                return Ok(true);
            }
        }

        Ok(false)
    }
}
