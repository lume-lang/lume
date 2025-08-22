use error_snippet::Result;
use lume_types::{TypeKind, TypeRef, UserType};

use crate::TyCheckCtx;

impl TyCheckCtx {
    /// Determines whether the given [`TypeRef`] is a kind of [`TypeKindRef::Trait`].
    #[tracing::instrument(level = "TRACE", skip(self), err, ret)]
    pub fn is_trait(&self, ty: &TypeRef) -> Result<bool> {
        match self.tdb().ty_expect(ty.instance_of)?.kind {
            TypeKind::User(UserType::Trait(_)) => Ok(true),
            _ => Ok(false),
        }
    }

    /// Determines whether the given [`TypeRef`] is a kind of [`TypeKindRef::Void`].
    #[tracing::instrument(level = "TRACE", skip(self), err, ret)]
    pub fn is_void(&self, ty: &TypeRef) -> Result<bool> {
        match self.tdb().ty_expect(ty.instance_of)?.kind {
            TypeKind::Void => Ok(true),
            _ => Ok(false),
        }
    }

    /// Determines whether the given [`TypeRef`], `ty`, implements the given trait, `trait_id`.
    #[tracing::instrument(level = "TRACE", skip(self), err, ret)]
    pub fn trait_impl_by(&self, trait_id: &TypeRef, ty: &TypeRef) -> Result<bool> {
        if let Some(type_param) = self.tdb().type_as_param(ty.instance_of) {
            for constraint in &type_param.constraints {
                if self.check_type_compatibility(constraint, trait_id)? {
                    return Ok(true);
                }
            }
        }

        for use_ in self.tdb().uses_on(ty) {
            if &use_.trait_ == trait_id {
                // Make sure the given "trait" is actually a trait.
                self.tdb().ty_expect_trait(trait_id.instance_of)?;

                return Ok(true);
            }
        }

        Ok(false)
    }
}
