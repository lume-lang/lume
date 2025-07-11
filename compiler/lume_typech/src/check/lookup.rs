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

    /// Determines whether the given [`TypeRef`] is a kind of [`TypeKindRef::TypeParameter`].
    #[tracing::instrument(level = "TRACE", skip(self), err, ret)]
    pub fn as_type_parameter(&self, ty: &TypeRef) -> Result<Option<&lume_types::TypeParameter>> {
        match self.tdb().ty_expect(ty.instance_of)?.kind {
            TypeKind::TypeParameter(id) => Ok(self.tdb().type_parameter(id)),
            _ => Ok(None),
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
