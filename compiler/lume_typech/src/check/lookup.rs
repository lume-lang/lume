use error_snippet::Result;
use lume_types::{TypeKind, TypeRef, Use, UserType};

use crate::TyCheckCtx;

impl TyCheckCtx {
    /// Determines whether the given [`TypeRef`] is a kind of
    /// [`TypeKindRef::Struct`].
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn is_struct(&self, ty: &TypeRef) -> Result<bool> {
        match self.tdb().ty_expect(ty.instance_of)?.kind {
            TypeKind::User(UserType::Struct(_)) => Ok(true),
            _ => Ok(false),
        }
    }

    /// Determines whether the given [`TypeRef`] is a kind of
    /// [`TypeKindRef::Trait`].
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn is_trait(&self, ty: &TypeRef) -> Result<bool> {
        match self.tdb().ty_expect(ty.instance_of)?.kind {
            TypeKind::User(UserType::Trait(_)) => Ok(true),
            _ => Ok(false),
        }
    }

    /// Determines whether the given [`TypeRef`], `ty`, implements the given
    /// trait, `trait_id`.
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn trait_impl_by(&self, trait_id: &TypeRef, ty: &TypeRef) -> Result<bool> {
        #[cfg(debug_assertions)]
        self.tdb().ty_expect_trait(trait_id.instance_of)?;

        if let Some(type_param) = self.tdb().type_as_param(ty.instance_of) {
            for constraint in &type_param.constraints {
                if self.check_type_compatibility(constraint, trait_id)? {
                    return Ok(true);
                }
            }
        }

        // Check for direct trait implementations on the target type.
        for use_ in self.tdb().uses_on(ty) {
            if &use_.trait_ == trait_id {
                // Make sure the given "trait" is actually a trait.
                self.tdb().ty_expect_trait(use_.trait_.instance_of)?;

                return Ok(true);
            }
        }

        // If no direct implementations exist, attempt to find any blanket
        // implementations which might encapsulate the target type.
        for blanket_impl in self.trait_blanket_impls(trait_id) {
            if self.check_type_compatibility(ty, &blanket_impl.target)? {
                return Ok(true);
            }
        }

        Ok(false)
    }

    /// Determines whether the given trait [`TypeRef`] has been blanket
    /// implemented anywhere.
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn is_trait_blanket_impl(&self, trait_type: &TypeRef) -> Result<bool> {
        for trait_impl in self.tdb().uses_of(trait_type) {
            if self.is_type_parameter(&trait_impl.target)? {
                return Ok(true);
            }
        }

        Ok(false)
    }

    /// Returns an iterator of all trait implementations of the given trait
    /// [`TypeRef`], which are blanket implementations.
    pub fn trait_blanket_impls(&self, trait_type: &TypeRef) -> impl Iterator<Item = &Use> {
        self.tdb()
            .uses_of(trait_type)
            .filter(|trait_impl| self.is_type_parameter(&trait_impl.target).unwrap_or(false))
    }
}
