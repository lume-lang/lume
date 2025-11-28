use lume_errors::Result;
use lume_hir::Node;
use lume_span::NodeId;
use lume_types::TypeRef;

use crate::TyInferCtx;

impl TyInferCtx {
    /// Returns the [`lume_hir::TraitDefinition`] definition, which matches the
    /// [`lume_hir::TraitImplementation`] with the given ID.
    #[libftrace::traced(level = Trace, err)]
    pub fn trait_def_of(&self, id: NodeId) -> Result<&lume_hir::TraitDefinition> {
        let Node::TraitImpl(trait_impl) = self.hir_expect_node(id) else {
            return Err(crate::query::diagnostics::NodeNotFound { id }.into());
        };

        let trait_ty = self.mk_type_ref_from(&trait_impl.name, trait_impl.id)?;

        Ok(self.hir_expect_trait(trait_ty.instance_of))
    }

    /// Determines whether the given [`TypeRef`], `ty`, implements the given
    /// trait, `trait_id`.
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn trait_impl_by(&self, trait_id: &TypeRef, ty: &TypeRef) -> Result<bool> {
        #[cfg(debug_assertions)]
        self.hir_expect_trait(trait_id.instance_of);

        if let Some(type_param) = self.as_type_parameter(ty)? {
            for constraint in &type_param.constraints {
                let constraint_type = self.mk_type_ref_from(constraint, trait_id.instance_of)?;

                if self.check_type_compatibility(&constraint_type, trait_id)? {
                    return Ok(true);
                }
            }
        }

        // Check for direct trait implementations on the target type.
        for trait_impl in self.tdb().traits.implementations_on(ty) {
            if trait_impl.instance_of != trait_id.instance_of {
                continue;
            }

            // Make sure the given "trait" is actually a trait.
            self.hir_expect_trait(trait_impl.instance_of);

            return Ok(true);
        }

        // If no direct implementations exist, attempt to find any blanket
        // implementations which might encapsulate the target type.
        for blanket_impl_target in self.trait_blanket_impls(trait_id) {
            if self.check_type_compatibility(ty, blanket_impl_target)? {
                return Ok(true);
            }
        }

        Ok(false)
    }

    /// Determines whether the given trait [`TypeRef`] has been blanket
    /// implemented anywhere.
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn is_trait_blanket_impl(&self, trait_type: &TypeRef) -> Result<bool> {
        for target_type in self.tdb().traits.implementations_of(trait_type) {
            if self.is_type_parameter(target_type)? {
                return Ok(true);
            }
        }

        Ok(false)
    }

    /// Returns an iterator of all trait implementations of the given trait
    /// [`TypeRef`], which are blanket implementations.
    pub fn trait_blanket_impls(&self, trait_type: &TypeRef) -> impl Iterator<Item = &TypeRef> {
        self.tdb()
            .traits
            .implementations_of(trait_type)
            .filter(|target_type| self.is_type_parameter(target_type).unwrap_or(false))
    }

    /// Gets the trait implementation of `trait_type` on the type `source`.
    #[libftrace::traced(level = Trace)]
    pub fn get_trait_impl_of(&self, trait_type: &TypeRef, source: &TypeRef) -> Option<&lume_hir::TraitImplementation> {
        if let Some(trait_impl_id) = self.tdb().traits.trait_impl_id(trait_type, source) {
            let lume_hir::Node::TraitImpl(trait_impl) = self.hir_node(trait_impl_id)? else {
                panic!("bug!: expected trait impl with ID {trait_impl_id}");
            };

            return Some(trait_impl);
        }

        None
    }

    /// Gets the trait implementation of `Cast<T>` on the type `source`, where
    /// `T` is the type `dest`.
    #[libftrace::traced(level = Trace)]
    pub fn cast_impl_of(&self, source: &TypeRef, dest: &TypeRef) -> Option<&lume_hir::TraitImplementation> {
        let mut cast_trait = self.lang_item_type("cast_trait")?;
        cast_trait.bound_types.push(dest.clone());

        self.get_trait_impl_of(&cast_trait, source)
    }

    /// Gets the trait definition which is being implemented by the given
    /// [`lume_hir::TraitImplementation`].
    #[libftrace::traced(level = Trace)]
    pub fn trait_definition_of_impl(
        &self,
        trait_impl: &lume_hir::TraitImplementation,
    ) -> Result<&lume_hir::TraitDefinition> {
        let trait_name = &trait_impl.name.name;

        let Some(type_def) = self.tdb().find_type(trait_name) else {
            panic!("bug!: trait definition with name `{trait_name:+}` does not exist");
        };

        Ok(self.hir_expect_trait(type_def.id))
    }

    /// Gets the parent trait definition which defines the method being
    /// implemented by `method_impl`.
    #[libftrace::traced(level = Trace)]
    pub fn trait_definition_of_method_impl(
        &self,
        method_impl: &lume_hir::TraitMethodImplementation,
    ) -> Result<&lume_hir::TraitDefinition> {
        for parent in self.hir_parent_iter(method_impl.id) {
            if let lume_hir::Node::TraitImpl(trait_impl) = parent {
                return self.trait_definition_of_impl(trait_impl);
            }
        }

        panic!("bug!: trait method implementation defined outside trait implementation");
    }

    /// Gets the matching trait method definition which defines the method being
    /// implemented by `method_impl`.
    #[libftrace::traced(level = Trace)]
    pub fn trait_method_definition_of_method_impl(
        &self,
        trait_method_impl: &lume_hir::TraitMethodImplementation,
    ) -> Result<&lume_hir::TraitMethodDefinition> {
        let trait_def = self.trait_definition_of_method_impl(trait_method_impl)?;

        let Some(trait_method_def) = trait_def
            .methods
            .iter()
            .find(|method| method.name == trait_method_impl.name)
        else {
            panic!("bug!: trait method implementation exists without trait method definition");
        };

        Ok(trait_method_def)
    }

    /// Gets the target type of the given method, given the type within the
    /// parent `impl` block.
    #[libftrace::traced(level = Trace)]
    pub fn impl_type_of_method(&self, method_id: NodeId) -> Result<TypeRef> {
        for parent in self.hir_parent_iter(method_id) {
            if let lume_hir::Node::Impl(impl_block) = parent {
                return self.mk_type_ref_from(&impl_block.target, method_id);
            }
        }

        Err(super::diagnostics::NoParentImpl {
            source: self.hir_span_of_node(method_id),
        }
        .into())
    }
}
