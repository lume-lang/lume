use std::collections::HashMap;

use indexmap::IndexSet;
use lume_span::NodeId;

use crate::TypeRef;

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct TraitImplKey(usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitImplementationMetadata {
    pub id: NodeId,
    pub methods: Vec<NodeId>,
}

/// Container for implemented traits.
///
/// Allows lookups of all traits implemented on a specific type, as well as all
/// types being implemented by a specific trait.
#[derive(Default, Debug)]
pub struct TraitLookup {
    impls_of: HashMap<TypeRef, IndexSet<TypeRef>>,
    impls_on: HashMap<TypeRef, IndexSet<TypeRef>>,

    metadata: HashMap<TraitImplKey, TraitImplementationMetadata>,
}

impl TraitLookup {
    /// Adds a new trait implementation to the container.
    pub fn add_impl(&mut self, id: NodeId, trait_type: &TypeRef, impl_type: &TypeRef) {
        self.impls_of
            .entry(trait_type.clone())
            .or_default()
            .insert(impl_type.clone());

        self.impls_on
            .entry(impl_type.clone())
            .or_default()
            .insert(trait_type.clone());

        let key = self.key_of(trait_type, impl_type);

        self.metadata.insert(key, TraitImplementationMetadata {
            id,
            methods: Vec::new(),
        });
    }

    /// Adds a new trait method implementation to the given trait
    /// implementation.
    pub fn add_impl_method(&mut self, trait_type: &TypeRef, impl_type: &TypeRef, method_id: NodeId) {
        debug_assert!(self.impls_of.get(trait_type).map_or(false, |im| im.contains(impl_type)));
        debug_assert!(self.impls_on.get(impl_type).map_or(false, |im| im.contains(trait_type)));

        let key = self.key_of(trait_type, impl_type);

        self.metadata
            .get_mut(&key)
            .expect("no trait implementation found")
            .methods
            .push(method_id);
    }

    /// Gets all the trait implementations of the given trait type.
    pub fn implementations_of(&self, trait_type: &TypeRef) -> impl Iterator<Item = &TypeRef> {
        static EMPTY: &'static indexmap::set::Slice<TypeRef> = indexmap::set::Slice::new();

        self.impls_of.get(trait_type).map_or(EMPTY, |i| i.as_slice()).iter()
    }

    /// Gets all the trait implementations on the given implemented type.
    pub fn implementations_on(&self, impl_type: &TypeRef) -> impl Iterator<Item = &TypeRef> {
        static EMPTY: &'static indexmap::set::Slice<TypeRef> = indexmap::set::Slice::new();

        self.impls_on.get(impl_type).map_or(EMPTY, |i| i.as_slice()).iter()
    }

    /// Determines whether there is any trait implemention of `trait_type` on
    /// the type `impl_type`.
    pub fn is_implemented(&self, trait_type: &TypeRef, impl_type: &TypeRef) -> bool {
        self.impls_of
            .get(trait_type)
            .is_some_and(|impls| impls.contains(impl_type))
    }

    /// Gets the ID of the trait implementation block.
    pub fn trait_impl_id(&self, trait_type: &TypeRef, impl_type: &TypeRef) -> Option<NodeId> {
        let key = self.key_of(trait_type, impl_type);

        self.metadata.get(&key).map(|meta| meta.id)
    }

    /// Gets all the implemented methods in the trait implementation of
    /// `trait_type` on `impl_type` type.
    pub fn implemented_methods_in(&self, trait_type: &TypeRef, impl_type: &TypeRef) -> &[NodeId] {
        let key = self.key_of(trait_type, impl_type);

        self.metadata.get(&key).map_or(&[], |meta| meta.methods.as_slice())
    }

    fn key_of(&self, trait_type: &TypeRef, impl_type: &TypeRef) -> TraitImplKey {
        let hash = lume_span::hash_id(&(trait_type, impl_type));

        TraitImplKey(hash)
    }
}
