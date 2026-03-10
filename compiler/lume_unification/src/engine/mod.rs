pub(crate) mod constraints;
pub(crate) mod normalize;

#[cfg(test)]
mod tests;

use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::sync::RwLock;

use indexmap::IndexMap;
use lume_errors::Result;
use lume_span::Location;
use normalize::normalize_equality_constraints;

pub trait Context: Sized {
    /// Unique ID which can represent any node, type or otherwise.
    type ID: Debug + Default + Hash + Clone + Copy + PartialEq + Eq;

    /// Reference to some type, along with optional type bindings.
    type Ty: Type<Self>;

    /// Gets the full name of the item with the given ID, as a string.
    fn name_of(&self, id: Self::ID) -> Result<String>;

    /// Gets the full name of the given type, as a string.
    fn name_of_type(&self, ty: &Self::Ty) -> Result<String>;

    /// Gets the location of the given ID.
    fn span_of(&self, id: Self::ID) -> Location;

    /// Gets the canonical type parameter ID of the given type parameter ID.
    fn canonical_type_parameter(&self, owner: Self::ID, type_parameter_id: Self::ID) -> Result<Self::ID>;

    /// Create a fresh type variable for the given owner, bound to the type
    /// variable `binding`.
    fn fresh_var(&mut self, owner: Self::ID, binding: Self::ID, location: Location) -> TypeVar<Self>;

    /// Gets the instanced type parameter ID of the given type variable.
    fn binding_of(&self, type_var: TypeVar<Self>) -> Option<Self::ID>;

    /// Gets the canonical type parameter ID of the given type variable.
    fn canonical_of(&self, type_var: TypeVar<Self>) -> Option<Self::ID>;

    /// Determines whether the given type is a reference to a type variable.
    fn is_type_variable(&self, ty: &Self::Ty) -> bool;

    /// If the type is a reference to a type variable, gets it as a type
    /// variable. Otherwise, [`None`].
    fn as_type_variable(&self, ty: &Self::Ty) -> Option<TypeVar<Self>>;

    /// Determines whether the given type implements the given subtype.
    fn implements_subtype(&self, ty: &Self::Ty, subtype: &Self::Ty) -> bool;
}

pub trait Type<C: Context>: Hash + Debug + Clone + PartialEq + Eq {
    /// Gets the ID of the given type.
    fn id(&self) -> C::ID;

    /// Gets a slice of all the immediate bound types within the type.
    fn bound_types(&self) -> &[Self];

    /// Walks all the type references within the given instance.
    ///
    /// The first yielded type reference is the instance itself, followed by all
    /// bound types, in a breadth-first fashion.
    fn walk(&self) -> impl Iterator<Item = &Self> {
        TypeWalker::new(self)
    }

    /// Checks whether the given type ID is contained within the current type
    /// reference. If it is, returns a reference to the containing type
    /// reference.
    fn contains_some(&self, needle: C::ID) -> Option<&Self> {
        self.walk().find(|ty| ty.id() == needle)
    }

    /// Checks whether the given type ID is contained within the current type
    /// reference.
    fn contains(&self, needle: C::ID) -> bool {
        self.contains_some(needle).is_some()
    }
}

/// Unique ID of an existential type variable.
#[derive(derive_more::Debug)]
pub struct TypeVar<C: Context>(pub(crate) C::ID);

impl<C: Context> Clone for TypeVar<C> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<C: Context> Copy for TypeVar<C> {}

impl<C: Context> Hash for TypeVar<C> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<C: Context> PartialEq for TypeVar<C> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<C: Context> Eq for TypeVar<C> {}

impl<C: Context> Display for TypeVar<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "T?{:X}", lume_span::hash_id(&self.0))
    }
}

#[derive(Clone)]
pub(crate) struct TypeVarEnv<C: Context> {
    /// Set of all constraints for the current type variable
    pub constraints: Vec<Constraint<C>>,

    /// Current available substitute for the type variable.
    pub substitute: Option<C::Ty>,
}

impl<C: Context> Default for TypeVarEnv<C> {
    fn default() -> Self {
        Self {
            constraints: Vec::new(),
            substitute: None,
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub(crate) enum Constraint<C: Context> {
    /// Declares that `lhs` must be equal to `rhs`.
    Equal { lhs: C::Ty, rhs: C::Ty },

    /// Declares that `ty` must implement `bound`.
    Subtype { of: C::Ty, type_parameter_id: C::ID },
}

#[derive(derive_more::Debug, Clone, PartialEq, Eq)]
pub enum Error<C: Context> {
    /// The constraints for a type variable required two incompatible types to
    /// be equal.
    Mismatch { lhs: C::Ty, rhs: C::Ty },

    /// The resolved type for a type variable contains itself, causing an
    /// infinite type.
    InfiniteType { var: TypeVar<C>, ty: C::Ty },

    /// The resolved type for a type variable could not satisfy a subtype bound.
    BoundUnsatisfied {
        ty: C::Ty,
        bound: C::Ty,
        type_parameter: C::ID,
    },

    /// Could not resolve any concrete type for the given type variable.
    Unsolved(TypeVar<C>),
}

pub(crate) struct Env<C: Context> {
    /// List of affected nodes, along with a type variable which was introduced
    /// in the node.
    affected_nodes: Vec<(C::ID, TypeVar<C>)>,

    /// Mapping of type variables and their corresponding environment.
    pub(crate) type_vars: IndexMap<TypeVar<C>, TypeVarEnv<C>>,
}

impl<C: Context> Env<C> {
    /// Ensure there is an entry for constraints for the given type variable.
    ///
    /// So, even if no constraints could be generated, we would still notice
    /// and empty constraint list and throw an error.
    fn ensure_entry_for(&mut self, type_variable: TypeVar<C>) {
        self.type_vars.entry(type_variable).or_default();
    }

    /// Creates a new equality containt, stating that `lhs` must be equal to
    /// `rhs`.
    ///
    /// As a good convention in relation to error messaging, the left-hand side
    /// should be the expected type, and the right-hand side be the found type.
    pub(crate) fn eq(&mut self, type_variable: TypeVar<C>, lhs: C::Ty, rhs: C::Ty) {
        self.type_vars
            .entry(type_variable)
            .or_default()
            .constraints
            .push(Constraint::Equal { lhs, rhs });
    }

    /// Creates a new subtyping containt, stating that the given type variable,
    /// `type_variable`, must subtype `of`.
    pub(crate) fn sub(&mut self, type_variable: TypeVar<C>, of: C::Ty, type_parameter_id: C::ID) {
        self.type_vars
            .entry(type_variable)
            .or_default()
            .constraints
            .push(Constraint::Subtype { of, type_parameter_id });
    }

    /// Declares the substitution type for the given type variable.
    ///
    /// # Panics
    ///
    /// If a type has already been substituted for this type variable, this
    /// method panics.
    pub(crate) fn subst(&mut self, type_variable: TypeVar<C>, with: C::Ty) {
        let existing = self
            .type_vars
            .entry(type_variable)
            .or_default()
            .substitute
            .replace(with);

        assert!(
            existing.is_none(),
            "bug!: replaced existing substitution of {type_variable}"
        );
    }
}

impl<C: Context> Default for Env<C> {
    fn default() -> Self {
        Self {
            affected_nodes: Vec::new(),
            type_vars: IndexMap::new(),
        }
    }
}

pub struct Engine<'ctx, C: Context> {
    pub(crate) ctx: &'ctx mut C,
    pub(crate) env: RwLock<Env<C>>,
}

impl<'ctx, C: Context> Engine<'ctx, C> {
    /// Creates a new unification engine from the given context.
    pub fn new(ctx: &'ctx mut C) -> Self {
        Self {
            ctx,
            env: RwLock::new(Env::default()),
        }
    }

    /// Iterates all the affected nodes from the pass.
    pub(crate) fn affected_nodes(&self) -> Vec<(C::ID, TypeVar<C>)> {
        self.env.try_read().unwrap().affected_nodes.clone()
    }

    /// Registers the given node as being affected by unification, caused by the
    /// given type variable.
    pub(crate) fn add_affected_node(&self, id: C::ID, type_variable: TypeVar<C>) {
        self.env.try_write().unwrap().affected_nodes.push((id, type_variable));
    }
}

impl<C: Context> Engine<'_, C> {
    /// Ensure there is an entry for constraints for the given type variable.
    ///
    /// So, even if no constraints could be generated, we would still notice
    /// and empty constraint list and throw an error.
    pub(crate) fn ensure_entry_for(&self, type_variable: TypeVar<C>) {
        self.env
            .try_write()
            .unwrap()
            .type_vars
            .entry(type_variable)
            .or_default();
    }

    /// Create a fresh type variable for the given owner, bound to the type
    /// variable `binding`.
    #[tracing::instrument(
        level = "TRACE",
        skip_all,
        fields(
            owner = %self.ctx.name_of(owner).unwrap(),
            binding = %self.ctx.name_of(binding).unwrap(),
        ),
        ret(Display)
    )]
    pub(crate) fn fresh_var(&mut self, owner: C::ID, binding: C::ID, location: Location) -> TypeVar<C> {
        let tyvar = self.ctx.fresh_var(owner, binding, location);

        self.env.try_write().unwrap().ensure_entry_for(tyvar);

        tyvar
    }

    /// Creates a new equality containt, stating that `lhs` must be equal to
    /// `rhs`.
    ///
    /// As a good convention in relation to error messaging, the left-hand side
    /// should be the expected type, and the right-hand side be the found type.
    #[tracing::instrument(
        level = "TRACE",
        skip_all,
        fields(
            type_var = %type_variable,
            lhs = %self.ctx.name_of_type(&lhs).unwrap(),
            rhs = %self.ctx.name_of_type(&rhs).unwrap(),
        )
    )]
    pub(crate) fn eq(&self, type_variable: TypeVar<C>, lhs: C::Ty, rhs: C::Ty) {
        self.env.try_write().unwrap().eq(type_variable, lhs, rhs);
    }

    /// Creates a new subtyping containt, stating that the given type variable,
    /// `type_variable`, must subtype `of`.
    #[tracing::instrument(
        level = "TRACE",
        skip_all,
        fields(
            type_var = %type_variable,
            operand = %self.ctx.name_of_type(&of).unwrap(),
            sub = %self.ctx.name_of(type_parameter_id).unwrap(),
        )
    )]
    pub(crate) fn sub(&self, type_variable: TypeVar<C>, of: C::Ty, type_parameter_id: C::ID) {
        self.env.try_write().unwrap().sub(type_variable, of, type_parameter_id);
    }

    /// Declares the substitution type for the given type variable.
    ///
    /// # Panics
    ///
    /// If a type has already been substituted for this type variable, this
    /// method panics.
    #[tracing::instrument(
        level = "TRACE",
        skip_all,
        fields(
            type_var = %type_variable,
            substitute = %self.ctx.name_of_type(&with).unwrap(),
            location = %self.ctx.span_of(type_variable.0),
        )
    )]
    pub(crate) fn subst(&self, type_variable: TypeVar<C>, with: C::Ty) {
        self.env.try_write().unwrap().subst(type_variable, with);
    }
}

pub struct TypeWalker<'ty, C, T> {
    stack: smallvec::SmallVec<[&'ty T; 8]>,
    _phantom: std::marker::PhantomData<C>,
}

impl<'ty, C: Context, T: Type<C>> TypeWalker<'ty, C, T> {
    pub fn new(root: &'ty T) -> Self {
        Self {
            stack: smallvec::smallvec![root],
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<'ty, C: Context, T: Type<C>> Iterator for TypeWalker<'ty, C, T> {
    type Item = &'ty T;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.stack.pop()?;
        self.stack.extend(next.bound_types());

        Some(next)
    }
}
