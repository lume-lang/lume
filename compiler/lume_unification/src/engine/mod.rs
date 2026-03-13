pub(crate) mod constraints;
pub(crate) mod union;

#[cfg(test)]
mod tests;

use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::sync::RwLock;

use indexmap::{IndexMap, IndexSet};
use lume_errors::Result;
use lume_span::Location;

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

    /// Determines the kind of the given type.
    fn kind_of_type(&self, ty: &Self::Ty) -> TypeKind<Self>;

    /// Create a fresh type variable for the given owner, bound to the type
    /// variable `binding`.
    fn fresh_var(&mut self, owner: Self::ID, binding: Self::ID, location: Location) -> TypeVar<Self>;

    /// Turn the given type variable into a type.
    fn as_type(&self, type_var: TypeVar<Self>) -> Self::Ty;

    /// If the type is a reference to a type variable, gets it as a type
    /// variable. Otherwise, [`None`].
    fn as_type_variable(&self, ty: &Self::Ty) -> Option<TypeVar<Self>>;

    /// Determines whether the given type implements the given subtype.
    fn implements_subtype(&self, ty: &Self::Ty, subtype: &Self::Ty) -> bool;

    /// Determines if the given type is a type variable.
    fn is_type_variable(&self, ty: &Self::Ty) -> bool {
        matches!(self.kind_of_type(ty), TypeKind::Variable(_))
    }
}

pub trait Type<C: Context>: Hash + Debug + Clone + PartialEq + Eq {
    /// Gets the ID of the given type.
    fn id(&self) -> C::ID;

    /// Gets a slice of all the immediate bound types within the type.
    fn bound_types(&self) -> &[Self];

    /// Gets a slice of all the immediate bound types within the type.
    fn bound_types_mut(&mut self) -> &mut [Self];

    /// Walks all the type references within the given instance.
    ///
    /// The first yielded type reference is the instance itself, followed by all
    /// bound types, in a breadth-first fashion.
    fn walk(&self) -> impl Iterator<Item = &Self> {
        TypeWalker::new(self)
    }

    /// Walks all the type references within the given instance.
    ///
    /// The first yielded type reference is the instance itself, followed by all
    /// bound types, in a breadth-first fashion.
    fn walk_mut<'ty>(&'ty mut self) -> impl Iterator<Item = &'ty mut Self>
    where
        C: 'ty,
    {
        TypeWalkerMut::new(self)
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

/// Denotes the kind of type being represented.
#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeKind<C: Context> {
    /// Concrete type - fixed at compile time
    Concrete(C::ID),

    /// Inference variable - flexible and solvable
    Variable(TypeVar<C>),

    /// Type parameter - rigid and fixed within it's scope
    Parameter(C::ID),
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
        write!(f, "T?{:X}", lume_hash::portable_hash(&self.0))
    }
}

impl<C: Context> union::UnionKey for TypeVar<C> {}

#[derive(derive_more::Debug)]
pub(crate) struct TypeVarEnv<C: Context> {
    /// Set of all constraints for the current type variable
    pub constraints: IndexSet<Constraint<C>>,

    /// Current available substitute for the type variable.
    pub substitute: Option<C::Ty>,
}

impl<C: Context> Default for TypeVarEnv<C> {
    fn default() -> Self {
        Self {
            constraints: IndexSet::new(),
            substitute: None,
        }
    }
}

#[derive(derive_more::Debug, derive_more::PartialEq, derive_more::Eq)]
pub(crate) enum Constraint<C: Context> {
    /// Declares that the type variable must be equal to `ty`.
    Equal { ty: C::Ty },

    /// Declares that the type variable must implement `bound`.
    Subtype { of: C::Ty, type_parameter: C::ID },
}

impl<C: Context> Clone for Constraint<C> {
    fn clone(&self) -> Self {
        match self {
            Self::Equal { ty } => Self::Equal { ty: ty.clone() },
            Self::Subtype { of, type_parameter } => Self::Subtype {
                of: of.clone(),
                type_parameter: *type_parameter,
            },
        }
    }
}

impl<C: Context> Hash for Constraint<C> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Equal { ty } => ty.hash(state),
            Self::Subtype { of, type_parameter } => {
                of.hash(state);
                type_parameter.hash(state);
            }
        }
    }
}

#[derive(derive_more::Debug, Clone, PartialEq, Eq)]
pub enum Error<C: Context> {
    /// The constraints for a type variable required two incompatible types to
    /// be equal.
    #[debug("Mismatch({lhs:?}, {rhs:?})")]
    Mismatch { lhs: C::Ty, rhs: C::Ty },

    /// The resolved type for a type variable contains itself, causing an
    /// infinite type.
    #[debug("InfiniteType({var}, {ty:?})")]
    InfiniteType { var: TypeVar<C>, ty: C::Ty },

    /// The resolved type for a type variable could not satisfy a subtype bound.
    #[debug("BoundUnsatisfied({ty:?}, {bound:?})")]
    BoundUnsatisfied {
        ty: C::Ty,
        bound: C::Ty,
        type_parameter: C::ID,
    },

    /// Attempted to equate two rigid types together
    #[debug("RigidMismatch({lhs:?}, {rhs:?})")]
    RigidMismatch { lhs: C::Ty, rhs: C::Ty },

    /// Could not resolve any concrete type for the given type variable.
    #[debug("Unresolved({_0})")]
    Unsolved(TypeVar<C>),
}

pub struct AggregateError<C: Context> {
    set: IndexMap<TypeVar<C>, Error<C>>,
}

impl<C: Context> AggregateError<C> {
    pub fn push(&mut self, type_var: TypeVar<C>, error: Error<C>) {
        // Prevents multiple errors from the same type variable to be raised.
        self.set.insert(type_var, error);
    }

    pub fn extend(&mut self, other: Self) {
        for (type_var, error) in other.set {
            if !self.set.contains_key(&type_var) {
                self.set.insert(type_var, error);
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        self.set.is_empty()
    }

    pub fn into_values(self) -> impl Iterator<Item = Error<C>> {
        self.set.into_values()
    }
}

impl<C: Context> Debug for AggregateError<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.set.values().enumerate().try_for_each(|(idx, err)| {
            Debug::fmt(err, f)?;

            if idx < self.set.len() - 1 {
                write!(f, ", ")?;
            }

            Ok(())
        })
    }
}

impl<C: Context> Default for AggregateError<C> {
    fn default() -> Self {
        Self { set: IndexMap::new() }
    }
}

pub(crate) struct Env<C: Context> {
    /// List of affected nodes, along with a type variable which was introduced
    /// in the node.
    affected_nodes: Vec<(C::ID, TypeVar<C>)>,

    /// Mapping of type variables and their corresponding environment.
    pub(crate) type_vars: IndexMap<TypeVar<C>, TypeVarEnv<C>>,

    /// Union-find data structure for grouping sets of type variables.
    pub(crate) union: RwLock<union::UnionFind<TypeVar<C>>>,
}

impl<C: Context> Env<C> {
    /// Gets the representative (root key) of the given type variable.
    ///
    /// If the type variable is not in the union-find, it returns itself.
    #[tracing::instrument(level = "TRACE", skip_all, fields(%type_variable), ret(Display))]
    fn representative_of(&self, type_variable: TypeVar<C>) -> TypeVar<C> {
        self.union
            .try_write()
            .unwrap()
            .find(type_variable)
            .unwrap_or(type_variable)
    }

    /// Unionize the two type variables inside the same set.
    #[tracing::instrument(level = "TRACE", skip_all, fields(%a, %b))]
    fn union(&mut self, a: TypeVar<C>, b: TypeVar<C>) {
        if a == b {
            return;
        }

        self.union.try_write().unwrap().union(a, b);

        // Since the two type variables have unionized, merge the environment of the
        // non-representative into the new union representative environment.
        let new_root_key = self.union.try_write().unwrap().find(a).unwrap();
        tracing::debug!(%new_root_key, %a, %b);

        for old_env_key in [a, b] {
            if old_env_key == new_root_key {
                continue;
            }

            let Some(old_env) = self.type_vars.swap_remove(&old_env_key) else {
                continue;
            };

            tracing::debug!(
                from = %old_env_key,
                to = %new_root_key,
                "merge_type_var_env"
            );

            self.type_vars
                .entry(new_root_key)
                .or_default()
                .constraints
                .extend(old_env.constraints);
        }
    }

    /// Ensure there is an entry for constraints for the given type variable.
    ///
    /// So, even if no constraints could be generated, we would still notice
    /// and empty constraint list and throw an error.
    fn ensure_entry_for(&mut self, type_variable: TypeVar<C>) {
        let root = self.representative_of(type_variable);

        self.type_vars.entry(root).or_default();
    }

    /// Gets the environment associated with the given type variable.
    fn env(&self, type_variable: TypeVar<C>) -> Option<&TypeVarEnv<C>> {
        let root = self.representative_of(type_variable);

        self.type_vars.get(&root)
    }

    /// Creates a new equality containt, stating that `type_variable` must be
    /// equal to `ty`.
    pub(crate) fn eq(&mut self, type_variable: TypeVar<C>, ty: C::Ty) {
        let root = self.representative_of(type_variable);

        self.type_vars
            .entry(root)
            .or_default()
            .constraints
            .insert(Constraint::Equal { ty });
    }

    /// Creates a new subtyping containt, stating that the given type variable,
    /// `type_variable`, must subtype `of`.
    pub(crate) fn sub(&mut self, type_variable: TypeVar<C>, of: C::Ty, type_parameter: C::ID) {
        let root = self.representative_of(type_variable);

        self.type_vars
            .entry(root)
            .or_default()
            .constraints
            .insert(Constraint::Subtype { of, type_parameter });
    }

    /// Declares the substitution type for the given type variable.
    pub(crate) fn subst(&mut self, type_variable: TypeVar<C>, with: C::Ty) {
        let root = self.representative_of(type_variable);

        self.type_vars.entry(root).or_default().substitute = Some(with);
    }

    /// Gets an iterator of all the constraints of the given type variable.
    pub(crate) fn constraints_of(&self, id: TypeVar<C>) -> IndexSet<Constraint<C>> {
        let root = self.representative_of(id);

        self.type_vars
            .get(&root)
            .map_or(IndexSet::new(), |type_var| type_var.constraints.clone())
    }

    /// Gets the current substitute of the given type variable.
    pub(crate) fn substitute_of(&self, type_variable: TypeVar<C>) -> Option<C::Ty> {
        let root = self.representative_of(type_variable);

        self.env(root).and_then(|env| env.substitute.clone())
    }
}

impl<C: Context> Default for Env<C> {
    fn default() -> Self {
        Self {
            affected_nodes: Vec::new(),
            type_vars: IndexMap::new(),
            union: RwLock::new(union::UnionFind::default()),
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

    /// Gets the current substitute of the given type variable.
    pub(crate) fn substitute_of(&self, type_variable: TypeVar<C>) -> Option<C::Ty> {
        self.env.try_read().unwrap().substitute_of(type_variable)
    }

    /// Fully resolve a type variable to its concrete type.
    #[tracing::instrument(level = "TRACE", skip_all, fields(%type_variable), err(Debug))]
    pub(crate) fn resolve(&self, type_variable: TypeVar<C>) -> std::result::Result<C::Ty, Error<C>> {
        let walked = self.walk(self.ctx.as_type(type_variable));

        if let Some(type_variable) = self.ctx.as_type_variable(&walked) {
            Err(Error::Unsolved(type_variable))
        } else {
            Ok(walked)
        }
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

pub struct TypeWalkerMut<'ty, C, T> {
    stack: smallvec::SmallVec<[*mut T; 8]>,
    _phantom: std::marker::PhantomData<(&'ty mut T, &'ty C)>,
}

impl<'ty, C: Context, T: Type<C>> TypeWalkerMut<'ty, C, T> {
    pub fn new(root: &'ty mut T) -> Self {
        Self {
            stack: smallvec::smallvec![std::ptr::from_mut(root)],
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<'ty, C: Context, T: Type<C> + 'ty> Iterator for TypeWalkerMut<'ty, C, T> {
    type Item = &'ty mut T;

    fn next(&mut self) -> Option<Self::Item> {
        let ptr = self.stack.pop()?;

        // SAFETY:
        // Each pointer in the stack was derived from a unique `&mut TypeRef`, so
        // the lifetime `'ty` is tied to the original `&'ty mut TypeRef`.
        let node = unsafe { &mut *ptr };

        // Reverse order so the first child is yielded first.
        for child in node.bound_types_mut().iter_mut().rev() {
            self.stack.push(std::ptr::from_mut(child));
        }

        Some(node)
    }
}

#[tracing::instrument(
    level = "TRACE",
    skip_all,
    fields(
        lhs = %ctx.name_of_type(lhs).unwrap(),
        rhs = %ctx.name_of_type(rhs).unwrap(),
    ),
    err(Debug)
)]
pub(crate) fn normalize_constraint_types<C: Context>(
    ctx: &C,
    target: C::ID,
    lhs: &C::Ty,
    rhs: &C::Ty,
) -> std::result::Result<(C::Ty, C::Ty), Error<C>> {
    // If either of the items in the set are the target, we send them back.
    if lhs.id() == target || rhs.id() == target {
        return Ok((lhs.to_owned(), rhs.to_owned()));
    }

    tracing::trace!(target = ?target, ?lhs, ?rhs);

    // If the two types being normalized don't refer to the type parent type, we
    // cannot normalize them. For example, image a set of constraints like this:
    // ```
    // U = [Option<?T> = Option<String>]
    // ```
    // can be normalized, since they both refer to the same containing type,
    // `Option`.
    //
    // Contrarily, this example cannot be normalized since they do not
    // refer to the same containing type:
    // ```
    // U = [Array<?T> = Option<String>]
    // ```
    if lhs.id() == rhs.id() {
        for (bound_lhs, bound_rhs) in lhs.bound_types().iter().zip(rhs.bound_types().iter()) {
            if !bound_lhs.contains(target) && !bound_rhs.contains(target) {
                continue;
            }

            return normalize_constraint_types(ctx, target, bound_lhs, bound_rhs);
        }
    }

    Err(Error::Mismatch {
        lhs: lhs.to_owned(),
        rhs: rhs.to_owned(),
    })
}
