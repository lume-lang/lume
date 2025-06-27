/// Uniquely identifies some definition or value, independently of
/// the parent package or item.
#[derive(Hash, Default, Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub struct Idx(usize);

impl Idx {
    /// Creates a new [`Idx`] without a unique ID.
    ///
    /// If an [`Idx`] with a valid ID is required, see [`Idx::next()`].
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new [`Idx`] with a unique ID.
    #[inline]
    #[must_use]
    pub fn next(self) -> Self {
        Idx(self.0 + 1)
    }

    /// Gets the [`Idx`] as a value of `usize`.
    #[inline]
    pub const fn as_usize(self) -> usize {
        self.0
    }

    /// Creates a new [`Idx`] from the given `usize` value.
    #[inline]
    pub const fn from_usize(val: usize) -> Self {
        Self(val)
    }
}

impl From<usize> for Idx {
    fn from(value: usize) -> Self {
        Idx(value)
    }
}

impl<T: std::hash::Hash + ?Sized> From<&T> for Idx {
    fn from(value: &T) -> Self {
        Idx(crate::hash_id(value))
    }
}

/// Uniquely identifies a package.
///
/// Packages are identified by a unique ID, which is used to locate the package's source files.
/// The ID is generated from the name of the package using a hash function.
#[derive(Hash, Debug, Default, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub struct PackageId(usize);

impl PackageId {
    /// Creates a new [`PackageId`] from the given name.
    ///
    /// # Examples
    ///
    /// ```
    /// use lume_span::PackageId;
    ///
    /// let _ = PackageId::new("lume");
    /// let _ = PackageId::new(&String::from("lume"));
    /// ```
    #[inline]
    pub fn new(name: &'_ str) -> Self {
        Self::from(name)
    }

    /// Creates an empty [`PackageId`] without a valid ID.
    #[inline]
    #[must_use]
    pub const fn empty() -> Self {
        Self(0)
    }

    /// Gets the [`PackageId`] as a value of `usize`.
    #[inline]
    pub const fn as_usize(self) -> usize {
        self.0
    }

    /// Creates a new [`PackageId`] from the given `usize` value.
    #[inline]
    pub const fn from_usize(val: usize) -> Self {
        Self(val)
    }

    /// Creates a new [`PackageId`] with the hash of the given value.
    #[inline]
    pub fn from_name<T: std::hash::Hash + ?Sized>(value: &T) -> Self {
        Self(crate::hash_id(value))
    }
}

impl<T: std::hash::Hash + ?Sized> From<&T> for PackageId {
    fn from(value: &T) -> Self {
        PackageId(crate::hash_id(value))
    }
}

/// Uniquely identifies a top-level item within the package [`ItemId::package`].
#[derive(Hash, Default, Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub struct ItemId(usize);

impl ItemId {
    /// Creates a new [`ItemId`] from the given name.
    ///
    /// # Examples
    ///
    /// ```
    /// use lume_span::ItemId;
    ///
    /// let _ = ItemId::new("lume");
    /// let _ = ItemId::new(&String::from("lume"));
    /// ```
    #[inline]
    pub fn new(name: &'_ str) -> Self {
        Self::from_name(name)
    }

    /// Gets the [`ItemId`] as a value of `usize`.
    #[inline]
    pub const fn as_usize(self) -> usize {
        self.0
    }

    /// Creates a new [`PackageId`] from the given `usize` value.
    #[inline]
    pub const fn from_usize(val: usize) -> Self {
        Self(val)
    }

    /// Creates a new [`ItemId`] with the hash of the given value.
    #[inline]
    pub fn from_name<T: std::hash::Hash + ?Sized>(value: &T) -> Self {
        Self(crate::hash_id(value))
    }

    /// Creates an empty [`ItemId`] without a valid ID.
    #[inline]
    #[must_use]
    pub const fn empty() -> Self {
        Self::from_usize(0)
    }
}

/// Uniquely identifies any local expression, such as variables, arguments, calls or otherwise.
///
/// [`LocalId`] instances are unique within the parent item, referenced by it's [`ItemId`] in [`LocalId::def`].
#[derive(Hash, Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct LocalId {
    pub def: ItemId,
    pub index: Idx,
}

impl LocalId {
    /// Creates a new [`LocalId`] without a unique ID.
    ///
    /// If an [`LocalId`] with a valid ID is required, see [`LocalId::next()`].
    #[inline]
    pub fn empty(def: ItemId) -> Self {
        Self { def, index: Idx::new() }
    }

    /// Creates a new [`LocalId`] with a unique ID.
    #[inline]
    #[must_use]
    pub fn next(def: ItemId, prev: Self) -> Self {
        Self {
            def,
            index: Idx::next(prev.index),
        }
    }
}

/// Uniquely identifies any local statement.
///
/// [`StatementId`] instances are unique within the parent item, referenced
/// by it's [`ItemId`] in [`StatementId::def`].
#[derive(Hash, Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct StatementId {
    pub def: ItemId,
    pub index: Idx,
}

impl StatementId {
    /// Creates a new [`StatementId`] without a unique ID.
    ///
    /// If an [`StatementId`] with a valid ID is required, see [`StatementId::next()`].
    #[inline]
    pub fn empty(def: ItemId) -> Self {
        Self { def, index: Idx::new() }
    }

    /// Creates a new [`StatementId`] with a unique ID.
    #[inline]
    #[must_use]
    pub fn next(def: ItemId, prev: Self) -> Self {
        Self {
            def,
            index: Idx::next(prev.index),
        }
    }

    /// Creates a new [`StatementId`] with the given parameters.
    #[inline]
    pub fn from_id(def: ItemId, index: impl Into<Idx>) -> Self {
        Self {
            def,
            index: index.into(),
        }
    }
}

/// Uniquely identifies any local expression, such as variables, literals, calls or otherwise.
///
/// [`ExpressionId`] instances are unique within the parent item, referenced
/// by it's [`ItemId`] in [`ExpressionId::def`].
#[derive(Hash, Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExpressionId {
    pub def: ItemId,
    pub index: Idx,
}

impl ExpressionId {
    /// Creates a new [`ExpressionId`] without a unique ID.
    ///
    /// If an [`ExpressionId`] with a valid ID is required, see [`ExpressionId::next()`].
    #[inline]
    pub fn empty(def: ItemId) -> Self {
        Self { def, index: Idx::new() }
    }

    /// Creates a new [`ExpressionId`] with a unique ID.
    #[inline]
    #[must_use]
    pub fn next(def: ItemId, prev: Self) -> Self {
        Self {
            def,
            index: Idx::next(prev.index),
        }
    }

    /// Creates a new [`ExpressionId`] with the given parameters.
    #[inline]
    pub fn from_id(def: ItemId, index: impl Into<Idx>) -> Self {
        Self {
            def,
            index: index.into(),
        }
    }
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DefId {
    Item(ItemId),
    Statement(StatementId),
    Expression(ExpressionId),
}
