/// Uniquely identifies some definition or value, independently of
/// the parent package or item.
#[derive(Hash, Default, Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub struct Idx(usize);

impl Idx {
    /// Creates a new [`Idx`] without a unique ID.
    ///
    /// If an [`Idx`] with a valid ID is required, see [`Idx::next()`].
    #[inline]
    pub const fn new() -> Self {
        Self(0)
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
pub struct ItemId {
    pub package: PackageId,
    pub index: Idx,
}

impl ItemId {
    /// Creates a new [`ItemId`].
    #[inline]
    pub fn new(package: PackageId) -> Self {
        Self {
            package,
            index: Idx::new(),
        }
    }
    /// Creates a new [`ItemId`] from the given value.
    #[inline]
    pub fn from_name<T: std::hash::Hash + ?Sized>(package: PackageId, value: &T) -> Self {
        Self {
            package,
            index: Idx::from_usize(crate::hash_id(value)),
        }
    }

    /// Creates a new [`ItemId`] from the next item in the sequence.
    #[inline]
    #[must_use]
    pub fn next(self) -> Self {
        Self {
            package: self.package,
            index: self.index.next(),
        }
    }

    /// Creates an empty [`ItemId`] without a valid ID.
    #[inline]
    #[must_use]
    pub const fn empty() -> Self {
        Self {
            package: PackageId::empty(),
            index: Idx::new(),
        }
    }
}

/// Uniquely identifies a property within a given parent item.
///
/// [`PropertyId`] instances are unique within the parent item, referenced
/// by it's [`ItemId`] in [`PropertyId::item`].
#[derive(Hash, Default, Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub struct PropertyId {
    pub item: ItemId,
    pub index: Idx,
}

impl PropertyId {
    /// Creates a new [`PropertyId`] without a unique ID.
    ///
    /// If an [`PropertyId`] with a valid ID is required, see [`PropertyId::next()`].
    #[inline]
    pub fn empty(item: ItemId) -> Self {
        Self {
            item,
            index: Idx::new(),
        }
    }

    /// Creates a new [`PropertyId`] with the given ID.
    #[inline]
    pub fn new(item: ItemId, index: impl Into<Idx>) -> Self {
        Self {
            item,
            index: index.into(),
        }
    }
}

/// Uniquely identifies a method within a given parent item.
///
/// [`MethodId`] instances are unique within the parent item, referenced
/// by it's [`ItemId`] in [`MethodId::item`].
#[derive(Hash, Default, Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub struct MethodId {
    pub item: ItemId,
    pub index: Idx,
}

impl MethodId {
    /// Creates a new [`MethodId`] without a unique ID.
    ///
    /// If an [`MethodId`] with a valid ID is required, see [`MethodId::next()`].
    #[inline]
    pub fn empty(item: ItemId) -> Self {
        Self {
            item,
            index: Idx::new(),
        }
    }

    /// Creates a new [`MethodId`] with the given ID.
    #[inline]
    pub fn new(item: ItemId, index: impl Into<Idx>) -> Self {
        Self {
            item,
            index: index.into(),
        }
    }
}

/// Uniquely identifies a pattern within a given parent item.
///
/// [`PatternId`] instances are unique within the parent item, referenced
/// by it's [`ItemId`] in [`PatternId::item`].
#[derive(Hash, Default, Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub struct PatternId {
    pub item: ItemId,
    pub index: Idx,
}

impl PatternId {
    /// Creates a new [`PatternId`] without a unique ID.
    ///
    /// If an [`PatternId`] with a valid ID is required, see [`PatternId::next()`].
    #[inline]
    pub fn empty(item: ItemId) -> Self {
        Self {
            item,
            index: Idx::new(),
        }
    }

    /// Creates a new [`PatternId`] with the given ID.
    #[inline]
    pub fn new(item: ItemId, index: impl Into<Idx>) -> Self {
        Self {
            item,
            index: index.into(),
        }
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

    /// Converts the [`StatementId`] into a [`usize`].
    #[inline]
    pub fn as_usize(&self) -> usize {
        crate::hash_id(&(self.def, self.index))
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
    Property(PropertyId),
    Method(MethodId),
    Pattern(PatternId),
    Statement(StatementId),
    Expression(ExpressionId),
}
