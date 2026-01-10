use std::fmt::Display;

use serde::{Deserialize, Serialize};

/// Uniquely identifies some definition or value, independently of
/// the parent package or item.
#[derive(Serialize, Deserialize, Hash, Default, Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
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
/// Packages are identified by a unique ID, which is used to locate the
/// package's source files. The ID is generated from the name of the package
/// using a hash function.
#[derive(Serialize, Deserialize, Hash, Debug, Default, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
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

    /// Determines if the [`PackageId`] refers to the standard library.
    #[inline]
    pub fn is_std(self) -> bool {
        self.0 == 0
    }
}

impl<T: std::hash::Hash + ?Sized> From<&T> for PackageId {
    fn from(value: &T) -> Self {
        PackageId(crate::hash_id(value))
    }
}

impl Display for PackageId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "P{}", self.as_usize())
    }
}

/// Uniquely identifiers any item, definition or local inside of the package
/// with the ID of [`NodeId::package`].
#[derive(Serialize, Deserialize, Hash, Default, Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub struct NodeId {
    pub package: PackageId,
    pub index: Idx,
}

impl NodeId {
    /// Creates a new [`NodeId`].
    #[inline]
    pub fn empty(package: PackageId) -> Self {
        Self {
            package,
            index: Idx::new(),
        }
    }

    pub fn next(self) -> Self {
        Self {
            package: self.package,
            index: Idx::next(self.index),
        }
    }

    /// Creates a new [`NodeId`] from the given value.
    #[inline]
    pub const fn from_usize(package: PackageId, value: usize) -> Self {
        Self {
            package,
            index: Idx::from_usize(value),
        }
    }

    /// Creates a new [`NodeId`] from the given value.
    #[inline]
    pub fn from_name<T: std::hash::Hash + ?Sized>(package: PackageId, value: &T) -> Self {
        Self {
            package,
            index: Idx::from_usize(crate::hash_id(value)),
        }
    }

    pub fn as_usize(self) -> usize {
        // Used to prevent `hash_id` from creating a value of 0 when the kind is
        // `FunctionKind::Function` and the ID is 0. A function ID of 0 can look
        // wrong or misleading, so we're explicitly removing that possiblity.
        static HASH_OFFSET: usize = 0x4D6B_0189;

        crate::hash_id(&(self, HASH_OFFSET))
    }
}

impl std::fmt::Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "!{:?}", self.as_usize())
    }
}
