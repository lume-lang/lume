use std::hash::Hash;

/// Hashes values using the `FxHasher` algorithm, which was extracted
/// from the Rust compiler.
///
/// This hashing function is usually used for hashing of smaller objects;
/// usually strings, IDs, etc.
///
/// The reason for using this instead of the [`std::hash::DefaultHasher`] is
/// that we require some deterministic hashing algorithm for consistent results,
/// so we can use it for incremental builds and caching. The default hasher does
/// not have any specific algorithm defined, so it cannot be relied upon to
/// create the same hash given the same input.
#[inline]
pub fn portable_hash<T: Hash + ?Sized>(value: &T) -> usize {
    fxhash::hash(value)
}

pub type SecureHash = blake3::Hash;

pub type SecureHasher = blake3::Hasher;

/// BLAKE3 hash function, which can be highly performant on larger blocks of
/// memory.
///
/// This hashing function is usually used for hashing of larger memory blocks,
/// such as source file content, package metadata, etc.
#[inline]
pub fn secure_hash<T: AsRef<[u8]>>(value: &T) -> SecureHash {
    blake3::hash(value.as_ref())
}
