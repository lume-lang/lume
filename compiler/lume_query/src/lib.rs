pub use once_cell::sync::Lazy;
use std::{
    collections::HashMap,
    sync::{RwLock, RwLockReadGuard, RwLockWriteGuard},
};

#[cfg(feature = "proc_macros")]
pub use lume_query_macros::cached_query;

pub struct CacheStore<T: std::clone::Clone> {
    inner: Lazy<RwLock<HashMap<u64, T>>>,
}

impl<T: std::clone::Clone> CacheStore<T> {
    /// Creates a new, empty [`CacheStore`].
    pub const fn new() -> Self {
        Self {
            inner: Lazy::new(|| RwLock::new(HashMap::new())),
        }
    }

    /// Retrieves a shared read access to the [`CacheStore`]'s inner map instance.
    ///
    /// # Panics
    ///
    /// This method panics if another thread write-locked the store before
    /// this method was invoked, without releasing the lock.
    pub fn read(&self) -> RwLockReadGuard<HashMap<u64, T>> {
        self.inner.read().unwrap()
    }

    /// Retrieves an exclusive-write access to the [`CacheStore`]'s inner map instance.
    ///
    /// # Panics
    ///
    /// This method panics if another thread write-locked the store before
    /// this method was invoked, without releasing the lock.
    pub fn write(&self) -> RwLockWriteGuard<HashMap<u64, T>> {
        self.inner.write().unwrap()
    }

    /// Attempts to retrieve the value with the given key from the
    /// [`CacheStore`], if it exists. If the value is found within the store,
    /// it is cloned and returned as `Some(...)`. Otherwise, returns `None`.
    pub fn get(&self, key: u64) -> Option<T> {
        self.read().get(&key).cloned()
    }

    /// Inserts the given value into the [`CacheStore`] instance with
    /// the given key. If a value already exists with the given key, it is
    /// overwritten.
    pub fn insert(&self, key: u64, value: T) {
        self.write().insert(key, value);
    }

    /// Looks up the given cache key within the [`CacheStore`] instance. If
    /// a value is found within the store, it is cloned and returned. If the key
    /// is not found within the store, `f` is invoked and the result is cloned and
    /// inserted into the store. After the value is stored, the original, un-cloned value
    /// is returned.
    pub fn get_or_insert(&self, key: u64, f: impl FnOnce() -> T) -> T {
        if let Some(existing) = self.get(key) {
            return existing;
        }

        let value = f();
        self.insert(key, value.clone());

        value
    }
}

impl<T: std::clone::Clone> Default for CacheStore<T> {
    fn default() -> Self {
        Self {
            inner: Lazy::new(|| RwLock::new(HashMap::new())),
        }
    }
}
