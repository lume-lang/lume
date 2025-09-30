pub use once_cell::sync::Lazy;
use std::{
    any::Any,
    collections::HashMap,
    sync::{RwLock, RwLockReadGuard, RwLockWriteGuard},
};

#[cfg(feature = "proc_macros")]
pub use lume_query_macros::cached_query;
#[cfg(feature = "proc_macros")]
pub use tracing;

/// A trait that provides access to a [`CacheStore`] instance.
pub trait CacheContext {
    /// Retrieves the instance of [`CacheStore`], which is provided by the
    /// [`CacheContext`] implementation.
    fn store(&self) -> &CacheStore;
}

pub struct CacheStore {
    inner: Lazy<RwLock<HashMap<u64, &'static (dyn Any + Send + Sync)>>>,
}

impl CacheStore {
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
    pub fn read(&'_ self) -> RwLockReadGuard<'_, HashMap<u64, &'static (dyn Any + Send + Sync)>> {
        self.inner.read().unwrap()
    }

    /// Retrieves an exclusive-write access to the [`CacheStore`]'s inner map instance.
    ///
    /// # Panics
    ///
    /// This method panics if another thread write-locked the store before
    /// this method was invoked, without releasing the lock.
    pub fn write(&'_ self) -> RwLockWriteGuard<'_, HashMap<u64, &'static (dyn Any + Send + Sync)>> {
        self.inner.write().unwrap()
    }

    /// Clears the entire [`CacheStore`] of items.
    pub fn clear(&self) {
        self.write().clear();
    }

    /// Attempts to retrieve the value with the given key from the
    /// [`CacheStore`], if it exists. If the value is found within the store,
    /// it is cloned and returned as `Some(...)`. Otherwise, returns `None`.
    pub fn get<T: Clone + 'static>(&self, key: u64) -> Option<T> {
        let container = self.read();

        container.get(&key)?.downcast_ref::<T>().cloned()
    }

    /// Inserts the given value into the [`CacheStore`] instance with
    /// the given key. If a value already exists with the given key, it is
    /// overwritten.
    pub fn insert<T: Clone + Send + Sync + 'static>(&self, key: u64, value: T) {
        let mut container = self.write();

        container.insert(key, Box::leak(Box::new(value)));
    }

    /// Looks up the given cache key within the [`CacheStore`] instance. If
    /// a value is found within the store, it is cloned and returned. If the key
    /// is not found within the store, `f` is invoked and the result is cloned and
    /// inserted into the store. After the value is stored, the original, un-cloned value
    /// is returned.
    pub fn get_or_insert<T: Clone + Send + Sync + 'static>(&self, key: u64, f: impl FnOnce() -> T) -> T {
        if let Some(existing) = self.get(key) {
            return existing;
        }

        let value = f();
        self.insert(key, value.clone());

        value
    }
}

impl Default for CacheStore {
    fn default() -> Self {
        Self {
            inner: Lazy::new(|| RwLock::new(HashMap::new())),
        }
    }
}
