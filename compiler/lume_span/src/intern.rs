//! Source files and spans within them, which are used heavily within the Lume compiler.
//!
//! This module is used by most other packages within the Lume compiler, since spans
//! are required to print useful diagnostics to the user - at least if source code is needed.

use std::any::Any;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt;
use std::hash::Hash;
use std::ops::Deref;
use std::sync::{LazyLock, RwLock};

use indexmap::IndexMap;

use crate::hash_id;

pub trait Internable
where
    Self: Sized,
{
    fn intern(&self) -> Interned<Self>;
}

impl<T: Clone + Hash + Eq + Send + Sync + 'static> Internable for T {
    fn intern(&self) -> Interned<Self> {
        Interner::with(|interner| interner.intern(self))
    }
}

#[derive(PartialEq, Eq)]
pub struct Interned<T>(usize, std::marker::PhantomData<T>);

impl<T> Clone for Interned<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Interned<T> {}

impl<T: 'static> Deref for Interned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        Interner::with(|interner| interner.get(*self))
    }
}

impl<T: fmt::Debug + 'static> fmt::Debug for Interned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", Interned::<T>::deref(self))
    }
}

impl<T: fmt::Display + 'static> fmt::Display for Interned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Interned::<T>::deref(self))
    }
}

impl<T: PartialOrd + 'static> PartialOrd for Interned<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let a = Interned::<T>::deref(self);
        let b = Interned::<T>::deref(other);

        a.partial_cmp(b)
    }
}

impl<T: Ord + 'static> Ord for Interned<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        let a = Interned::<T>::deref(self);
        let b = Interned::<T>::deref(other);

        a.cmp(b)
    }
}

impl<T: Hash + 'static> Hash for Interned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Interned::<T>::deref(self).hash(state);
    }
}

impl<T: AsRef<U> + 'static, U: ?Sized> AsRef<U> for Interned<T> {
    fn as_ref(&self) -> &U {
        Interned::<T>::deref(self).as_ref()
    }
}

unsafe impl<T> Send for Interned<T> {}
unsafe impl<T> Sync for Interned<T> {}

impl Interned<String> {
    pub fn as_str(&self) -> &str {
        self
    }
}

pub struct Interner {
    container: RwLock<IndexMap<usize, &'static (dyn Any + Send + Sync)>>,
    #[cfg(debug_assertions)]
    types: RwLock<IndexMap<usize, &'static str>>,
}

impl Interner {
    /// Creates a new lazy-locked [`Interner`], which can be used to create
    /// static instances of [`Interner`].
    const fn new_locked() -> LazyLock<Interner> {
        LazyLock::new(|| Self {
            container: RwLock::new(IndexMap::new()),
            #[cfg(debug_assertions)]
            types: RwLock::new(IndexMap::new()),
        })
    }

    /// Retrieves the amount of interned instances within the global
    /// [`Interner`] instance.
    #[inline]
    pub fn count() -> usize {
        CURRENT_INTERNER.with(|interner| match interner.container.read() {
            Ok(container) => container.len(),
            Err(_) => 0,
        })
    }

    /// Interns the given instance so it can be referenced multiple times without
    /// duplicating the memory data.
    ///
    /// # Panics
    ///
    /// Panics if the internal lock is poisoned or multiple threads are trying to
    /// access the lock simultaneously.
    #[inline]
    pub fn interned<B, T>(val: &T) -> Interned<B>
    where
        T: Clone + Eq + Hash + Send + Sync + 'static,
        B: Borrow<T>,
    {
        CURRENT_INTERNER.with(|interner| interner.intern(val))
    }

    /// Interns the given instance so it can be referenced multiple times without
    /// duplicating the memory data.
    ///
    /// # Panics
    ///
    /// Panics if the internal lock is poisoned or multiple threads are trying to
    /// access the lock simultaneously.
    #[inline]
    pub fn intern<B, T>(&self, val: &T) -> Interned<B>
    where
        T: Clone + Eq + Hash + Send + Sync + 'static,
        B: Borrow<T>,
    {
        let mut container = self.container.write().unwrap();
        let key = hash_id(val);

        if container.contains_key(&key) {
            return Interned(key, std::marker::PhantomData);
        }

        // SAFETY:
        // We allocate the value on the heap and leak it, ensuring it remains valid
        // for the lifetime of the program.
        let leaked: &'static T = Box::leak(Box::new(val.clone()));
        let cast = leaked as &(dyn Any + Send + Sync);

        container.insert(key, cast);

        #[cfg(debug_assertions)]
        {
            let mut container = self.types.write().unwrap();
            container.insert(key, std::any::type_name::<T>());
        }

        Interned(key, std::marker::PhantomData)
    }

    /// Gets an interned value from the interner.
    ///
    /// # Panics
    ///
    /// Panics if the internal lock is poisoned or if the given index is invalid.
    pub fn get<T>(&self, interned: Interned<T>) -> &'static T {
        let container = self.container.read().unwrap();
        let Some(value) = container.get(&interned.0) else {
            panic!("interned index value is invalid: {}", interned.0)
        };

        match value.downcast_ref::<T>() {
            Some(value) => value,

            #[cfg(debug_assertions)]
            None => panic!(
                "interned value is not of the expected type; expected {}, found {}",
                std::any::type_name::<T>(),
                self.types.read().unwrap().get(&interned.0).unwrap()
            ),

            #[cfg(not(debug_assertions))]
            None => panic!(
                "interned value is not of the expected type; expected {}",
                std::any::type_name::<T>(),
            ),
        }
    }

    /// Invokes some functionality on the global [`Interner`] instance.
    pub fn with<R, F>(f: F) -> R
    where
        F: FnOnce(&Interner) -> R,
    {
        CURRENT_INTERNER.with(|it| f(it))
    }
}

thread_local! {
    pub static CURRENT_INTERNER: LazyLock<Interner> = Interner::new_locked();
}

#[cfg(test)]
#[allow(clippy::borrow_as_ptr, clippy::ref_as_ptr, reason = "used for reference checking")]
mod tests {
    use crate::source::Location;

    use super::*;

    #[test]
    fn test_intern() {
        let interned: Interned<String> = Interner::interned(&String::from("test"));

        assert_eq!(interned.as_str(), "test");
    }

    #[test]
    fn test_intern_double() {
        Interner::interned::<String, String>(&String::from("test"));
        Interner::interned::<String, String>(&String::from("test"));
    }

    #[test]
    fn test_intern_2() {
        let x: Interned<String> = Interner::interned(&String::from("x"));
        let y: Interned<String> = Interner::interned(&String::from("y"));

        assert_eq!(x.as_str(), "x");
        assert_eq!(y.as_str(), "y");
    }

    #[test]
    fn test_same_value() {
        let x: Interned<String> = Interner::interned(&String::from("test"));
        let y: Interned<String> = Interner::interned(&String::from("test"));

        assert_eq!(&*x as *const String, &*y as *const String);
    }

    #[test]
    fn test_different_value() {
        let x: Interned<String> = Interner::interned(&String::from("first"));
        let y: Interned<String> = Interner::interned(&String::from("second"));

        assert_ne!(&*x as *const String, &*y as *const String);
    }

    #[test]
    fn test_different_types() {
        let x: Interned<Location> = Interner::interned(&Location::empty());
        let y: Interned<Location> = Interner::interned(&Location::empty());

        assert_eq!(&*x as *const Location, &*y as *const Location);
    }

    #[test]
    #[cfg_attr(
        debug_assertions,
        should_panic(
            expected = "interned value is not of the expected type; expected i64, found alloc::string::String"
        )
    )]
    #[cfg_attr(
        not(debug_assertions),
        should_panic(expected = "interned value is not of the expected type; expected i64")
    )]
    fn test_invalid_type() {
        let actual: Interned<String> = Interner::interned(&String::from("value"));
        let fake = Interned::<i64>(actual.0, std::marker::PhantomData);

        let _ = *fake;
    }
}
