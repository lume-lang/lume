//! Simple implementation of union-find (or disjoint-set) data structure.

use std::collections::HashMap;
use std::hash::Hash;

pub(crate) trait UnionKey: Hash + Clone + Copy + Eq {}

pub(crate) struct UnionFind<K: UnionKey> {
    parent: Vec<usize>,
    set_sizes: Vec<usize>,

    key_to_idx: HashMap<K, usize>,
    idx_to_key: Vec<K>,
}

impl<K: UnionKey> UnionFind<K> {
    /// Finds the set root of the given key, if it exists within the structure.
    ///
    /// If the key does not exist within this instance, returns [`None`].
    pub fn find(&mut self, key: K) -> Option<K> {
        let idx = self.index_of(key)?;
        let root = self.root_of_index(idx);

        Some(self.idx_to_key[root])
    }

    /// Places the two keys within the same set.
    ///
    /// If the keys already exist within the same set, returns `false`.
    /// Otherwise, returns `true`.
    pub fn union(&mut self, a: K, b: K) -> bool {
        let idx_a = self.insert(a);
        let idx_b = self.insert(b);

        let mut root_a = self.root_of_index(idx_a);
        let mut root_b = self.root_of_index(idx_b);

        if root_a == root_b {
            return false;
        }

        if self.set_sizes[root_a] < self.set_sizes[root_b] {
            std::mem::swap(&mut root_a, &mut root_b);
        }

        self.parent[root_b] = root_a;
        self.set_sizes[root_a] += root_b;

        true
    }

    /// Determines if the two keys exist within the same set.
    #[allow(dead_code, reason = "currently used in tests")]
    pub fn connected(&mut self, a: K, b: K) -> bool {
        match (self.index_of(a), self.index_of(b)) {
            (Some(idx_a), Some(idx_b)) => self.root_of_index(idx_a) == self.root_of_index(idx_b),
            _ => false,
        }
    }

    /// Gets the index of the given key.
    #[inline]
    fn index_of(&self, key: K) -> Option<usize> {
        self.key_to_idx.get(&key).copied()
    }

    /// Gets the index of the given key.
    fn root_of_index(&mut self, mut x: usize) -> usize {
        while self.parent.get(x).copied() != Some(x) {
            self.parent[x] = self.parent[self.parent[x]];
            x = self.parent[x];
        }

        x
    }

    /// Inserts a new key in the instance, if not already present.
    fn insert(&mut self, key: K) -> usize {
        if let Some(idx) = self.index_of(key) {
            return idx;
        }

        let idx = self.parent.len();
        self.key_to_idx.insert(key, idx);
        self.idx_to_key.push(key);

        self.parent.push(idx);
        self.set_sizes.push(1);

        idx
    }
}

impl<K: UnionKey> Default for UnionFind<K> {
    fn default() -> Self {
        Self {
            parent: Vec::new(),
            set_sizes: Vec::new(),
            key_to_idx: HashMap::new(),
            idx_to_key: Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Default, Hash, Debug, Clone, Copy, PartialEq, Eq)]
    struct Key(usize);

    impl UnionKey for Key {}

    #[test]
    pub fn connected() {
        let mut tbl = UnionFind::<Key>::default();

        let a = Key(1);
        let b = Key(2);

        assert!(!tbl.connected(a, b));
        assert!(tbl.union(a, b));
        assert!(tbl.connected(a, b));
    }

    #[test]
    pub fn large_set() {
        const COUNT: usize = 30_000;

        let mut tbl = UnionFind::<Key>::default();

        for i in 1..COUNT {
            let a = Key(i - 1);
            let b = Key(i);

            tbl.union(a, b);
        }

        for i in 0..COUNT {
            assert!(tbl.connected(Key(0), Key(i)));
        }
    }
}
