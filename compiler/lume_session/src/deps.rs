use std::collections::HashMap;

use lume_errors::Result;
use lume_span::PackageId;
use semver::Version;

use crate::{FileLoader, Package};

#[derive(Default, Debug, Clone)]
pub struct DependencyMap {
    /// Defines the root package of the tree.
    ///
    /// The root package is guaranteed to not be a dependant of a
    /// single other package within the dependency tree.
    pub root: PackageId,

    /// Defines all packages included in the dependency tree,
    /// keyed by their unique IDs.
    pub packages: HashMap<PackageId, Package>,

    /// Defines the resolved version of each package, which is
    /// meant to be used in the compilation process.
    pub resolved: HashMap<PackageId, Version>,

    /// Defines the dependencies and dependents of all packages in the map.
    pub tree: Tree,
}

impl DependencyMap {
    /// Gets the root package as a [`Package`] reference.
    pub fn root_package(&self) -> &Package {
        let id = self.root;

        self.packages.get(&id).unwrap()
    }

    /// Gets the root package as a [`Package`] reference.
    pub fn root_package_mut(&mut self) -> &mut Package {
        let id = self.root;

        self.packages.get_mut(&id).unwrap()
    }

    /// Adds all expected source files to all the [`Package`]s inside
    /// the dependency tree.
    pub fn add_package_sources_recursive<IO: FileLoader>(&mut self, loader: &IO) -> Result<()> {
        for dependency in self.packages.values_mut() {
            dependency.add_package_sources(loader)?;
        }

        Ok(())
    }

    /// Returns an [`Iterator`] which starts at the root package, then
    /// descends into each dependency recursively.
    pub fn iter(&self) -> impl Iterator<Item = &Package> {
        DependencyIter {
            stack: vec![self.root],
            packages: &self.packages,
        }
    }

    /// Returns an [`Iterator`] which iterates the dependencies of the the given
    /// package.
    pub fn dependencies_of(&self, package: PackageId) -> impl Iterator<Item = PackageId> {
        self.tree.dependencies.get(&package).unwrap_or(&EMPTY).iter().copied()
    }

    /// Returns an [`Iterator`] which iterates the dependents of the the given
    /// package.
    pub fn dependents_of(&self, package: PackageId) -> impl Iterator<Item = PackageId> {
        self.tree.dependents.get(&package).unwrap_or(&EMPTY).iter().copied()
    }
}

pub struct DependencyIter<'a> {
    stack: Vec<PackageId>,
    packages: &'a HashMap<PackageId, Package>,
}

impl<'a> Iterator for DependencyIter<'a> {
    type Item = &'a Package;

    fn next(&mut self) -> Option<Self::Item> {
        let id = self.stack.pop()?;
        let pkg = self.packages.get(&id)?;

        for (dep, _) in pkg.dependencies.graph.iter().rev() {
            self.stack.push(*dep);
        }

        Some(pkg)
    }
}

static EMPTY: Vec<PackageId> = Vec::<PackageId>::new();

/// Represents a tree of all packages within a dependency map, indexed with all
/// their dependencies and dependents.
#[derive(Default, Debug, Clone)]
pub struct Tree {
    dependencies: HashMap<PackageId, Vec<PackageId>>,
    dependents: HashMap<PackageId, Vec<PackageId>>,
}

impl Tree {
    /// Adds the given package to the tree and places all the given  dependency
    /// indices under it.
    pub fn add_dependencies<I>(&mut self, id: PackageId, dependencies: I)
    where
        I: IntoIterator<Item = PackageId>,
    {
        let tree_entry = self.dependencies.entry(id).or_default();

        for dependency in dependencies {
            tree_entry.push(dependency);
            self.dependents.entry(id).or_default().push(id);
        }
    }
}
