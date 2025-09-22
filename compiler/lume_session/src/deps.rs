use std::collections::HashMap;

use lume_errors::Result;
use lume_span::PackageId;
use semver::Version;

use crate::Package;

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
}

impl DependencyMap {
    /// Gets the root package as a [`Package`] reference.
    pub fn root_package(&self) -> &Package {
        let id = self.root;

        self.packages.get(&id).unwrap()
    }

    /// Adds all expected source files to all the [`Package`]s inside
    /// the dependency tree.
    pub fn add_package_sources_recursive(&mut self) -> Result<()> {
        for dependency in self.packages.values_mut() {
            dependency.add_package_sources()?;
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
