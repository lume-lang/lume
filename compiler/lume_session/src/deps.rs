use std::collections::HashMap;

use lume_errors::Result;
use lume_span::PackageId;
use semver::{Version, VersionReq};

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

#[derive(Default, Clone)]
pub struct DependencyTree {
    /// Defines the root package of the tree.
    ///
    /// The root package is guaranteed to not be a dependant of a
    /// single other package within the dependency tree.
    pub root: Option<PackageId>,

    /// Defines a list of all packages within the tree.
    ///
    /// This map is only meant to be used for lookup. It is not used
    /// in the solving algorithm.
    pub packages: HashMap<PackageId, Package>,

    /// Defines all the nodes within the tree.
    ///
    /// Each node is a package, with a set of dependencies. Each dependency
    /// has some version constraint, limiting what packages can be used.
    pub nodes: HashMap<PackageId, DependencyTreeNode>,
}

impl DependencyTree {
    /// Add a new node into the tree.
    ///
    /// Each node is an entry of a [`Package`] and it's corresponding dependencies,
    /// before their versions have been resolved.
    pub fn add_node(&mut self, mut pkg: Package, dependencies: Vec<(PackageId, VersionReq)>) {
        let package_id = pkg.id;

        if self.root.is_none() {
            self.root = Some(package_id);
        }

        // Ensure the set also exists on the package itself.
        pkg.dependencies.graph = dependencies.clone();

        let node = DependencyTreeNode {
            package_id,
            version: pkg.version.clone(),
            dependencies,
        };

        self.nodes.insert(package_id, node);
        self.packages.insert(package_id, pkg);
    }

    /// Attempts to resolve all versions within the tree.
    pub fn solve(self) -> Result<DependencyMap> {
        let root = self.root.unwrap();

        let packages = self.packages;
        let resolved = self.nodes.into_iter().map(|(id, node)| (id, node.version)).collect();

        Ok(DependencyMap {
            root,
            packages,
            resolved,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DependencyTreeNode {
    pub package_id: PackageId,
    pub version: Version,
    pub dependencies: Vec<(PackageId, VersionReq)>,
}
