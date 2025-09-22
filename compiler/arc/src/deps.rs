use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use error_snippet::Result;
use lume_session::{DependencyMap, Package};
use lume_span::PackageId;
use semver::{Version, VersionReq};

use crate::PackageParser;

/// Builds a dependency from the root package, found at `root`.
pub(crate) fn build_dependency_tree(root: &PathBuf) -> Result<DependencyMap> {
    let mut visited_pkgs = HashSet::new();
    let mut work_queue = vec![root.clone()];

    let mut dependency_tree = DependencyTree::default();

    while let Some(root) = work_queue.pop() {
        let manifest = PackageParser::locate(&root)?;

        let package_id = manifest.package_id();
        if visited_pkgs.contains(&package_id) {
            continue;
        }

        visited_pkgs.insert(package_id);

        let mut node_dependencies = Vec::new();

        for (name, dependency) in &manifest.dependencies {
            let dependency_id = PackageId::from_name(name);

            let mut dep_local_path = dependency.fetch()?;
            if dep_local_path.is_relative() {
                dep_local_path = root.join(dep_local_path);
            }

            // If no constraint is defined, use whichever one is available.
            let version_constraint = dependency.required_version.clone();

            work_queue.push(dep_local_path);
            node_dependencies.push((dependency_id, version_constraint));
        }

        dependency_tree.add_node(manifest.into(), node_dependencies);
    }

    dependency_tree.solve()
}

#[derive(Default, Clone)]
struct DependencyTree {
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
