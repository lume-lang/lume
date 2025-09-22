use std::collections::HashSet;
use std::path::PathBuf;

use error_snippet::Result;
use lume_session::{DependencyMap, DependencyTree};
use lume_span::PackageId;
use semver::VersionReq;

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
            let version_constraint = dependency.required_version.clone().unwrap_or_else(|| VersionReq::STAR);

            work_queue.push(dep_local_path);
            node_dependencies.push((dependency_id, version_constraint));
        }

        dependency_tree.add_node(manifest.into(), node_dependencies);
    }

    dependency_tree.solve()
}
