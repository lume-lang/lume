use std::path::Path;
use std::path::PathBuf;

use error_snippet::Result;
use lume_errors::DiagCtxHandle;
use lume_session::DependencyGraph;
use lume_session::dep_graph::DependencyMap;
use lume_session::dep_graph::ManifestMap;
use semver::VersionReq;

use crate::fetch::DependencyFetcher;
use crate::fetch::FileDependencyFetcher;
use crate::fetch::GitDependencyFetcher;
use crate::parser::{ManifestDependency, ManifestDependencySource};
use crate::{Package, PackageParser};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dependency {
    /// If the dependency is located on the same filesystem
    /// as the referencing package, defines the path to the dependency
    /// root.
    ///
    /// If the dependency is remote, whether it's a networked location or
    /// from an internet registry, defines the path to the local copy
    /// of the dependency source.
    pub source: PathBuf,

    /// Defines the required version of the dependency, which is required
    /// by the referencing package.
    pub required_version: VersionReq,
}

pub struct DependencyResolver<'pkg> {
    /// Defines the path to the package directory, which is at the
    /// root of the dependency tree, which is to be resolved.
    root: &'pkg Path,

    /// Defines the diagnostics context to report errors.
    dcx: DiagCtxHandle,
}

impl<'pkg> DependencyResolver<'pkg> {
    /// Resolves the given serialized dependencies into a local cache,
    /// which can be parsed and used to compile the given package.
    ///
    /// # Errors
    ///
    /// Returns `Err` if a path for one-or-more of the given dependencies
    /// is invalid for the implementation or the dependency was found, but
    /// inaccessible or invalid.
    pub fn resolve(root: &PathBuf, dcx: DiagCtxHandle) -> Result<Package> {
        let mut resolver = DependencyResolver { root, dcx };

        let (mut root_package, manifest_map, dependency_map) = resolver.resolve_package_dependencies(root)?;
        let graph = DependencyGraph::build(manifest_map, dependency_map)?;

        // Verify that all local dependencies have an allowed version number,
        // which matches their required version, as defined by their dependent.
        resolver.dcx.with_res(|handle| graph.verify_dependencies(handle))?;

        root_package.dependencies.graph = graph;

        Ok(root_package)
    }

    /// Parses the `Arcfile` at the given package directory and records all defined
    /// dependencies within it and recursively descends into defined sub-dependencies.
    fn resolve_package_dependencies(&mut self, root: &Path) -> Result<(Package, ManifestMap, DependencyMap)> {
        let mut manifest_map = ManifestMap::new();
        let mut dependency_map = DependencyMap::new();

        let package = self.recurse_package_dependencies(root, &mut manifest_map, &mut dependency_map)?;

        Ok((package, manifest_map, dependency_map))
    }

    /// Parses the `Arcfile` at the given package directory and records all defined
    /// dependencies within it and recursively descends into defined sub-dependencies.
    fn recurse_package_dependencies(
        &mut self,
        root: &Path,
        manifest_map: &mut ManifestMap,
        dependency_map: &mut DependencyMap,
    ) -> Result<Package> {
        let manifest = PackageParser::locate(root)?;
        let dependencies = manifest.dependencies.clone();

        let package: Package = manifest.into();

        // If the manifest already exists within the set, we've already
        // visited it and we can hop out.
        if manifest_map.contains_key(&package.id) {
            return Ok(package);
        }

        let _ = manifest_map.insert(package.id, package.clone());
        let _ = dependency_map.insert(package.id, Vec::new());

        for (_, dependency) in &dependencies {
            let dep_local_path = self.fetch_dependency(dependency)?;
            let dep_package = self.recurse_package_dependencies(&dep_local_path, manifest_map, dependency_map)?;

            let version_req = dependency.required_version.clone().unwrap_or_else(|| VersionReq::STAR);

            dependency_map
                .get_mut(&package.id)
                .unwrap()
                .push((dep_package.id, version_req));
        }

        Ok(package)
    }

    /// Fetches the given dependency to a local cache, where it can be parsed
    /// and included within the compilation.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the path is invalid for the implementation or the dependency
    /// was found, but inaccessible or invalid.
    fn fetch_dependency(&'pkg self, dependency: &ManifestDependency) -> Result<PathBuf> {
        let path = match dependency.source {
            ManifestDependencySource::Local { .. } => FileDependencyFetcher.fetch(dependency)?,
            ManifestDependencySource::Git { .. } => GitDependencyFetcher.fetch(dependency)?,
        };

        if path.is_relative() {
            Ok(self.root.join(&path))
        } else {
            Ok(path)
        }
    }
}
