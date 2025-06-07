pub(crate) mod fetch;
pub mod graph;

use std::path::Path;
use std::path::PathBuf;

use error_snippet::Result;
use indexmap::IndexMap;
use lume_errors::DiagCtxHandle;
use lume_span::PackageId;
use semver::VersionReq;

use crate::deps::fetch::DependencyFetcher;
use crate::deps::fetch::FileDependencyFetcher;
use crate::deps::fetch::GitDependencyFetcher;
use crate::deps::graph::DependencyGraph;
use crate::{Package, PackageParser};

const FILE_SCHEME: &str = "file";
const GIT_SCHEME: &str = "git";

/// Defines a mapping between [`PackageId`]s and their corresponding
/// [`Package`] instances.
pub type ManifestMap = IndexMap<PackageId, Package>;

/// Defines a mapping between [`Package`] instances and all of their
/// direct dependencies, indexed by [`PackageId`]s.
pub type DependencyMap = IndexMap<PackageId, Vec<(PackageId, VersionReq)>>;

#[derive(Clone, Debug)]
pub enum DependencyPath {
    /// Defines that the path is represented using an URL,
    /// which can be fetched depending on the scheme.
    Url(url::Url),

    /// Defines that the path is some local path and
    /// can be either relative or absolute.
    Path(PathBuf),
}

impl DependencyPath {
    /// Gets the protocol or scheme of the given [`DependencyPath`]:
    ///
    /// - for [`DependencyPath::Url`], returns the scheme of the URL,
    /// - for [`DependencyPath::Path`], returns `"file"`.
    pub fn protocol(&self) -> &str {
        match self {
            Self::Url(url) => url.scheme(),
            Self::Path(_) => FILE_SCHEME,
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct Dependencies {
    /// Defines whether the parent [`Package`] should compile without linking
    /// the standard library. Defaults to [`false`].
    pub no_std: bool,

    /// Defines the graph of all dependencies from the current [`Package`] instance
    /// and descending down to all sub-dependencies, as well.
    pub graph: DependencyGraph,
}

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
        let dependency_graph = DependencyGraph::build(manifest_map, dependency_map)?;

        // Verify that all local dependencies have an allowed version number,
        // which matches their required version, as defined by their dependent.
        resolver.dcx.with(|handle| dependency_graph.verify_dependencies(handle));

        root_package.dependencies.graph = dependency_graph;

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
        let manifest = self.dcx.with(|handle| PackageParser::locate(root, handle))?;
        let dependencies = manifest.dependencies.clone();

        let package = Package::from_manifest(manifest);

        // If the manifest already exists within the set, we've already
        // visited it and we can hop out.
        if manifest_map.contains_key(&package.id) {
            return Ok(package);
        }

        let _ = manifest_map.insert(package.id, package.clone());
        let _ = dependency_map.insert(package.id, Vec::new());

        for dependency in &dependencies.dependencies {
            let dep_local_path = self.fetch_dependency(&dependency.source, &dependency.required_version)?;
            let dep_package = self.recurse_package_dependencies(&dep_local_path, manifest_map, dependency_map)?;

            dependency_map
                .get_mut(&package.id)
                .unwrap()
                .push((dep_package.id, dependency.required_version.clone()));
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
    fn fetch_dependency(&'pkg self, source: &str, version: &VersionReq) -> Result<PathBuf> {
        let dependency_path = self.parse_dependency_path(source);

        let DependencyPath::Url(url) = dependency_path.clone() else {
            return FileDependencyFetcher.fetch(dependency_path, version);
        };

        if let Some(host) = url.host_str() {
            if ["http", "https"].contains(&url.scheme()) && ["github.com", "gitlab.com"].contains(&host) {
                return GitDependencyFetcher.fetch(dependency_path, version);
            }
        }

        match url.scheme() {
            FILE_SCHEME => FileDependencyFetcher.fetch(dependency_path, version),
            GIT_SCHEME => GitDependencyFetcher.fetch(dependency_path, version),
            scheme => Err(error_snippet::SimpleDiagnostic::new(format!(
                "protocol or scheme is not supported: {scheme}"
            ))
            .into()),
        }
    }

    /// Attempts to parse the given path to a [`DependencyPath`].
    fn parse_dependency_path(&'pkg self, path_str: &str) -> DependencyPath {
        if let Ok(url) = url::Url::parse(path_str) {
            return DependencyPath::Url(url);
        }

        let mut path = PathBuf::from(path_str);

        if path.is_relative() {
            path = self.root.join(&path);
        }

        DependencyPath::Path(path)
    }
}
