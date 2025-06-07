use error_snippet::Result;
use lume_errors::DiagCtxHandle;
use lume_span::PackageId;
use petgraph::prelude::DiGraphMap;
use semver::VersionReq;

use crate::Package;
use crate::deps::{DependencyMap, ManifestMap};

#[derive(Default, Debug, Clone)]
pub struct DependencyGraph {
    gcx: DiGraphMap<PackageId, VersionReq>,
    map: ManifestMap,
}

impl DependencyGraph {
    /// Builds a new [`DependencyGraph`] from the given manifests.
    ///
    /// # Errors
    ///
    /// Returns `Err` if a package has a duplicated dependency, either with the same or different
    /// version requirements.
    pub fn build(manifests: ManifestMap, dependencies: DependencyMap) -> Result<DependencyGraph> {
        let mut graph = DependencyGraph {
            gcx: DiGraphMap::<PackageId, VersionReq>::new(),
            map: manifests,
        };

        for (package_id, dependency_ids) in dependencies {
            for (dependency_id, version_req) in dependency_ids {
                graph.insert(package_id, dependency_id, version_req)?;
            }
        }

        Ok(graph)
    }

    /// Verifies the dependencies all packages in the dependency graph and
    /// checks whether they match the defined version requirement.
    ///
    /// # Errors
    ///
    /// Returns `Err` if one or more local dependencies of a package has
    /// a `SemVer` version number which doesn't match the required version defined by
    /// the dependent.
    #[allow(clippy::needless_pass_by_value)]
    pub fn verify_dependencies(&self, dcx: DiagCtxHandle) {
        for (package_id, _) in &self.map {
            self.verify_dependencies_of(*package_id, dcx.clone());
        }
    }

    /// Verifies the dependencies of the package with the given ID and
    /// checks whether they match the defined version requirement.
    ///
    /// # Errors
    ///
    /// Returns `Err` if one or more local dependencies of the given package has
    /// a `SemVer` version number which doesn't match the required version defined by
    /// the dependent.
    #[allow(clippy::needless_pass_by_value)]
    pub fn verify_dependencies_of(&self, id: PackageId, dcx: DiagCtxHandle) {
        for dependency in self.dependencies_of(id) {
            self.verify_dependency(id, dependency.id, dcx.clone());
        }
    }

    /// Verifies the given dependency of the package with the given ID and
    /// checks whether it matches the defined version requirement.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the local dependency has a `SemVer` version number which
    /// doesn't match the required version defined by the dependent.
    pub fn verify_dependency(&self, dependent: PackageId, dependency: PackageId, mut dcx: DiagCtxHandle) {
        let dependent = match self.get_err(dependent) {
            Ok(dep) => dep,
            Err(err) => {
                dcx.emit(err);
                return;
            }
        };

        let dependency = match self.get_err(dependency) {
            Ok(dep) => dep,
            Err(err) => {
                dcx.emit(err);
                return;
            }
        };

        let Some(required_version) = self.required_version(dependent.id, dependency.id) else {
            dcx.emit(
                error_snippet::SimpleDiagnostic::new(format!(
                    "could not find dependency relation in dependency graph ({} -> {})",
                    dependent.name, dependency.name
                ))
                .into(),
            );

            return;
        };

        if !required_version.matches(dependency.version.value()) {
            dcx.emit(
                error_snippet::SimpleDiagnostic::new(format!(
                    "`{}` {required_version} required by `{}` could not be found (found {})",
                    dependency.name,
                    dependent.name,
                    dependency.version.value(),
                ))
                .into(),
            );
        }
    }

    /// Gets the [`Package`] instance with the given ID, if any.
    pub fn get(&self, id: PackageId) -> Option<&Package> {
        self.map.get(&id)
    }

    /// Gets the [`Package`] instance with the given ID.
    ///
    /// # Errors
    ///
    /// Returns `Err` if no [`Package`] with the given ID could be found within the graph.
    pub fn get_err(&self, id: PackageId) -> Result<&Package> {
        match self.get(id) {
            Some(package) => Ok(package),
            None => Err(error_snippet::SimpleDiagnostic::new(format!(
                "could not find dependency in dependency graph ({id:?})"
            ))
            .into()),
        }
    }

    /// Gets the [`Package`] instance with the given ID.
    ///
    /// # Panics
    ///
    /// Panics if no [`Package`] with the given ID could be found within the graph.
    pub fn get_expect(&self, id: PackageId) -> &Package {
        match self.get(id) {
            Some(package) => package,
            None => panic!("bug!: could not find package in dependency graph ({id:?})"),
        }
    }

    /// Inserts a dependency relationship between `dependent` and `dependency`, where
    /// `dependency` depends on `dependency` and has some version requirement of `version_req`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the relationship is already established within the graph.
    pub fn insert(&mut self, dependent: PackageId, dependency: PackageId, version_req: VersionReq) -> Result<()> {
        // `add_node` will not overwrite and add a new node, if the given key
        // already exists within the graph. If it does, it just returns the existing node.
        let dependent_node = self.gcx.add_node(dependent);
        let dependency_node = self.gcx.add_node(dependency);

        if let Some(existing) = self.gcx.add_edge(dependency_node, dependent_node, version_req) {
            return Err(error_snippet::SimpleDiagnostic::new(format!(
                "bug!: dependency relation already exists in graph (existing {existing})",
            ))
            .into());
        }

        Ok(())
    }

    /// Gets all the direct dependencies of the [`Package`] with the given ID.
    pub fn dependencies_of(&self, id: PackageId) -> Vec<&Package> {
        let mut dependencies = Vec::new();
        let neighbors = self.gcx.neighbors_directed(id, petgraph::Direction::Incoming);

        for dependency_id in neighbors {
            dependencies.push(self.get_expect(dependency_id));
        }

        dependencies
    }

    /// Gets the version required for `dependency` which is defined by `dependent`.
    ///
    /// If both packages are found and has a direct dependency relation, returns [`Some`].
    /// Otherwise, returns [`None`].
    pub fn required_version(&self, dependent: PackageId, dependency: PackageId) -> Option<&VersionReq> {
        self.gcx.edge_weight(dependency, dependent)
    }
}
