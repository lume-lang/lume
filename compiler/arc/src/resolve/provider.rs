use std::collections::{BTreeSet, HashMap};
use std::fmt::{Debug, Display};
use std::path::Path;
use std::sync::RwLock;

use lume_errors::SimpleDiagnostic;
use lume_session::{DependencyMap, FileLoader, Package, Tree};
use lume_span::PackageId;
use pubgrub::{Dependencies, DependencyConstraints, PackageResolutionStatistics, VersionSet as _};
use semver::Version;

use crate::resolve::report::report;
use crate::resolve::*;

pub(crate) fn resolve<R: AsRef<Path>, IO: FileLoader>(root: R, resolver: Resolver<'_, IO>) -> Result<DependencyMap> {
    let root_path = PathBuf::from(root.as_ref());
    let root_manifest = PackageParser::locate(&root_path, resolver.io)?;
    let root_package_id = root_manifest.package_id();
    let root_version = root_manifest.package.version.into_inner();

    let resolver = RwLockResolver(resolver.dcx.clone(), RwLock::new(resolver));
    let root_package_key = PackageKey::Local(root_path);

    resolver.0.clone().with(|dcx| {
        let solution = match pubgrub::resolve(&resolver, root_package_key, root_version) {
            Ok(solution) => solution,
            Err(pubgrub::PubGrubError::NoSolution(mut derivation_tree)) => {
                derivation_tree.collapse_no_versions();

                let mut resolver = resolver.1.try_write().unwrap();
                let packages = std::mem::take(&mut resolver.local_packages);

                let report = report(packages, &derivation_tree);
                dcx.emit_and_push(SimpleDiagnostic::new(report).into());

                return Err(dcx.ensure_untainted().unwrap_err());
            }
            Err(
                pubgrub::PubGrubError::ErrorRetrievingDependencies { .. }
                | pubgrub::PubGrubError::ErrorChoosingVersion { .. }
                | pubgrub::PubGrubError::ErrorInShouldCancel(_),
            ) => {
                // These error types will always have some error raised to the
                // diagnostics context, so just return the tainted-error here.
                return Err(dcx.ensure_untainted().unwrap_err());
            }
        };

        let mut map = DependencyMap {
            root: root_package_id,
            packages: HashMap::new(),
            resolved: HashMap::new(),
            tree: Tree::default(),
        };

        let mut resolver = resolver.1.try_write().unwrap();
        let packages = std::mem::take(&mut resolver.local_packages);

        for (package, version) in solution.iter() {
            let metadata = packages.get(package).unwrap();
            let package: Package = metadata.clone().into();

            map.tree.add_dependencies(
                package.id,
                package.dependencies.graph.iter().map(|(dep, _version)| *dep),
            );

            map.resolved.insert(package.id, version.clone());
            map.packages.insert(package.id, package);
        }

        for (package, _version) in solution {
            let metadata = packages.get(&package).unwrap();
            let package_id = metadata.package_id();

            for (dependency_name, dependency) in &metadata.dependencies {
                let id = PackageId::from_name(dependency_name.as_str());

                map.packages
                    .get_mut(&package_id)
                    .unwrap()
                    .dependencies
                    .graph
                    .push((id, dependency.required_version.clone()));
            }
        }

        Ok(map)
    })
}

struct RwLockResolver<'io, IO>(DiagCtxHandle, RwLock<Resolver<'io, IO>>);

impl<IO: FileLoader> RwLockResolver<'_, IO> {
    pub fn versions_of(&self, key: &PackageKey) -> Result<Vec<Version>> {
        self.1.try_write().unwrap().versions_of(key)
    }
}

impl<IO: FileLoader> pubgrub::DependencyProvider for RwLockResolver<'_, IO> {
    type Err = DependencyError;
    type M = DependencyError;
    type P = PackageKey;
    type Priority = (u32, std::cmp::Reverse<Version>);
    type V = Version;
    type VS = VersionSet;

    fn prioritize(
        &self,
        package: &Self::P,
        range: &Self::VS,
        package_conflicts_counts: &PackageResolutionStatistics,
    ) -> Self::Priority {
        let package_versions = match self.versions_of(package) {
            Ok(versions) => versions,
            Err(err) => {
                self.0.emit_and_push(err);

                return (u32::MAX, std::cmp::Reverse(Version::new(0, 0, 0)));
            }
        };

        if let Some(version) = package_versions.into_iter().find(|version| range.contains(version)) {
            let conflict_count = package_conflicts_counts.conflict_count();

            return (conflict_count, std::cmp::Reverse(version.clone()));
        }

        (u32::MAX, std::cmp::Reverse(Version::new(0, 0, 0)))
    }

    fn choose_version(&self, package: &Self::P, range: &Self::VS) -> std::result::Result<Option<Self::V>, Self::Err> {
        match self.versions_of(package) {
            Ok(versions) => Ok(versions.into_iter().filter(|v| range.contains(v)).max()),
            Err(err) => {
                self.0.emit_and_push(err);

                Err(DependencyError::PackageFetchFailed {
                    source: package.clone(),
                })
            }
        }
    }

    fn get_dependencies(
        &self,
        package: &Self::P,
        version: &Self::V,
    ) -> std::result::Result<Dependencies<Self::P, Self::VS, Self::M>, Self::Err> {
        let (manifest_root, manifest_dependencies) = match self.1.try_write().unwrap().manifest_of(package) {
            Ok(manifest) => {
                if manifest.package.version.get_ref() != version {
                    return Err(DependencyError::VersionNotFound);
                }

                (manifest.path.clone(), manifest.dependencies.clone())
            }
            Err(err) => {
                self.0.emit_and_push(err);

                return Ok(Dependencies::Unavailable(DependencyError::PackageFetchFailed {
                    source: package.clone(),
                }));
            }
        };

        let mut dependencies = Vec::new();

        for (dependency_name, manifest_dependency) in manifest_dependencies {
            let dependency = match package_key_of(&manifest_root, &manifest_dependency.source) {
                Ok(key) => key,
                Err(err) => {
                    self.0.emit_and_push(err);

                    return Ok(Dependencies::Unavailable(DependencyError::VersionsFetchFailed {
                        package_name: dependency_name.clone(),
                    }));
                }
            };

            let compatible = match self.versions_of(&dependency) {
                Ok(versions) => versions
                    .into_iter()
                    .filter(|v| manifest_dependency.required_version.matches(v)),
                Err(err) => {
                    self.0.emit_and_push(err);

                    return Ok(Dependencies::Unavailable(DependencyError::VersionsFetchFailed {
                        package_name: dependency_name.clone(),
                    }));
                }
            };

            dependencies.push((dependency, VersionSet::from_iter(compatible)));
        }

        Ok(Dependencies::Available(DependencyConstraints::from_iter(dependencies)))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum DependencyError {
    /// Failed to fetch a package from the corresponding provider.
    PackageFetchFailed { source: PackageKey },

    /// Failed to fetch package versions from the corresponding provider.
    VersionsFetchFailed { package_name: String },

    /// Could not find a dependency with the requested version.
    VersionNotFound,
}

impl Display for DependencyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

impl std::error::Error for DependencyError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct VersionSet {
    // We use a BTreeSet to keep the versions sorted and unique.
    pub(super) versions: BTreeSet<Version>,
    inverted: bool,
}

impl VersionSet {
    pub fn from_set(versions: BTreeSet<Version>) -> Self {
        Self {
            versions,
            inverted: false,
        }
    }

    pub fn from_iter<I: Iterator<Item = Version>>(versions: I) -> Self {
        Self {
            versions: versions.collect(),
            inverted: false,
        }
    }
}

impl Display for VersionSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug_assert!(!self.inverted, "cannot format inverted version-sets");

        let joined = self
            .versions
            .iter()
            .map(std::string::ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");

        f.write_str(&joined)
    }
}

impl pubgrub::VersionSet for VersionSet {
    type V = Version;

    fn empty() -> Self {
        Self {
            versions: BTreeSet::new(),
            inverted: false,
        }
    }

    fn singleton(v: Self::V) -> Self {
        let mut versions = BTreeSet::new();
        versions.insert(v);

        Self::from_set(versions)
    }

    fn complement(&self) -> Self {
        Self {
            versions: self.versions.clone(),
            inverted: !self.inverted,
        }
    }

    fn intersection(&self, other: &Self) -> Self {
        if self.inverted && other.inverted {
            // Intersection of two inverted sets is the complement of the union of the
            // versions.
            let union: BTreeSet<_> = self.versions.union(&other.versions).cloned().collect();

            VersionSet {
                versions: union,
                inverted: true,
            }
        } else if self.inverted {
            // `self` is infinite, `other` is finite.
            let difference: BTreeSet<_> = other.versions.difference(&self.versions).cloned().collect();

            VersionSet {
                versions: difference,
                inverted: false,
            }
        } else if other.inverted {
            // `self` is finite, `other` is infinite.
            let difference: BTreeSet<_> = self.versions.difference(&other.versions).cloned().collect();

            VersionSet {
                versions: difference,
                inverted: false,
            }
        } else {
            // Intersection of two finite sets.
            let intersection: BTreeSet<_> = self.versions.intersection(&other.versions).cloned().collect();

            VersionSet {
                versions: intersection,
                inverted: false,
            }
        }
    }

    fn contains(&self, v: &Self::V) -> bool {
        if self.inverted {
            !self.versions.contains(v)
        } else {
            self.versions.contains(v)
        }
    }
}
