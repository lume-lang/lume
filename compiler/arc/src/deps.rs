use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap};
use std::fmt::{Debug, Display};
use std::path::PathBuf;
use std::sync::Arc;

use lume_errors::{DiagCtxHandle, Result, SimpleDiagnostic};
use lume_session::DependencyMap;
use lume_span::PackageId;

use pubgrub::VersionSet as _;
use pubgrub::{Dependencies, PackageResolutionStatistics};
use semver::Version;

use crate::PackageParser;
use crate::fetch::PackageMetadata;
use crate::parser::ManifestDependencySource;

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Dependency {
    /// Defines the name of the dependency in the manifest.
    pub id: PackageId,

    /// Defines the name of the dependency in the manifest.
    pub name: String,

    /// Defines where the dependency can be found.
    pub source: ManifestDependencySource,
}

impl Display for Dependency {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.name, f)
    }
}

/// Builds a dependency from the root package, found at `root`.
pub(crate) fn build_dependency_tree(root: &PathBuf, dcx: DiagCtxHandle) -> Result<DependencyMap> {
    let manifest = PackageParser::locate(&root)?;
    let root_id = manifest.package_id();

    let package = Dependency {
        id: root_id,
        name: manifest.package.name,
        source: ManifestDependencySource::Local {
            path: root.as_os_str().to_string_lossy().into(),
        },
    };

    let version = manifest.package.version.into_inner();
    let solver = DependencyResolver::new(dcx.clone());

    let solution = match pubgrub::resolve(&solver, package, version) {
        Ok(solution) => solution,
        Err(pubgrub::PubGrubError::NoSolution(mut derivation_tree)) => {
            derivation_tree.collapse_no_versions();

            let packages = solver
                .metadata
                .into_inner()
                .into_iter()
                .map(|(_, pkg)| (pkg.package_id, Arc::into_inner(pkg).unwrap()))
                .collect::<HashMap<PackageId, PackageMetadata>>();

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
        root: root_id,
        packages: HashMap::new(),
        resolved: HashMap::new(),
    };

    for (dependency, version) in solution {
        let local_path = dependency.source.fetch()?;
        let manifest = PackageParser::locate(&local_path)?;
        let package = manifest.into();

        map.packages.insert(dependency.id, package);
        map.resolved.insert(dependency.id, version);
    }

    Ok(map)
}

struct DependencyResolver {
    dcx: DiagCtxHandle,
    metadata: RefCell<HashMap<ManifestDependencySource, Arc<PackageMetadata>>>,
}

impl DependencyResolver {
    pub fn new(dcx: DiagCtxHandle) -> Self {
        Self {
            dcx,
            metadata: RefCell::new(HashMap::new()),
        }
    }

    /// Gets the metadata of the given package source from the cache
    /// or fetches it from the appropriate fetcher.
    fn get_or_fetch(&self, source: &ManifestDependencySource) -> Result<Arc<PackageMetadata>> {
        if let Some(existing) = self.metadata.borrow().get(source) {
            return Ok(existing.clone());
        }

        let metadata = Arc::new(source.get_metadata()?);
        let inserted = self
            .metadata
            .borrow_mut()
            .entry(source.to_owned())
            .or_insert(metadata)
            .clone();

        Ok(inserted)
    }

    /// Gets a list of all the versions of the given package.
    fn get_versions_of(&self, package: &ManifestDependencySource) -> Result<Vec<Version>> {
        let metadata = self.get_or_fetch(package)?;

        Ok(metadata.dependencies.keys().cloned().collect::<Vec<_>>())
    }
}

impl pubgrub::DependencyProvider for DependencyResolver {
    type P = Dependency;
    type V = Version;
    type VS = VersionSet;
    type Priority = (u32, std::cmp::Reverse<Version>);
    type M = DependencyError;
    type Err = DependencyError;

    fn prioritize(
        &self,
        package: &Self::P,
        range: &Self::VS,
        package_conflicts_counts: &PackageResolutionStatistics,
    ) -> Self::Priority {
        let metadata = match self.get_or_fetch(&package.source) {
            Ok(metadata) => metadata,
            Err(err) => {
                self.dcx.emit_and_push(err);
                return (u32::MAX, std::cmp::Reverse(Version::new(0, 0, 0)));
            }
        };

        let version = metadata.dependencies.keys().find(|version| range.contains(version));

        if let Some(version) = version {
            let conflict_count = package_conflicts_counts.conflict_count();

            return (conflict_count, std::cmp::Reverse(version.to_owned()));
        }

        (u32::MAX, std::cmp::Reverse(Version::new(0, 0, 0)))
    }

    fn choose_version(&self, package: &Self::P, range: &Self::VS) -> std::result::Result<Option<Self::V>, Self::Err> {
        let metadata = match self.get_or_fetch(&package.source) {
            Ok(metadata) => metadata,
            Err(err) => {
                self.dcx.emit_and_push(err);

                return Err(DependencyError::PackageFetchFailed {
                    source: package.source.clone(),
                });
            }
        };

        let version = metadata
            .dependencies
            .keys()
            .filter(|v| range.contains(v))
            .max()
            .cloned();

        Ok(version)
    }

    fn get_dependencies(
        &self,
        package: &Self::P,
        version: &Self::V,
    ) -> std::result::Result<Dependencies<Self::P, Self::VS, Self::M>, Self::Err> {
        let metadata = match self.get_or_fetch(&package.source) {
            Ok(metadata) => metadata,
            Err(err) => {
                self.dcx.emit_and_push(err);

                return Ok(Dependencies::Unavailable(DependencyError::PackageFetchFailed {
                    source: package.source.clone(),
                }));
            }
        };

        let Some(deps_for_version) = metadata.dependencies.get(version) else {
            return Ok(Dependencies::Unavailable(DependencyError::VersionNotFound));
        };

        let mut dependencies = pubgrub::Map::default();

        for (dep, constraint) in deps_for_version {
            let metadata = match self.get_or_fetch(dep) {
                Ok(metadata) => metadata,
                Err(err) => {
                    self.dcx.emit_and_push(err);

                    return Ok(Dependencies::Unavailable(DependencyError::PackageFetchFailed {
                        source: dep.clone(),
                    }));
                }
            };

            let dependency = Dependency {
                id: metadata.package_id,
                name: metadata.name.clone(),
                source: dep.to_owned(),
            };

            let versions = match self.get_versions_of(dep) {
                Ok(versions) => versions,
                Err(err) => {
                    self.dcx.emit_and_push(err);
                    return Ok(Dependencies::Unavailable(DependencyError::VersionsFetchFailed {
                        package: dependency.id,
                    }));
                }
            };

            let compatible = versions.into_iter().filter(|v| constraint.matches(v));

            dependencies.insert(dependency, VersionSet::from_iter(compatible));
        }

        Ok(Dependencies::Available(dependencies))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum DependencyError {
    /// Failed to fetch a package from the corresponding provider.
    PackageFetchFailed { source: ManifestDependencySource },

    /// Failed to fetch package versions from the corresponding provider.
    VersionsFetchFailed { package: PackageId },

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
struct VersionSet {
    // We use a BTreeSet to keep the versions sorted and unique.
    versions: BTreeSet<Version>,
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
            // Intersection of two inverted sets is the complement of the union of the versions.
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

fn report(
    packages: HashMap<PackageId, PackageMetadata>,
    derivation_tree: &pubgrub::DerivationTree<Dependency, VersionSet, DependencyError>,
) -> String {
    let formatter = Reporter {
        packages,
        fallback: pubgrub::DefaultStringReportFormatter,
    };

    match derivation_tree {
        pubgrub::DerivationTree::External(external) => pubgrub::ReportFormatter::format_external(&formatter, external),
        pubgrub::DerivationTree::Derived(_) => todo!(),
    }
}

#[derive(Default)]
struct Reporter {
    packages: HashMap<PackageId, PackageMetadata>,
    fallback: pubgrub::DefaultStringReportFormatter,
}

impl pubgrub::ReportFormatter<Dependency, VersionSet, DependencyError> for Reporter {
    type Output = String;

    // Format an [External] incompatibility.
    fn format_external(&self, external: &pubgrub::External<Dependency, VersionSet, DependencyError>) -> Self::Output {
        match external {
            pubgrub::External::FromDependencyOf(pkg_a, set_a, pkg_b, set_b) => {
                let metadata_a = self.packages.get(&pkg_a.id).expect("expected metadata of package");
                let (version_a, constraint_b) = metadata_a
                    .dependencies
                    .iter()
                    .find_map(|(version, deps)| {
                        if let Some(constraint) = deps.get(&pkg_b.source)
                            && set_a.contains(version)
                        {
                            Some((version, constraint))
                        } else {
                            None
                        }
                    })
                    .unwrap();

                if !set_b.versions.iter().any(|v| constraint_b.matches(v)) {
                    return format!(
                        "{pkg_a} {version_a} depends on {pkg_b} {constraint_b}, but no version matches constraint"
                    );
                }

                // Fallback to default behaviour
                external.to_string()
            }
            pubgrub::External::NoVersions(pkg, set) => format!("could not find versions {set} of {pkg}"),
            pubgrub::External::NotRoot(_, _) => unreachable!(),
            pubgrub::External::Custom(pkg, set, m) => match m {
                DependencyError::PackageFetchFailed { source } => {
                    format!("could not fetch dependency {source}, required by {pkg} {set}")
                }
                DependencyError::VersionsFetchFailed { package } => {
                    let package = self.packages.get(package).unwrap();

                    format!(
                        "could not retrieve versions for {}, required by {pkg} {set}",
                        package.name
                    )
                }
                DependencyError::VersionNotFound => format!("could not find any matching version of {pkg}"),
            },
        }
    }

    /// Format terms of an incompatibility.
    fn format_terms(&self, terms: &pubgrub::Map<Dependency, pubgrub::Term<VersionSet>>) -> Self::Output {
        pubgrub::ReportFormatter::<Dependency, VersionSet, DependencyError>::format_terms(&self.fallback, terms)
    }

    /// Simplest case, we just combine two external incompatibilities.
    fn explain_both_external(
        &self,
        external1: &pubgrub::External<Dependency, VersionSet, DependencyError>,
        external2: &pubgrub::External<Dependency, VersionSet, DependencyError>,
        current_terms: &pubgrub::Map<Dependency, pubgrub::Term<VersionSet>>,
    ) -> Self::Output {
        self.fallback.explain_both_external(external1, external2, current_terms)
    }

    /// Both causes have already been explained so we use their refs.
    fn explain_both_ref(
        &self,
        ref_id1: usize,
        derived1: &pubgrub::Derived<Dependency, VersionSet, DependencyError>,
        ref_id2: usize,
        derived2: &pubgrub::Derived<Dependency, VersionSet, DependencyError>,
        current_terms: &pubgrub::Map<Dependency, pubgrub::Term<VersionSet>>,
    ) -> Self::Output {
        self.fallback
            .explain_both_ref(ref_id1, derived1, ref_id2, derived2, current_terms)
    }

    /// One cause is derived (already explained so one-line),
    /// the other is a one-line external cause,
    /// and finally we conclude with the current incompatibility.
    fn explain_ref_and_external(
        &self,
        ref_id: usize,
        derived: &pubgrub::Derived<Dependency, VersionSet, DependencyError>,
        external: &pubgrub::External<Dependency, VersionSet, DependencyError>,
        current_terms: &pubgrub::Map<Dependency, pubgrub::Term<VersionSet>>,
    ) -> Self::Output {
        self.fallback
            .explain_ref_and_external(ref_id, derived, external, current_terms)
    }

    /// Add an external cause to the chain of explanations.
    fn and_explain_external(
        &self,
        external: &pubgrub::External<Dependency, VersionSet, DependencyError>,
        current_terms: &pubgrub::Map<Dependency, pubgrub::Term<VersionSet>>,
    ) -> Self::Output {
        self.fallback.and_explain_external(external, current_terms)
    }

    /// Add an already explained incompat to the chain of explanations.
    fn and_explain_ref(
        &self,
        ref_id: usize,
        derived: &pubgrub::Derived<Dependency, VersionSet, DependencyError>,
        current_terms: &pubgrub::Map<Dependency, pubgrub::Term<VersionSet>>,
    ) -> Self::Output {
        self.fallback.and_explain_ref(ref_id, derived, current_terms)
    }

    /// Add an already explained incompat to the chain of explanations.
    fn and_explain_prior_and_external(
        &self,
        prior_external: &pubgrub::External<Dependency, VersionSet, DependencyError>,
        external: &pubgrub::External<Dependency, VersionSet, DependencyError>,
        current_terms: &pubgrub::Map<Dependency, pubgrub::Term<VersionSet>>,
    ) -> Self::Output {
        self.fallback
            .and_explain_prior_and_external(prior_external, external, current_terms)
    }
}
