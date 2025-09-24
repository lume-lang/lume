use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap};
use std::fmt::{Debug, Display};
use std::path::PathBuf;
use std::sync::Arc;

use lume_errors::{DiagCtxHandle, Result};
use lume_session::DependencyMap;
use lume_span::PackageId;

use pubgrub::Reporter as _;
use pubgrub::VersionSet as _;
use pubgrub::{Dependencies, PackageResolutionStatistics};
use semver::{Version, VersionReq};

use crate::PackageParser;
use crate::fetch::PackageMetadata;
use crate::parser::ManifestDependencySource;

/// Builds a dependency from the root package, found at `root`.
pub(crate) fn build_dependency_tree(root: &PathBuf, dcx: DiagCtxHandle) -> Result<DependencyMap> {
    let manifest = PackageParser::locate(&root)?;
    let package = ManifestDependencySource::Local {
        path: root.as_os_str().to_string_lossy().into(),
    };

    let version = manifest.package.version.into_inner();
    let solver = DependencyResolver::new(dcx.clone());

    match pubgrub::resolve(&solver, package, version) {
        Ok(solution) => println!("{:#?}", solution),
        Err(pubgrub::PubGrubError::NoSolution(mut derivation_tree)) => {
            derivation_tree.collapse_no_versions();
            eprintln!("{}", pubgrub::DefaultStringReporter::report(&derivation_tree));
        }
        Err(err) => panic!("{:?}", err),
    };

    panic!();
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DependencyTreeNode {
    pub package_id: PackageId,
    pub version: Version,
    pub dependencies: Vec<(PackageId, VersionReq)>,
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
    fn get_or_fetch(&self, package: &ManifestDependencySource) -> Result<Arc<PackageMetadata>> {
        if let Some(existing) = self.metadata.borrow().get(package) {
            return Ok(existing.clone());
        }

        let metadata = Arc::new(package.metadata()?);
        let inserted = self
            .metadata
            .borrow_mut()
            .entry(package.to_owned())
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
    type P = ManifestDependencySource;
    type V = Version;
    type VS = VersionSet;
    type Priority = (u32, std::cmp::Reverse<Version>);
    type M = DependencyError;
    type Err = DependencyError;

    fn prioritize(
        &self,
        source: &Self::P,
        range: &Self::VS,
        package_conflicts_counts: &PackageResolutionStatistics,
    ) -> Self::Priority {
        let metadata = match self.get_or_fetch(source) {
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

    fn choose_version(&self, source: &Self::P, range: &Self::VS) -> std::result::Result<Option<Self::V>, Self::Err> {
        let metadata = match self.get_or_fetch(source) {
            Ok(metadata) => metadata,
            Err(err) => {
                self.dcx.emit_and_push(err);
                return Err(DependencyError::FetchFailed);
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
        source: &Self::P,
        version: &Self::V,
    ) -> std::result::Result<Dependencies<Self::P, Self::VS, Self::M>, Self::Err> {
        let metadata = match self.get_or_fetch(source) {
            Ok(metadata) => metadata,
            Err(err) => {
                self.dcx.emit_and_push(err);
                return Ok(Dependencies::Unavailable(DependencyError::FetchFailed));
            }
        };

        let Some(deps_for_version) = metadata.dependencies.get(version) else {
            return Ok(Dependencies::Unavailable(DependencyError::VersionNotFound));
        };

        let mut dependencies = pubgrub::Map::default();

        for (dep, constraint) in deps_for_version {
            let versions = match self.get_versions_of(dep) {
                Ok(versions) => versions,
                Err(err) => {
                    self.dcx.emit_and_push(err);
                    return Ok(Dependencies::Unavailable(DependencyError::FetchFailed));
                }
            };

            let compatible = versions
                .into_iter()
                .filter(|v| constraint.matches(v))
                .collect::<BTreeSet<_>>();

            let version_set = VersionSet::from_set(compatible);

            dependencies.insert(dep.to_owned(), version_set);
        }

        Ok(Dependencies::Available(dependencies))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum DependencyError {
    FetchFailed,
    VersionNotFound,
}

impl Display for DependencyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FetchFailed => write!(f, "failed to fetch dependency"),
            Self::VersionNotFound => write!(f, "version could not be found"),
        }
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
