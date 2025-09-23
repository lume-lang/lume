use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::path::PathBuf;
use std::sync::Arc;

use lume_errors::Result;
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
pub(crate) fn build_dependency_tree(root: &PathBuf) -> Result<DependencyMap> {
    let manifest = PackageParser::locate(&root)?;
    let package = ManifestDependencySource::Local {
        path: root.as_os_str().to_string_lossy().into(),
    };

    let version = manifest.package.version.into_inner();
    let solver = DependencyResolver::default();

    match pubgrub::resolve(&solver, package, version) {
        Ok(solution) => println!("{:?}", solution),
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

#[derive(Default)]
struct DependencyResolver {
    metadata: RefCell<HashMap<ManifestDependencySource, Arc<PackageMetadata>>>,
}

impl DependencyResolver {
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
}

impl pubgrub::DependencyProvider for DependencyResolver {
    type P = ManifestDependencySource;
    type V = Version;
    type VS = VersionSet;
    type Priority = (u32, std::cmp::Reverse<Version>);
    type M = DependencyError;
    type Err = std::convert::Infallible;

    fn prioritize(
        &self,
        source: &Self::P,
        range: &Self::VS,
        package_conflicts_counts: &PackageResolutionStatistics,
    ) -> Self::Priority {
        let metadata = self.get_or_fetch(source).unwrap();
        let version = metadata.dependencies.keys().find(|version| range.contains(version));

        if let Some(version) = version {
            let conflict_count = package_conflicts_counts.conflict_count();

            return (conflict_count, std::cmp::Reverse(version.to_owned()));
        }

        (u32::MAX, std::cmp::Reverse(Version::new(0, 0, 0)))
    }

    fn choose_version(&self, source: &Self::P, range: &Self::VS) -> std::result::Result<Option<Self::V>, Self::Err> {
        let metadata = self.get_or_fetch(source).unwrap();
        let version = metadata.dependencies.keys().find(|v| range.contains(v)).cloned();

        Ok(version)
    }

    fn get_dependencies(
        &self,
        source: &Self::P,
        version: &Self::V,
    ) -> std::result::Result<Dependencies<Self::P, Self::VS, Self::M>, Self::Err> {
        let metadata = self.get_or_fetch(source).unwrap();
        let Some(deps_for_version) = metadata.dependencies.get(version) else {
            return Ok(Dependencies::Unavailable(DependencyError::VersionNotFound));
        };

        let dependencies = deps_for_version
            .iter()
            .map(|(dep, req)| (dep.to_owned(), VersionSet(req.clone())))
            .collect::<pubgrub::Map<ManifestDependencySource, VersionSet>>();

        Ok(Dependencies::Available(dependencies))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum DependencyError {
    VersionNotFound,
}

impl Display for DependencyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VersionNotFound => write!(f, "version could not be found"),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
struct VersionSet(VersionReq);

impl From<VersionReq> for VersionSet {
    fn from(value: VersionReq) -> Self {
        Self(value)
    }
}

impl From<VersionSet> for VersionReq {
    fn from(value: VersionSet) -> Self {
        value.0
    }
}

impl Debug for VersionSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl Display for VersionSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl pubgrub::VersionSet for VersionSet {
    type V = Version;

    fn empty() -> Self {
        Self(VersionReq::STAR)
    }

    fn singleton(v: Self::V) -> Self {
        Self(VersionReq::parse(&v.to_string()).unwrap())
    }

    fn complement(&self) -> Self {
        let Some(mut comp) = self.0.comparators.first().cloned() else {
            // TODO: what would be the complement to a wildcard constraint?
            return Self(VersionReq::STAR);
        };

        comp.op = match comp.op {
            semver::Op::Exact => semver::Op::Greater,
            semver::Op::Greater => semver::Op::LessEq,
            semver::Op::GreaterEq => semver::Op::Less,
            semver::Op::Less => semver::Op::GreaterEq,
            semver::Op::LessEq => semver::Op::Greater,
            semver::Op::Tilde => semver::Op::Greater,
            semver::Op::Caret => semver::Op::Greater,
            semver::Op::Wildcard => semver::Op::Greater,
            op => unimplemented!("unsupported comparator op: {op:?}"),
        };

        Self(VersionReq {
            comparators: vec![comp],
        })
    }

    fn intersection(&self, other: &Self) -> Self {
        let mut comparators = self.0.comparators.clone();

        for comp in &other.0.comparators {
            if !comparators.contains(comp) {
                comparators.push(comp.to_owned());
            }
        }

        Self(VersionReq { comparators })
    }

    fn contains(&self, v: &Self::V) -> bool {
        self.0.matches(v)
    }
}
