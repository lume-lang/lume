mod git;
mod provider;
mod report;

use std::fmt::Display;
use std::path::{Path, PathBuf};

use indexmap::IndexMap;
use lume_errors::{DiagCtxHandle, Result, SimpleDiagnostic};
use lume_session::FileLoader;
pub(crate) use provider::resolve;
use semver::Version;

use crate::manifest::{Manifest, ManifestDependencySource};
use crate::parser::PackageParser;

pub struct Resolver<'io, IO> {
    io: &'io IO,
    dcx: DiagCtxHandle,
    local_paths: IndexMap<PackageKey, PathBuf>,
    local_packages: IndexMap<PackageKey, Manifest>,
}

impl<'io, IO> Resolver<'io, IO> {
    pub fn new(io: &'io IO, dcx: DiagCtxHandle) -> Self {
        Self {
            io,
            dcx,
            local_paths: IndexMap::default(),
            local_packages: IndexMap::default(),
        }
    }
}

impl<IO: FileLoader> Resolver<'_, IO> {
    pub fn fetch(&mut self, source: &PackageKey) -> Result<&PathBuf> {
        if !self.local_paths.contains_key(source) {
            let path = match source {
                PackageKey::Local(source) => source.clone(),
                PackageKey::Git { repository, rev, dir } => git::clone_repository(repository, rev, dir.as_deref())?,
            };

            self.local_paths.insert(source.clone(), path);
        }

        Ok(self.local_paths.get(source).unwrap())
    }

    pub fn manifest_of<'resolver>(&'resolver mut self, source: &PackageKey) -> Result<&'resolver Manifest> {
        if !self.local_packages.contains_key(source) {
            let source_path = self.fetch(source)?.to_owned();
            let manifest = PackageParser::locate(&source_path, self.io)?;

            self.local_packages.insert(source.clone(), manifest);
        }

        Ok(self.local_packages.get(source).unwrap())
    }

    pub fn versions_of(&mut self, source: &PackageKey) -> Result<Vec<Version>> {
        self.manifest_of(source)
            .map(|manifest| vec![manifest.package.version.clone().into_inner()])
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum PackageKey {
    /// Canonicalized absolute path to the package root.
    Local(PathBuf),

    /// Git packages are identified by `(repository, rev)`.
    ///
    /// Using the resolved SHA means two deps pinning the same commit
    /// are deduplicated, while branch/tag refs that diverge are not.
    Git {
        repository: String,
        rev: String,
        dir: Option<String>,
    },
}

impl Display for PackageKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PackageKey::Local(source) => write!(f, "{}", source.display()),
            PackageKey::Git { repository, .. } => write!(f, "{repository}"),
        }
    }
}

pub(crate) fn package_key_of<P: AsRef<Path>>(root: P, source: &ManifestDependencySource) -> Result<PackageKey> {
    match source {
        ManifestDependencySource::Local(source) => {
            let path = PathBuf::from(&source.path);
            let absolute = root.as_ref().join(path);

            match absolute.canonicalize() {
                Ok(path) => Ok(PackageKey::Local(path)),
                Err(err) => Err(
                    SimpleDiagnostic::new(format!("could not canonicalize path: {}", source.path))
                        .add_cause(err)
                        .into(),
                ),
            }
        }
        ManifestDependencySource::Git(source) => {
            let rev = git::revision_of(source)?;

            Ok(PackageKey::Git {
                repository: source.repository.clone(),
                rev,
                dir: source.dir.clone(),
            })
        }
    }
}
