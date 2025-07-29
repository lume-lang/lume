use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::errors::*;
use crate::parser::{Block, Parser, Spanned, Value};

use error_snippet::Result;
use lume_errors::DiagCtxHandle;
use lume_session::{Dependencies, Package};
use lume_span::{PackageId, SourceFile};
use semver::{Version, VersionReq};
use url::Url;

pub(crate) mod dep;
mod errors;
mod prop;
mod version;

#[cfg(test)]
mod tests;

pub const DEFAULT_ARCFILE: &str = "Arcfile";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Manifest {
    /// Defines the root directory which defines this package.
    pub path: PathBuf,

    /// Defines the name of the package.
    pub name: String,

    /// Defines the minimum required version of Lume.
    pub lume_version: Spanned<VersionReq>,

    /// Defines the current version of the package.
    pub version: Spanned<Version>,

    /// Defines an optional description of the package.
    pub description: Option<String>,

    /// Defines the license of the source code within the package. Optional.
    pub license: Option<String>,

    /// Defines the URL of the source code repository for the package.
    pub repository: Option<String>,

    /// Defines the dependencies for the package.
    pub dependencies: ManifestDependencies,
}

impl std::hash::Hash for Manifest {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.version.value().hash(state);
    }
}

impl From<Manifest> for Package {
    fn from(manifest: Manifest) -> Self {
        let id = PackageId::from_name(&manifest.name);

        Self {
            id,
            path: manifest.path,
            name: manifest.name,
            lume_version: manifest.lume_version.into_value(),
            version: manifest.version.into_value(),
            description: manifest.description,
            license: manifest.license,
            repository: manifest.repository,
            files: Vec::new(),
            dependencies: Dependencies {
                no_std: manifest.dependencies.no_std,
                ..Dependencies::default()
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ManifestDependencies {
    /// Defines whether the parent [`Package`] should compile without linking
    /// the standard library. Defaults to [`false`].
    pub no_std: bool,

    /// Defines the dependencies with the package manifest.
    pub dependencies: Vec<ManifestDependency>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ManifestDependency {
    /// If the dependency is located on the same filesystem
    /// as the referencing package, defines the path to the dependency
    /// root.
    ///
    /// If the dependency is remote, whether it's a networked location or
    /// from an internet registry, defines the path to the local copy
    /// of the dependency source.
    pub source: String,

    /// Defines the required version of the dependency, which is required
    /// by the referencing package.
    pub required_version: VersionReq,
}

pub(crate) struct PackageParser {
    /// Absolute path to the package's Arcfile.
    path: PathBuf,

    /// Handle to the diagnostics context which will handle parsing errors.
    dcx: DiagCtxHandle,

    /// Source file of the project's Arcfile.
    source: Arc<SourceFile>,

    /// Defines the current Lume version.
    current_lume_version: Version,
}

impl PackageParser {
    /// Creates a new [`PackageParser`] instance using the given `Arcfile` source file path.
    ///
    /// # Errors
    ///
    /// This method may fail if the given path is unreadable or
    /// otherwise inaccessible.
    fn new(path: &Path, dcx: DiagCtxHandle) -> Result<Self> {
        let content = match std::fs::read_to_string(path) {
            Ok(content) => content,
            Err(err) => {
                return Err(ArcfileIoError { inner: err.into() }.into());
            }
        };

        let file_name: String = match path.file_name() {
            Some(os_str) => os_str.to_string_lossy().into_owned(),
            None => DEFAULT_ARCFILE.into(),
        };

        let source = SourceFile::new(PackageId::empty(), file_name, content);

        Ok(Self::from_source(path, Arc::new(source), dcx))
    }

    /// Creates a new [`PackageParser`] instance using the given `Arcfile` source file.
    ///
    /// # Errors
    ///
    /// This method may fail if the given `Arcfile` is improperly formatted or
    /// otherwise invalid.
    pub fn from_source(path: &Path, source: Arc<SourceFile>, dcx: DiagCtxHandle) -> Self {
        Self {
            path: path.to_path_buf(),
            source,
            dcx,
            current_lume_version: Self::current_lume_version(),
        }
    }

    /// Creates a new [`PackageParser`] instance by locating the `Arcfile` in the given root directory.
    ///
    /// # Errors
    ///
    /// This method may fail if:
    /// - the given path has no `Arcfile` stored within it,
    /// - the located `Arcfile` doesn't refer to a file
    /// - or if the given `Arcfile` is otherwise invalid
    pub fn locate(root: &Path, dcx: DiagCtxHandle) -> Result<Manifest> {
        let url = normalize_path_url(root)?;

        if url.scheme() != "file" {
            return Err(error_snippet::SimpleDiagnostic::new(format!(
                "only file:// URIs are supported, found {}",
                url.scheme()
            ))
            .into());
        }

        let root = url.to_file_path().unwrap();
        let path = root.join(DEFAULT_ARCFILE);

        if !path.is_file() {
            return Err(ArcfileMissing {
                dir: root.display().to_string(),
            }
            .into());
        }

        Self::new(&path, dcx)?.parse()
    }

    /// Parses the [`Manifest`] instance from the input source code.
    fn parse(&mut self) -> Result<Manifest> {
        let blocks = self
            .dcx
            .with(|handle| Parser::new(self.source.clone(), handle).parse())?;

        let mut package = None;

        for block in &blocks {
            if block.ty.value() != "Package" {
                self.dcx.emit(
                    ArcfileUnknownItem {
                        source: block.location.source.clone(),
                        range: block.ty.span().clone(),
                        name: block.ty.value().clone(),
                    }
                    .into(),
                );

                continue;
            }

            if package.is_some() {
                return Err(ArcfileMultiplePackages {
                    source: block.location.source.clone(),
                    range: block.ty.span().clone(),
                }
                .into());
            }

            package = Some(self.parse_package(block)?);
        }

        if let Some(pkg) = package {
            return Ok(pkg);
        }

        Err(ArcfileNoPackages {
            path: self.path.to_string_lossy().into_owned(),
        }
        .into())
    }

    /// Parses a single [`Manifest`] instance from the given [`Block`].
    fn parse_package(&self, block: &Block) -> Result<Manifest> {
        let name = match block.arguments.first() {
            Some(name) => match name {
                Value::String(str) => str.value().clone(),
                value => {
                    return Err(ArcfileUnexpectedType {
                        source: block.location.source.clone(),
                        range: value.location().clone(),
                        expected: String::from("String"),
                        found: value.to_string(),
                    }
                    .into());
                }
            },
            None => {
                return Err(ArcfileMissingName {
                    source: block.location.source.clone(),
                    range: block.location.range.clone(),
                }
                .into());
            }
        };

        let version = match block.find_prop("version") {
            Some(prop) => self.version(prop)?,
            None => {
                return Err(ArcfileMissingVersion {
                    source: block.location.source.clone(),
                    range: block.location.range.clone(),
                }
                .into());
            }
        };

        let lume_version = match block.find_prop("lume_version") {
            Some(prop) => self.version_req(prop)?,
            None => {
                return Err(ArcfileMissingLumeVersion {
                    source: block.location.source.clone(),
                    range: block.location.range.clone(),
                }
                .into());
            }
        };

        let description = self.opt_string_prop(block, "description")?;
        let license = self.opt_string_prop(block, "license")?;
        let repository = self.opt_string_prop(block, "repository")?;

        let dependencies = self.parse_dependencies(block)?;

        let manifest = Manifest {
            path: self.path.parent().unwrap().to_path_buf(),
            name,
            version,
            lume_version,
            description,
            license,
            repository,
            dependencies,
        };

        self.verify_lume_version(&manifest)?;

        Ok(manifest)
    }
}

fn normalize_path_url(path: &Path) -> Result<Url> {
    let path_str = path.as_os_str().to_str().unwrap();

    if let Ok(url) = url::Url::parse(path_str) {
        return Ok(url);
    }

    if let Ok(url) = url::Url::parse(&format!("file://{path_str}")) {
        return Ok(url);
    }

    Err(error_snippet::SimpleDiagnostic::new(format!("invalid file URI: {}", path.display())).into())
}
