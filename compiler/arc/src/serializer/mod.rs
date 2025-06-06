use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::parser::{Block, Parser, Value};
use crate::{Package, Project, errors::*};

use error_snippet::Result;
use lume_errors::DiagCtxHandle;
use lume_span::{PackageId, SourceFile};
use semver::Version;

mod dep;
mod prop;
mod version;

#[cfg(test)]
mod tests;

pub const DEFAULT_ARCFILE: &str = "Arcfile";

pub(crate) struct ProjectParser {
    /// Absolute path to the project's Arcfile.
    path: PathBuf,

    /// Handle to the diagnostics context which will handle parsing errors.
    dcx: DiagCtxHandle,

    /// Source file of the project's Arcfile.
    source: Arc<SourceFile>,

    /// Defines the parsed blocks within the source file.
    blocks: Vec<Block>,

    /// Defines the current Lume version.
    current_lume_version: Version,
}

impl ProjectParser {
    /// Creates a new [`ProjectParser`] instance using the given `Arcfile` source file path.
    ///
    /// # Errors
    ///
    /// This method may fail if the given path is unreadable or
    /// otherwise inaccessible.
    fn new(path: &Path, dcx: DiagCtxHandle) -> Result<Self> {
        let content = match std::fs::read_to_string(path) {
            Ok(content) => content,
            Err(err) => {
                return Err(ArcfileIoError {
                    inner: vec![err.into()],
                }
                .into());
            }
        };

        let file_name: String = match path.file_name() {
            Some(os_str) => os_str.to_string_lossy().into_owned(),
            None => DEFAULT_ARCFILE.into(),
        };

        let source = SourceFile::new(PackageId::empty(), file_name, content);

        Self::from_source(path, Arc::new(source), dcx)
    }

    /// Creates a new [`ProjectParser`] instance using the given `Arcfile` source file.
    ///
    /// # Errors
    ///
    /// This method may fail if the given `Arcfile` is improperly formatted or
    /// otherwise invalid.
    pub fn from_source(path: &Path, source: Arc<SourceFile>, mut dcx: DiagCtxHandle) -> Result<Self> {
        let blocks = dcx.with(|handle| Parser::new(source.clone(), handle).parse())?;

        Ok(Self {
            path: path.to_path_buf(),
            source,
            dcx,
            blocks,
            current_lume_version: Self::current_lume_version(),
        })
    }

    /// Creates a new [`ProjectParser`] instance by locating the `Arcfile` in the given root directory.
    ///
    /// # Errors
    ///
    /// This method may fail if:
    /// - the given path has no `Arcfile` stored within it,
    /// - the located `Arcfile` doesn't refer to a file
    /// - or if the given `Arcfile` is otherwise invalid
    pub fn locate(root: &Path, dcx: DiagCtxHandle) -> Result<Project> {
        let path = root.join(DEFAULT_ARCFILE);
        if !path.is_file() {
            return Err(ArcfileMissing {
                dir: root.to_path_buf(),
            }
            .into());
        }

        ProjectParser::new(&path, dcx)?.parse()
    }

    /// Parses a [`Project`] from the input source file.
    fn parse(&mut self) -> Result<Project> {
        let project = Project {
            path: self.path.clone(),
            packages: self.parse_packages()?,
        };

        Ok(project)
    }

    /// Parses a list of zero-or-more [`Package`] instances from the input source code.
    fn parse_packages(&mut self) -> Result<Vec<Package>> {
        let mut packages = Vec::new();

        for block in &self.blocks {
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

            packages.push(self.parse_package(block)?);
        }

        if packages.is_empty() {
            return Err(ArcfileNoPackages {
                path: self.path.to_string_lossy().into_owned(),
            }
            .into());
        }

        Ok(packages)
    }

    /// Parses a single [`Package`] instance from the given [`Block`].
    fn parse_package(&self, block: &Block) -> Result<Package> {
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
            Some(prop) => Some(self.version(prop)?),
            None => None,
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

        let package = Package {
            id: PackageId::from_name(&name),
            path: self.path.parent().unwrap().to_path_buf(),
            name,
            lume_version,
            version,
            description,
            license,
            repository,
            files: Vec::new(),
            dependencies,
        };

        self.verify_lume_version(&package)?;

        Ok(package)
    }
}
