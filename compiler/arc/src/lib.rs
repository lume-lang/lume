pub mod deps;
pub mod errors;
pub(crate) mod parser;
pub(crate) mod serializer;
pub mod stdlib;

use crate::deps::DependencyResolver;
use crate::parser::Spanned;
use crate::serializer::{Manifest, PackageParser};
use crate::{deps::Dependencies, errors::*};

use error_snippet::{IntoDiagnostic, Result};
use glob::glob;
use lume_errors::DiagCtxHandle;
use lume_span::{PackageId, SourceFile};
use semver::{Version, VersionReq};
use std::ops::Range;
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

#[derive(Debug, Clone)]
pub struct Package {
    /// Uniquely identifies the package.
    pub id: PackageId,

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

    /// Defines the source files defined within the [`Project`].
    pub files: Vec<Arc<SourceFile>>,

    /// Defines the dependencies for the package.
    pub dependencies: Dependencies,
}

impl Package {
    /// Locates the [`Package`] in the given root directory.
    ///
    /// # Errors
    ///
    /// This method may fail if:
    /// - the given path has no `Arcfile` stored within it
    /// - or the located `Arcfile` doesn't refer to a file.
    pub fn locate(root: &PathBuf, dcx: DiagCtxHandle) -> Result<Package> {
        DependencyResolver::resolve(root, dcx)
    }

    /// Creates a new [`Package`] from the given [`Manifest`]-instance.
    pub fn from_manifest(manifest: Manifest) -> Package {
        let id = PackageId::from_name(&manifest.name);

        Self {
            id,
            path: manifest.path,
            name: manifest.name,
            lume_version: manifest.lume_version,
            version: manifest.version,
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

    /// Gets the absolute path to the project directory.
    ///
    /// The project directory is the parent directory of the `Arcfile`.
    ///
    /// # Panics
    ///
    /// This method may panic, if the current path of the project's `Arcfile`
    /// exists outside of any directory.
    pub fn root(&self) -> &Path {
        self.path.as_path()
    }

    /// Gets the relative path to a file within the project directory.
    #[expect(clippy::missing_panics_doc, reason = "infallible")]
    pub fn relative_source_path<'a>(&'a self, file: &'a Path) -> &'a Path {
        let root = self.root();

        if file.is_absolute() && file.starts_with(root) {
            file.strip_prefix(root).unwrap()
        } else {
            file
        }
    }

    /// Pushes the given source file to the [`Project`]s files.
    pub fn add_source(&mut self, file: Arc<SourceFile>) {
        self.files.push(file);
    }

    /// Appends all the given source files to the [`Project`]s files.
    pub fn append_sources(&mut self, files: impl IntoIterator<Item = Arc<SourceFile>>) {
        self.files.extend(files);
    }

    /// Adds all the files within the standard library to the [`Project`]s files.
    ///
    /// # Errors
    ///
    /// This method will return `Err` if the current path to the `Arcfile` exists
    /// outside of any directory.
    pub fn add_std_sources(&mut self) {
        self.append_sources(stdlib::Assets::as_sources(self.id));
    }

    /// Adds all the discovered source files from the project root to
    /// the projects files.
    ///
    /// By default, it will only find source files with the `.lm` file extension. If any files are explicitly
    /// excluded from within the Arcfile, they will be ignored.
    ///
    /// # Errors
    ///
    /// This method will return `Err` if the current path to the `Arcfile` exists
    /// outside of any directory.
    pub fn add_project_sources(&mut self) -> Result<()> {
        for source_file in self.locate_source_files()? {
            // We get the relative path of the file within the project,
            // so error messages don't use the full path to a file.
            let relative_path = self.relative_source_path(&source_file).to_string_lossy().to_string();

            let content = std::fs::read_to_string(source_file)?;
            let source_file = Arc::new(SourceFile::new(self.id, relative_path, content));

            self.add_source(source_file);
        }

        Ok(())
    }

    /// Attempts to find all the Lume source files within the project.
    ///
    /// By default, it will only find source files with the `.lm` file extension. If any files are explicitly
    /// excluded from within the Arcfile, they will be ignored.
    ///
    /// # Errors
    ///
    /// This method will return `Err` if the current path to the `Arcfile` exists
    /// outside of any directory.
    pub fn locate_source_files(&self) -> Result<Vec<PathBuf>> {
        let root_directory = self.root();
        let glob_pattern = format!("{}/**/*.lm", root_directory.display());
        let mut matched_files = Vec::new();

        let paths = match glob(&glob_pattern) {
            Ok(paths) => paths,
            Err(err) => {
                return Err(ArcfileGlobError {
                    inner: vec![err.into_diagnostic()],
                }
                .into());
            }
        };

        for file in paths {
            let file = match file {
                Ok(file) => file,
                Err(err) => {
                    return Err(ArcfileGlobError {
                        inner: vec![err.into_diagnostic()],
                    }
                    .into());
                }
            };

            matched_files.push(file);
        }

        Ok(matched_files)
    }
}

impl Default for Package {
    fn default() -> Self {
        Self {
            id: PackageId::empty(),
            path: PathBuf::new(),
            name: String::new(),
            lume_version: Spanned::default(),
            version: Spanned::new(Version::new(0, 0, 0), Range::<usize>::default()),
            description: None,
            license: None,
            repository: None,
            files: Vec::new(),
            dependencies: Dependencies::default(),
        }
    }
}
