pub mod dep_graph;
mod errors;
pub mod stdlib;

use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use error_snippet::{IntoDiagnostic, Result};
use glob::glob;
use lume_errors::DiagCtx;
use lume_query::{CacheContext, CacheStore};
use lume_span::{PackageId, SourceFile};
use semver::{Version, VersionReq};

pub use crate::dep_graph::DependencyGraph;

#[derive(Default)]
pub struct Options {
    /// Defines whether the type context should be printed to `stdio`, after
    /// it's been inferred and type checked.
    pub print_type_context: bool,

    /// Defines whether the generated MIR should be printed to `stdio`.
    pub print_mir: MirPrinting,

    /// Defines whether the generated LLVM IR should be printed to `stdio`.
    pub print_llvm_ir: bool,

    /// Defines the optimization level for the generated LLVM IR.
    pub optimize: OptimizationLevel,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum MirPrinting {
    #[default]
    None,
    Pretty,
    Debug,
}

/// Defines how much the generated LLVM IR should be optimized.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptimizationLevel {
    #[default]
    O0,
    O1,
    O2,
    O3,
    Os,
    Oz,
}

/// Represents a compilation session, invoked by the driver.
#[derive(Default)]
pub struct Session {
    pub options: Options,
    pub dep_graph: DependencyGraph,
}

unsafe impl Send for Session {}
unsafe impl Sync for Session {}

/// Global context for all compiler operations and is used to pass around data
/// to segmented stages of the compiler processs, such as parsing, analysis, checking,
/// etc.
///
/// [`GlobalCtx`] also functions as a lookup table for options and session-variables,
/// which have been defined in some previous stage, such as the options passed to the
/// compiler, callbacks to invoke during execution, etc.
pub struct GlobalCtx {
    pub session: Session,
    pub dcx: DiagCtx,
    store: CacheStore,
}

impl GlobalCtx {
    pub fn new(session: Session, dcx: DiagCtx) -> Self {
        Self {
            session,
            dcx,
            store: CacheStore::new(),
        }
    }
}

impl Default for GlobalCtx {
    fn default() -> Self {
        Self {
            dcx: DiagCtx::new_buffered(512),
            session: Session::default(),
            store: CacheStore::new(),
        }
    }
}

impl CacheContext for GlobalCtx {
    fn store(&self) -> &CacheStore {
        &self.store
    }
}

unsafe impl Send for GlobalCtx {}
unsafe impl Sync for GlobalCtx {}

#[derive(Debug, Clone)]
pub struct Package {
    /// Uniquely identifies the package.
    pub id: PackageId,

    /// Defines the root directory which defines this package.
    pub path: PathBuf,

    /// Defines the name of the package.
    pub name: String,

    /// Defines the minimum required version of Lume.
    pub lume_version: VersionReq,

    /// Defines the current version of the package.
    pub version: Version,

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

    /// Adds all the files within the standard library to the [`Package`]s files.
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

    /// Adds all the discovered source files from the project root to
    /// the projects files, as well as source files for the standard library (if enabled.)
    ///
    /// # Errors
    ///
    /// This method will return `Err` if the current path to the `Arcfile` exists
    /// outside of any directory.
    pub fn add_package_sources(&mut self) -> Result<()> {
        if !self.dependencies.no_std {
            self.add_std_sources();
        }

        self.add_project_sources()?;

        Ok(())
    }

    /// Adds all expected source files to the current [`Package`], as well as all
    /// nested dependencies.
    ///
    /// # Errors
    ///
    /// This method will return `Err` if the current path to the `Arcfile` exists
    /// outside of any directory.
    pub fn add_package_sources_recursive(&mut self) -> Result<()> {
        self.add_package_sources()?;

        for dependency in self.dependencies.graph.all_mut() {
            dependency.add_package_sources()?;
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
                return Err(errors::ArcfileGlobError {
                    inner: err.into_diagnostic(),
                }
                .into());
            }
        };

        for file in paths {
            let file = match file {
                Ok(file) => file,
                Err(err) => {
                    return Err(errors::ArcfileGlobError {
                        inner: err.into_diagnostic(),
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
            lume_version: VersionReq::default(),
            version: Version::new(0, 0, 0),
            description: None,
            license: None,
            repository: None,
            files: Vec::new(),
            dependencies: Dependencies::default(),
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
