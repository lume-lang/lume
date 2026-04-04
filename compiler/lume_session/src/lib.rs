pub mod deps;
mod errors;
pub mod stdlib;

use std::fmt::Display;
use std::path::{Path, PathBuf};
use std::sync::Arc;

pub use deps::*;
use error_snippet::{IntoDiagnostic, Result};
use indexmap::IndexMap;
use lume_architect::{Database, DatabaseContext};
use lume_errors::DiagCtx;
use lume_span::{FileName, PackageId, SourceFile};
use semver::{Version, VersionReq};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone)]
#[allow(clippy::struct_excessive_bools)]
pub struct Options {
    /// Defines whether the type context should be printed to `stdio`, after
    /// it's been inferred and type checked.
    pub print_type_context: bool,

    /// Defines the optimization level for the generated LLVM IR.
    pub optimize: OptimizationLevel,

    /// Defines which linker to use when linking objects together.
    pub linker: Option<LinkerPreference>,

    /// Defines the amount of debug information to preserve in the binary.
    pub debug_info: DebugInfo,

    /// Defines the name of the entrypoint function.
    pub entrypoint: String,

    /// Defines whether incremental compilation is enabled.
    pub enable_incremental: bool,

    /// Defines the absolute path to the runtime library.
    pub runtime_path: Option<PathBuf>,

    /// Defines the path to the for the output artifacts.
    ///
    /// If the path is relative, it will be resolved relative to the root
    /// package directory. If it is absolute, it will be used as is.
    pub output_directory: Option<PathBuf>,

    /// Defines whether the generated HIR should be printed to `stdio`.
    pub dump_hir: bool,

    /// Defines whether the generated MIR should be printed to `stdio`.
    pub dump_mir: Option<Vec<String>>,

    /// Defines which MIR functions to dump, if any.
    pub dump_mir_func: Vec<String>,

    /// Defines whether the generated codegen IR should be printed to `stdio`.
    pub dump_codegen_ir: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            print_type_context: false,
            optimize: OptimizationLevel::default(),
            linker: None,
            debug_info: DebugInfo::default(),
            entrypoint: String::from("main"),
            enable_incremental: true,
            runtime_path: None,
            output_directory: None,
            dump_hir: false,
            dump_mir: None,
            dump_mir_func: Vec::new(),
            dump_codegen_ir: false,
        }
    }
}

/// Defines how much the generated IR should be optimized.
#[derive(Serialize, Deserialize, Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptimizationLevel {
    O0,
    O1,
    #[default]
    O2,
    O3,
    Os,
    Oz,
}

impl OptimizationLevel {
    /// Gets the runtime "speed level" of the given optimization level.
    ///
    /// Each optimization level has a speed level, which helps determine how
    /// important the **runtime speed** of the compiled binary is.
    ///
    /// Currently, the levels are:
    ///
    /// | Optimization level | Speed level |
    /// |--------------------|-------------|
    /// | `O0`               | 0           |
    /// | `O1`               | 1           |
    /// | `O2`               | 2           |
    /// | `O3`               | 3           |
    /// | `Os`               | 2           |
    /// | `Oz`               | 2           |
    #[inline]
    pub fn speed_level(self) -> u8 {
        match self {
            Self::O0 => 0,
            Self::O1 => 1,
            Self::O2 | Self::Os | Self::Oz => 2,
            Self::O3 => 3,
        }
    }

    /// Gets the binary "size level" of the given optimization level.
    ///
    /// Each optimization level has a size level, which helps determine how
    /// important the **binary size** of the compiled binary is.
    ///
    /// Currently, the levels are:
    ///
    /// | Optimization level | Size level |
    /// |--------------------|------------|
    /// | `O0`               | 0          |
    /// | `O1`               | 1          |
    /// | `O2`               | 1          |
    /// | `O3`               | 1          |
    /// | `Os`               | 2          |
    /// | `Oz`               | 1          |
    #[inline]
    pub fn size_level(self) -> u8 {
        match self {
            Self::O0 => 0,
            Self::O1 | Self::O2 | Self::O3 | Self::Oz => 1,
            Self::Os => 2,
        }
    }
}

/// Defines how much debug information should be included in the binary.
#[derive(Serialize, Deserialize, Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DebugInfo {
    None,
    Partial,
    #[default]
    Full,
}

impl DebugInfo {
    /// Determines whether to embed source content into the binary, given the
    /// debug info level.
    pub fn embed_sources(self) -> bool {
        self >= Self::Full
    }
}

/// Defines how much the generated IR should be optimized.
#[derive(Serialize, Deserialize, Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum LinkerPreference {
    #[default]
    Clang,
    Gcc,
}

/// Represents a compilation session, invoked by the driver.
pub struct Session {
    pub options: Options,
    pub loader: Box<dyn FileLoader>,
    pub workspace_root: PathBuf,
    pub dep_graph: DependencyMap,
}

impl Default for Session {
    fn default() -> Self {
        Self {
            options: Options::default(),
            loader: Box::new(FileSystemLoader),
            workspace_root: PathBuf::new(),
            dep_graph: DependencyMap::default(),
        }
    }
}

unsafe impl Send for Session {}
unsafe impl Sync for Session {}

pub trait FileLoader: Send + Sync {
    /// Determines whether a file exists at the given path.
    fn exists(&self, path: &Path) -> bool;

    /// Reads the contents of a file at the given path into memory.
    fn read(&self, path: &Path) -> std::io::Result<String>;

    /// Reads the contents of a directory at the given path into memory.
    fn read_dir(&self, path: &Path) -> std::io::Result<Vec<PathBuf>>;
}

pub struct FileSystemLoader;

impl FileLoader for FileSystemLoader {
    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }

    fn read(&self, path: &Path) -> std::io::Result<String> {
        std::fs::read_to_string(path)
    }

    fn read_dir(&self, path: &Path) -> std::io::Result<Vec<PathBuf>> {
        let mut entries = Vec::new();

        if !path.is_dir() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::IsADirectory,
                format!("cannot read directory: {}", path.display()),
            ));
        }

        for entry in std::fs::read_dir(path)? {
            let entry = entry?;
            let meta = entry.metadata()?;

            if meta.is_dir() && !meta.is_symlink() {
                let mut subdir = self.read_dir(&entry.path())?;
                entries.append(&mut subdir);
            }

            if meta.is_file() {
                entries.push(entry.path());
            }
        }

        Ok(entries)
    }
}

/// Global context for all compiler operations and is used to pass around data
/// to segmented stages of the compiler processs, such as parsing, analysis,
/// checking, etc.
///
/// [`GlobalCtx`] also functions as a lookup table for options and
/// session-variables, which have been defined in some previous stage, such as
/// the options passed to the compiler, callbacks to invoke during execution,
/// etc.
#[derive(Default)]
pub struct GlobalCtx {
    pub session: Session,
    pub dcx: DiagCtx,
    store: Database,
}

impl GlobalCtx {
    pub fn new(session: Session, dcx: DiagCtx) -> Self {
        Self {
            session,
            dcx,
            store: Database::new(),
        }
    }

    /// Gets a reference to the package within the given ID.
    pub fn package(&self, id: PackageId) -> Option<&Package> {
        self.session.dep_graph.packages.get(&id)
    }

    /// Defines the absolute path of the directory to
    /// place compilation artifacts within.
    ///
    /// The directory is guranteed to exist within the workspace root.
    pub fn obj_path(&self) -> PathBuf {
        match self.session.options.output_directory.as_ref() {
            Some(output_path) if output_path.is_relative() => self.session.workspace_root.join(output_path),
            Some(output_path) => output_path.to_owned(),
            None => self.session.workspace_root.join("obj"),
        }
    }

    /// Defines the absolute path of the directory to
    /// place final binary executables within.
    ///
    /// The directory is guranteed to exist within the workspace root.
    pub fn bin_path(&self) -> PathBuf {
        self.obj_path().join("bin")
    }

    pub fn obj_bc_path(&self) -> PathBuf {
        self.obj_path().join("bc")
    }

    pub fn obj_bc_path_of(&self, file: &str) -> PathBuf {
        self.obj_bc_path().join(format!("{file}.o"))
    }

    /// Defines the absolute path of the directory to place final
    /// intermediary metadata object files within.
    ///
    /// The directory is guranteed to exist within the workspace root.
    pub fn obj_metadata_path(&self) -> PathBuf {
        self.obj_path().join("meta")
    }

    pub fn binary_output_path(&self, bin_name: &str) -> PathBuf {
        self.bin_path().join(bin_name)
    }

    /// Gets the name of the package with the given ID.
    ///
    /// If the package is not found, returns [`None`].
    pub fn package_name(&self, id: PackageId) -> Option<&str> {
        if id.is_std() {
            return Some("std");
        }

        self.session.dep_graph.packages.get(&id).map(|pkg| pkg.name.as_str())
    }
}

impl DatabaseContext for GlobalCtx {
    fn db(&self) -> &Database {
        &self.store
    }
}

unsafe impl Send for GlobalCtx {}
unsafe impl Sync for GlobalCtx {}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Package {
    /// Uniquely identifies the package.
    pub id: PackageId,

    /// Defines the root directory which defines this package.
    pub path: PathBuf,

    /// Defines the name of the package.
    pub name: String,

    /// Defines the current version of the package.
    pub version: Version,

    /// Defines the minimum required version of Lume.
    pub lume_version: Option<VersionReq>,

    /// Defines an optional description of the package.
    pub description: Option<String>,

    /// Defines the license of the source code within the package. Optional.
    pub license: Option<String>,

    /// Defines the URL of the source code repository for the package.
    pub repository: Option<String>,

    /// Defines the source files defined within the [`Package`].
    pub files: IndexMap<FileName, Arc<SourceFile>>,

    /// Defines the dependencies for the package.
    pub dependencies: Dependencies,

    /// Defines the options for the runtime.
    pub runtime: lume_options::RuntimeOptions,
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
    pub fn relative_source_path<'a>(&'a self, file: &'a Path) -> PathBuf {
        let root = self.root();

        if file.is_absolute() && file.starts_with(root) {
            file.strip_prefix(root).unwrap_or(file).to_path_buf()
        } else if file.is_absolute() {
            file.to_path_buf()
        } else {
            root.join(file)
        }
    }

    /// Pushes the given source file to the [`Package`]s files.
    pub fn add_source(&mut self, file: Arc<SourceFile>) {
        if !self.files.contains_key(&file.name) {
            self.files.insert(file.name.clone(), file);
        }
    }

    /// Appends all the given source files to the [`Package`]s files.
    pub fn append_sources(&mut self, files: impl IntoIterator<Item = Arc<SourceFile>>) {
        for file in files {
            self.add_source(file);
        }
    }

    /// Adds all the files within the standard library to the [`Package`]s
    /// files.
    ///
    /// # Errors
    ///
    /// This method will return `Err` if the current path to the `Arcfile`
    /// exists outside of any directory.
    pub fn add_std_sources(&mut self) {
        self.append_sources(stdlib::Assets::as_sources(self.id));
    }

    /// Adds all the discovered source files from the project root to
    /// the projects files.
    ///
    /// By default, it will only find source files with the `.lm` file
    /// extension. If any files are explicitly excluded from within the
    /// Arcfile, they will be ignored.
    ///
    /// # Errors
    ///
    /// This method will return `Err` if the current path to the `Arcfile`
    /// exists outside of any directory.
    pub fn add_package_sources(&mut self, loader: &dyn FileLoader) -> Result<()> {
        for source_file in self.locate_source_files(loader)? {
            // We get the relative path of the file within the project,
            // so error messages don't use the full path to a file.
            let relative_path = self.relative_source_path(&source_file).to_string_lossy().to_string();

            let content = loader.read(&source_file)?;
            let source_file = Arc::new(SourceFile::new(self.id, relative_path, content));

            self.add_source(source_file);
        }

        Ok(())
    }

    /// Attempts to find all the Lume source files within the project.
    ///
    /// By default, it will only find source files with the `.lm` file
    /// extension. If any files are explicitly excluded from within the
    /// Arcfile, they will be ignored.
    ///
    /// # Errors
    ///
    /// This method will return `Err` if the current path to the `Arcfile`
    /// exists outside of any directory.
    pub fn locate_source_files(&self, loader: &dyn FileLoader) -> Result<Vec<PathBuf>> {
        match loader.read_dir(self.root()) {
            Ok(paths) => Ok(paths
                .into_iter()
                .filter(|path| path.extension() == Some("lm".as_ref()))
                .collect()),
            Err(err) => Err(errors::ArcfileGlobError {
                inner: err.into_diagnostic(),
            }
            .into()),
        }
    }

    /// Returns an iterator over all the Lume source files within the package.
    pub fn iter_sources(&self) -> impl Iterator<Item = &Arc<SourceFile>> {
        self.files.values()
    }
}

impl Default for Package {
    fn default() -> Self {
        Self {
            id: PackageId::from_usize(1881),
            path: PathBuf::new(),
            name: String::new(),
            lume_version: None,
            version: Version::new(0, 0, 0),
            description: None,
            license: None,
            repository: None,
            files: IndexMap::new(),
            dependencies: Dependencies::default(),
            runtime: lume_options::RuntimeOptions::default(),
        }
    }
}

/// Represents a unique hash for a given iteration of a package, including
/// source file content, package metadata, etc..
#[derive(Serialize, Deserialize, Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct PackageHash(lume_hash::SecureHash);

impl Display for PackageHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl Package {
    /// Generates a hash for the current package iteration.
    pub fn package_hash(&self) -> PackageHash {
        use lume_hash::SecureHasher;
        let mut state = SecureHasher::new();

        state.update(&usize::to_ne_bytes(self.id.as_usize()));
        state.update(self.name.as_bytes());
        state.update(self.version.to_string().as_bytes());

        for (file_name, source_file) in &self.files {
            state.update(file_name.to_string().as_bytes());
            state.update(source_file.content.as_bytes());
        }

        for (dependency, version) in &self.dependencies.graph {
            state.update(&usize::to_ne_bytes(dependency.as_usize()));
            state.update(version.to_string().as_bytes());
        }

        state.update(if self.dependencies.no_std { &[1] } else { &[0] });

        PackageHash(state.finalize())
    }
}

#[derive(Serialize, Deserialize, Default, Debug, Clone)]
pub struct Dependencies {
    /// Defines whether the parent [`Package`] should compile without linking
    /// the standard library. Defaults to [`false`].
    pub no_std: bool,

    /// Defines the graph of all dependencies from the current [`Package`]
    /// instance and descending down to all sub-dependencies, as well.
    pub graph: Vec<(PackageId, VersionReq)>,
}
