use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use arc::locate_package;
use indexmap::IndexMap;
use lume_errors::{DiagCtxHandle, Result};
use lume_session::{DependencyMap, FileLoader, Options, Package, Session};
use lume_span::{FileName, PackageId, SourceFile};
use lume_typech::TyCheckCtx;

#[cfg(feature = "codegen")]
pub mod build;

pub mod check;
pub use check::*;

pub mod pipeline;
pub use pipeline::*;

#[cfg(feature = "test-support")]
pub mod test_support;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledExecutable {
    pub binary: PathBuf,
}

/// Compiler configuration
pub struct Config<IO> {
    /// Command-line input options.
    pub options: Options,

    /// Don't write any files to disk.
    pub dry_run: bool,

    /// Abstract loader for loading source files.
    pub io: IO,

    /// Defines an optional list of overrides for source files.
    ///
    /// Currently, only the source files of the root package are attempted
    /// to be overriden. If the file doesn't exist within the package, it is
    /// skipped.
    pub source_overrides: Option<IndexMap<FileName, String>>,
}

impl<IO: Default> Default for Config<IO> {
    fn default() -> Self {
        Self {
            options: Options::default(),
            dry_run: false,
            io: IO::default(),
            source_overrides: None,
        }
    }
}

pub struct Driver<IO> {
    /// Defines the structure of the Arcfile within the package.
    pub package: Package,

    config: Config<IO>,
    dependencies: DependencyMap,

    /// Defines the diagnostics context for reporting errors during compilation.
    dcx: DiagCtxHandle,
}

impl<IO> Driver<IO>
where
    IO: FileLoader,
{
    /// Creates a new compilation driver from the given package root.
    ///
    /// This function will look for Arcfiles within the given root folder, and
    /// build the package accordingly. If no Arcfile is found, an error will
    /// be returned. Any other compilation errors will also be returned.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the given path has no `Arcfile` within it.
    pub fn from_root(root: &Path, config: Config<IO>, dcx: DiagCtxHandle) -> Result<Self> {
        let mut dependencies = dcx.with(|handle| locate_package(root, &config.io, handle))?;
        dependencies.add_package_sources_recursive(&config.io)?;

        Ok(Driver {
            package: dependencies.root_package().clone(),
            config,
            dependencies,
            dcx,
        })
    }

    /// Overrides the source files of the root package, if the
    /// [`Options::source_overrides`] is set. If it is set, it is taken and
    /// consumed to replace source files in the root package.
    fn override_root_sources(&mut self) {
        if let Some(source_overrides) = self.config.source_overrides.take() {
            for (file_name, content) in source_overrides {
                let root_package = self.dependencies.root_package();
                if !root_package.files.contains_key(&file_name) {
                    continue;
                }

                let source_file = SourceFile::new(root_package.id, file_name.to_string(), content);

                self.dependencies
                    .root_package_mut()
                    .files
                    .insert(file_name, Arc::new(source_file));
            }
        }
    }
}
