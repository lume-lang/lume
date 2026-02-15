use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use arc::locate_package;
use indexmap::IndexMap;
use lume_errors::{DiagCtxHandle, Result};
use lume_infer::TyInferCtx;
use lume_session::{DependencyMap, FileLoader, GlobalCtx, Options, Package, Session};
use lume_span::{FileName, PackageId, SourceFile, SourceMap};
use lume_tir::TypedIR;
use lume_typech::TyCheckCtx;
use lume_types::TyCtx;

#[cfg(feature = "codegen")]
pub(crate) mod incremental;

#[cfg(feature = "codegen")]
pub mod build;

#[cfg(feature = "codegen")]
pub use build::*;

pub mod check;
pub use check::*;

#[cfg(feature = "test-support")]
pub mod test_support;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledExecutable {
    pub binary: PathBuf,
}

/// Compiler configuration
pub struct Config {
    /// Command-line input options.
    pub options: Options,

    /// Don't write any files to disk.
    pub dry_run: bool,

    /// Abstract loader for loading source files.
    pub loader: Box<dyn FileLoader>,

    /// Defines an optional list of overrides for source files.
    ///
    /// Currently, only the source files of the root package are attempted
    /// to be overriden. If the file doesn't exist within the package, it is
    /// skipped.
    pub source_overrides: Option<IndexMap<FileName, String>>,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            options: Options::default(),
            dry_run: false,
            loader: Box::new(lume_session::FileSystemLoader),
            source_overrides: None,
        }
    }
}

pub struct Driver {
    /// Defines the structure of the Arcfile within the package.
    pub package: Package,

    config: Config,
    dependencies: DependencyMap,

    /// Defines the diagnostics context for reporting errors during compilation.
    dcx: DiagCtxHandle,
}

impl Driver {
    /// Creates a new compilation driver from the given package root.
    ///
    /// This function will look for Arcfiles within the given root folder, and
    /// build the package accordingly. If no Arcfile is found, an error will
    /// be returned. Any other compilation errors will also be returned.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the given path has no `Arcfile` within it.
    pub fn from_root(root: &Path, config: Config, dcx: DiagCtxHandle) -> Result<Self> {
        let mut dependencies = dcx.with(|handle| locate_package(root, config.loader.as_ref(), handle))?;
        dependencies.add_package_sources_recursive(config.loader.as_ref())?;

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

pub struct Compiler {
    /// Defines the specific [`Package`] instance to compile.
    package: Package,

    /// Defines the global compilation context.
    gcx: Arc<GlobalCtx>,

    /// Defines the global source map for all source files.
    source_map: SourceMap,
}

impl Compiler {
    /// Parses all the source files within the current [`Package`] into HIR.
    #[libftrace::traced(level = Debug)]
    fn parse(&mut self) -> Result<lume_hir::map::Map> {
        self.gcx
            .dcx
            .with(|dcx| lume_hir_lower::LowerState::new(&self.package, &mut self.source_map, dcx).lower_into())
    }

    /// Type checks all the given source files.
    #[libftrace::traced(level = Debug)]
    fn type_check(&mut self, hir: lume_hir::map::Map) -> Result<(TyCheckCtx, TypedIR)> {
        let tcx = TyCtx::new(self.gcx.clone());

        // Defines the types of all nodes within the HIR maps.
        let mut ticx = TyInferCtx::new(tcx, hir);
        ticx.infer()?;
        libftrace::info!("finished type inference");

        // Unifies all the types within the type inference context.
        lume_unification::unify(&mut ticx)?;
        libftrace::info!("finished type unification");

        // Then, make sure they're all valid.
        let mut tccx = TyCheckCtx::new(ticx);
        tccx.typecheck()?;
        libftrace::info!("finished type checking");

        #[allow(clippy::disallowed_macros, reason = "only used in debugging")]
        if self.gcx.session.options.print_type_context {
            println!("{:#?}", tccx.tdb());
        }

        let typed_ir = lume_tir_lower::Lower::build(&tccx)?;

        Ok((tccx, typed_ir))
    }

    /// Generates MIR for all the modules within the given state object.
    #[cfg(feature = "codegen")]
    #[libftrace::traced(level = Debug)]
    fn codegen(self, tcx: &TyCheckCtx, tir: TypedIR) -> lume_mir::ModuleMap {
        let opts = self.gcx.session.options.clone();

        let mut transformer = lume_mir_lower::ModuleTransformer::create(self.package, tcx, tir.metadata, opts);
        let mir = transformer.transform(&tir.functions.into_values().collect::<Vec<_>>());

        lume_mir_opt::Optimizer::optimize(tcx, mir)
    }
}
