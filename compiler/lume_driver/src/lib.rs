use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use arc::locate_package;
use lume_errors::{DiagCtxHandle, Result};
use lume_infer::TyInferCtx;
use lume_session::{DependencyMap, GlobalCtx, Options, Package, Session};
use lume_span::{PackageId, SourceFile, SourceMap};
use lume_tir::TypedIR;
use lume_typech::TyCheckCtx;
use lume_types::TyCtx;

#[cfg(feature = "codegen")]
pub mod build;

#[cfg(feature = "codegen")]
pub use build::*;

pub mod check;
pub use check::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledExecutable {
    pub binary: PathBuf,
}

pub struct Driver {
    /// Defines the structure of the Arcfile within the package.
    pub package: Package,

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
    pub fn from_root(root: &Path, dcx: DiagCtxHandle) -> Result<Self> {
        let mut dependencies = dcx.with(|handle| locate_package(root, handle))?;

        dependencies.add_package_sources_recursive()?;

        Ok(Driver {
            package: dependencies.root_package().clone(),
            dependencies,
            dcx,
        })
    }

    /// Overrides the source files of the root package, if the
    /// [`Options::source_overrides`] is set. If it is set, it is taken and
    /// consumed to replace source files in the root package.
    fn override_root_sources(&mut self, options: &mut Options) {
        if let Some(source_overrides) = options.source_overrides.take() {
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
            .with(|dcx| lume_hir_lower::LowerState::lower(&self.package, &mut self.source_map, dcx))
    }

    /// Type checks all the given source files.
    #[libftrace::traced(level = Debug)]
    fn type_check(&mut self, hir: lume_hir::map::Map) -> Result<(TyCheckCtx, TypedIR)> {
        let tcx = TyCtx::new(self.gcx.clone());

        // Defines the types of all nodes within the HIR maps.
        let mut ticx = TyInferCtx::new(tcx, hir);
        ticx.infer()?;
        libftrace::info!("finished type inference");

        // Then, make sure they're all valid.
        let mut tccx = TyCheckCtx::new(ticx);
        tccx.typecheck()?;
        libftrace::info!("finished type checking");

        if self.gcx.session.options.print_type_context {
            println!("{:#?}", tccx.tdb());
        }

        let typed_ir = lume_tir_lower::Lower::build(&tccx)?;

        Ok((tccx, typed_ir))
    }

    /// Generates MIR for all the modules within the given state object.
    #[cfg(feature = "codegen")]
    #[libftrace::traced(level = Debug)]
    fn codegen(self, tcx: &TyCheckCtx, tir: TypedIR) -> Result<lume_mir::ModuleMap> {
        let opts = self.gcx.session.options.clone();
        let mir = lume_mir_lower::ModuleTransformer::transform(self.package, tcx, tir, opts);

        let mir = lume_mir_opt::Optimizer::optimize(tcx, mir);

        Ok(mir)
    }
}
