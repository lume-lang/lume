use std::{collections::HashMap, path::PathBuf, sync::Arc};

use arc::locate_package;
use error_snippet::Result;
use lume_errors::DiagCtxHandle;
use lume_infer::TyInferCtx;
use lume_session::{GlobalCtx, Options, Package, Session};
use lume_span::{PackageId, SourceMap};
use lume_tir::TypedIR;
use lume_typech::TyCheckCtx;
use lume_types::TyCtx;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledExecutable {
    pub binary: PathBuf,
}

pub struct Driver {
    /// Defines the structure of the Arcfile within the package.
    pub package: Package,

    /// Defines the diagnostics context for reporting errors during compilation.
    dcx: DiagCtxHandle,
}

impl Driver {
    /// Creates a new compilation driver from the given package root.
    ///
    /// This function will look for Arcfiles within the given root folder, and build the package accordingly.
    /// If no Arcfile is found, an error will be returned. Any other compilation errors will also be returned.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the given path has no `Arcfile` within it.
    pub fn from_root(root: &PathBuf, dcx: DiagCtxHandle) -> Result<Self> {
        let mut package = dcx.with(|handle| locate_package(root, handle))?;

        package.add_package_sources_recursive()?;

        Ok(Driver::from_package(package, dcx))
    }

    /// Creates a new compilation driver from the given [`Package`].
    pub fn from_package(package: Package, dcx: DiagCtxHandle) -> Self {
        Driver { package, dcx }
    }

    /// Locates the [`Package`] from the given path and builds it into an executable or library.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - the given path has no `Arcfile` within it,
    /// - an error occured while compiling the package,
    /// - an error occured while linking the package
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[cfg(feature = "codegen")]
    #[allow(clippy::needless_pass_by_value)]
    pub fn build_package(root: &PathBuf, opts: Options, dcx: DiagCtxHandle) -> Result<CompiledExecutable> {
        let driver = Self::from_root(root, dcx.clone())?;

        driver.build(opts)
    }

    /// Locates the [`Package`] from the given path and checks it for errors.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - the given path has no `Arcfile` within it,
    /// - an error occured while compiling the package,
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[allow(clippy::needless_pass_by_value)]
    pub fn check_package(root: &PathBuf, opts: Options, dcx: DiagCtxHandle) -> Result<()> {
        let driver = Self::from_root(root, dcx.clone())?;

        if let Err(err) = driver.check(opts) {
            dcx.emit_and_push(err);
        }

        Ok(())
    }

    /// Builds the given compiler state into an executable or library.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - an error occured while compiling the package,
    /// - an error occured while linking the package
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[cfg(feature = "codegen")]
    #[tracing::instrument(skip_all, fields(root = %self.package.path.display()), err)]
    pub fn build(mut self, options: Options) -> Result<CompiledExecutable> {
        let session = Session {
            dep_graph: std::mem::take(&mut self.package.dependencies.graph),
            workspace_root: self.package.path.clone(),
            options,
        };

        let gcx = Arc::new(GlobalCtx::new(session, self.dcx.to_context()));
        let mut dependencies = gcx.session.dep_graph.all();

        // Build all the dependencies of the package in reverse, so all the
        // dependencies without any sub-dependencies can be built first.
        dependencies.reverse();

        let mut codegen_mods = lume_codegen::CodegenResult::default();
        codegen_mods.modules.reserve_exact(dependencies.len());

        for dependency in dependencies {
            let module = Compiler::build_package(dependency, gcx.clone())?;
            codegen_mods.modules.push(module);
        }

        let objects = Self::write_object_files(&gcx, codegen_mods)?;
        let output_file_path = gcx.binary_output_path(&self.package.name);

        lume_linker::link_objects(&objects, &output_file_path, &gcx.session.options)?;

        Ok(CompiledExecutable {
            binary: output_file_path,
        })
    }

    /// Checks the packages within the current compiler state for errors.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - an error occured while compiling the package,
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[tracing::instrument(skip_all, fields(root = %self.package.path.display()), err)]
    pub fn check(mut self, options: Options) -> Result<CheckedPackageGraph> {
        let session = Session {
            dep_graph: std::mem::take(&mut self.package.dependencies.graph),
            workspace_root: self.package.path.clone(),
            options,
        };

        let gcx = Arc::new(GlobalCtx::new(session, self.dcx.to_context()));
        let mut dependencies = gcx.session.dep_graph.all();

        // Build all the dependencies of the package in reverse, so all the
        // dependencies without any sub-dependencies can be built first.
        dependencies.reverse();

        let mut graph = CheckedPackageGraph::default();

        for dependency in dependencies {
            let checked = Compiler::check_package(dependency, gcx.clone())?;

            graph.packages.insert(checked.package, checked);
        }

        Ok(graph)
    }

    /// Writes the object files of the given codegen result to disk in
    /// the current workspace directory.
    #[cfg(feature = "codegen")]
    fn write_object_files(
        gcx: &Arc<GlobalCtx>,
        result: lume_codegen::CodegenResult,
    ) -> Result<lume_codegen::CodegenObjects> {
        use error_snippet::IntoDiagnostic;
        use rayon::iter::{IntoParallelIterator, ParallelIterator};

        std::fs::create_dir_all(gcx.obj_path()).map_err(IntoDiagnostic::into_diagnostic)?;
        std::fs::create_dir_all(gcx.obj_bc_path()).map_err(IntoDiagnostic::into_diagnostic)?;

        let objects = result
            .modules
            .into_par_iter()
            .map(|module| {
                use std::io::Write;

                let output_file_path = gcx.obj_bc_path().join(format!("{}.o", module.name));
                let mut output_file =
                    std::fs::File::create(&output_file_path).map_err(IntoDiagnostic::into_diagnostic)?;

                output_file
                    .write_all(&module.bytecode)
                    .map_err(IntoDiagnostic::into_diagnostic)?;

                Ok(lume_codegen::CodegenObject {
                    name: module.name,
                    path: output_file_path,
                })
            })
            .collect::<Result<Vec<lume_codegen::CodegenObject>>>()?;

        Ok(lume_codegen::CodegenObjects { objects })
    }
}

pub struct Compiler<'a> {
    /// Defines the specific [`Package`] instance to compile.
    package: &'a Package,

    /// Defines the global compilation context.
    gcx: Arc<GlobalCtx>,

    /// Defines the global source map for all source files.
    source_map: SourceMap,
}

impl<'a> Compiler<'a> {
    /// Builds the [`Package`] with the given ID from the [`Project`] into an executable or library.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - an error occured while compiling the project,
    /// - an error occured while linking the project
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[cfg(feature = "codegen")]
    #[tracing::instrument(skip_all, fields(package = %package.name), err)]
    pub fn build_package(package: &'a Package, gcx: Arc<GlobalCtx>) -> Result<lume_codegen::CompiledModule> {
        let mut compiler = Self {
            package,
            gcx,
            source_map: SourceMap::default(),
        };

        let sources = compiler.parse()?;
        tracing::debug!(target: "driver", "finished parsing");

        let (tcx, typed_ir) = compiler.type_check(sources)?;

        compiler.codegen(&tcx, typed_ir)
    }

    /// Checks the given [`Package`] for errors, such as parsing-, semantic- or configuration errors.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - an error occured while compiling the project,
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[tracing::instrument(skip_all, fields(package = %package.name), err)]
    pub fn check_package(package: &'a Package, gcx: Arc<GlobalCtx>) -> Result<CheckedPackage> {
        let mut compiler = Self {
            package,
            gcx,
            source_map: SourceMap::default(),
        };

        let sources = compiler.parse()?;
        tracing::debug!(target: "driver", "finished parsing");

        let (tcx, _) = compiler.type_check(sources)?;

        Ok(CheckedPackage {
            package: package.id,
            tcx,
        })
    }

    /// Parses all the source files within the current [`Package`] into HIR.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn parse(&mut self) -> Result<lume_hir::map::Map> {
        self.gcx
            .dcx
            .with(|dcx| lume_hir_lower::LowerState::lower(self.package, &mut self.source_map, dcx))
    }

    /// Type checks all the given source files.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn type_check(&mut self, hir: lume_hir::map::Map) -> Result<(TyCheckCtx, TypedIR)> {
        let tcx = TyCtx::new(self.gcx.clone());

        // Defines the types of all nodes within the HIR maps.
        let mut ticx = TyInferCtx::new(tcx, hir);
        tracing::info_span!("type inference").in_scope(|| ticx.infer())?;

        // Then, make sure they're all valid.
        let mut tccx = TyCheckCtx::new(ticx);
        tracing::info_span!("type checking").in_scope(|| tccx.typecheck())?;

        if self.gcx.session.options.print_type_context {
            println!("{:#?}", tccx.tdb());
        }

        let typed_ir = tracing::info_span!("tir lowering").in_scope(|| lume_tir_lower::Lower::build(&tccx))?;

        Ok((tccx, typed_ir))
    }

    /// Generates LLVM IR for all the modules within the given state object.
    #[cfg(feature = "codegen")]
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn codegen(&mut self, tcx: &TyCheckCtx, tir: TypedIR) -> Result<lume_codegen::CompiledModule> {
        let mir =
            tracing::info_span!("mir lowering").in_scope(|| lume_mir_lower::ModuleTransformer::transform(tcx, tir));

        match self.gcx.session.options.print_mir {
            lume_session::MirPrinting::None => {}
            lume_session::MirPrinting::Pretty => println!("{mir}"),
            lume_session::MirPrinting::Debug => println!("{mir:#?}"),
        }

        tracing::info_span!("codegen")
            .in_scope(|| lume_codegen::Generator::codegen(self.package, mir, &self.gcx.session.options))
    }

    /// Links all the modules within the given state object into a single executable or library.
    #[cfg(feature = "codegen")]
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn link(&mut self) -> Result<()> {
        Ok(())
    }
}

#[derive(Default)]
pub struct CheckedPackageGraph {
    /// Defines map of all checked packages, keyed by their ID.
    pub packages: HashMap<PackageId, CheckedPackage>,
}

pub struct CheckedPackage {
    /// Defines the ID of the checked [`Package`].
    pub package: PackageId,

    /// Defines the checked type context.
    pub tcx: TyCheckCtx,
}
