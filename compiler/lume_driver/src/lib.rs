use std::{path::PathBuf, sync::Arc};

use arc::locate_package;
use error_snippet::Result;
use lume_errors::{DiagCtx, DiagCtxHandle};
use lume_infer::TyInferCtx;
use lume_session::{GlobalCtx, Options, Package, Session};
use lume_span::SourceMap;
use lume_typech::TyCheckCtx;
use lume_types::TyCtx;

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

    /// Locates the [`Project`] from the given path and builds it into an executable or library.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - the given path has no `Arcfile` within it,
    /// - an error occured while compiling the project,
    /// - an error occured while linking the project
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[allow(clippy::needless_pass_by_value)]
    pub fn build_project(root: &PathBuf, opts: Options, dcx: DiagCtxHandle) -> Result<()> {
        let driver = Self::from_root(root, dcx.clone())?;

        if let Err(err) = driver.build(opts) {
            dcx.emit(err);
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
    #[tracing::instrument(skip_all, fields(root = %self.package.path.display()), err)]
    pub fn build(mut self, options: Options) -> Result<()> {
        let session = Session {
            dep_graph: std::mem::take(&mut self.package.dependencies.graph),
            options,
        };

        let gcx = Arc::new(GlobalCtx::new(session, self.dcx.to_context()));
        let mut dependencies = gcx.session.dep_graph.all();

        // Build all the dependencies of the package in reverse, so all the
        // dependencies without any sub-dependencies can be built first.
        dependencies.reverse();

        for dependency in dependencies {
            Compiler::build_package(dependency, gcx.clone())?;
        }

        Ok(())
    }
}

#[allow(unused)]
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
    #[tracing::instrument(skip_all, fields(package = %package.name), err)]
    pub fn build_package(package: &'a Package, gcx: Arc<GlobalCtx>) -> Result<()> {
        let mut compiler = Self {
            package,
            gcx,
            source_map: SourceMap::default(),
        };

        let sources = compiler.parse()?;
        let thir_ctx = compiler.type_check(sources)?;

        compiler.codegen(thir_ctx)?;
        compiler.link()?;

        Ok(())
    }

    /// Returns the diagnostics context.
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn dcx(&self) -> DiagCtx {
        self.gcx.dcx.clone()
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
    fn type_check(&mut self, hir: lume_hir::map::Map) -> Result<TyCheckCtx> {
        let tcx = TyCtx::new(self.gcx.clone());

        // Defines the types of all nodes within the HIR maps.
        let mut ticx = TyInferCtx::new(tcx, hir);
        ticx.infer()?;

        // Then, make sure they're all valid.
        let mut tccx = TyCheckCtx::new(ticx);
        tccx.typecheck()?;

        if self.gcx.session.options.print_type_context {
            println!("{:#?}", tccx.tdb());
        }

        Ok(tccx)
    }

    /// Generates LLVM IR for all the modules within the given state object.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn codegen(&mut self, thir: TyCheckCtx) -> Result<()> {
        let mir = lume_mir_lower::ModuleTransformer::transform(&thir);

        if self.gcx.session.options.print_mir {
            println!("{mir}");
        }

        lume_codegen::Generator::codegen(self.package, mir, &self.gcx.session.options);

        Ok(())
    }

    /// Links all the modules within the given state object into a single executable or library.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn link(&mut self) -> Result<()> {
        Ok(())
    }
}
