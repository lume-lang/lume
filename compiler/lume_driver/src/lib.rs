use std::path::PathBuf;

use arc::Package;
use error_snippet::Result;
use lume_errors::DiagCtxHandle;
use lume_span::SourceMap;
use lume_typech::ThirBuildCtx;

#[derive(Default)]
pub struct Options {
    /// Defines whether the [`ThirBuildCtx`] should be printed to `stdio`, after
    /// it's been inferred and type checked.
    pub print_type_context: bool,
}

pub struct Driver {
    /// Defines the structure of the Arcfile within the package.
    pub package: Package,

    /// Defines the compilations options to use.
    pub options: Options,

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
        let package = dcx.with(|handle| Package::locate(root, handle))?;

        Ok(Driver::from_package(package, dcx))
    }

    /// Creates a new compilation driver from the given [`Package`].
    pub fn from_package(package: Package, dcx: DiagCtxHandle) -> Self {
        Driver {
            package,
            options: Options::default(),
            dcx,
        }
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
    pub fn build_project(root: &PathBuf, dcx: DiagCtxHandle) -> Result<()> {
        let mut driver = Self::from_root(root, dcx)?;

        if let Err(err) = driver.build() {
            driver.dcx.emit(err);
        }

        Ok(())
    }

    /// Builds the given compiler state into an executable or library.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - an error occured while compiling the project,
    /// - an error occured while linking the project
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[tracing::instrument(skip_all, fields(project = %self.package.path.display()), err)]
    pub fn build(&mut self) -> Result<()> {
        let mut package = Package::default();
        let mut dependencies = self.package.dependencies.graph.all();

        // Build all the dependencies of the package in reverse, so all the
        // dependencies without any sub-dependencies can be built first.
        dependencies.reverse();

        for dependency in dependencies {
            dependency.clone_into(&mut package);
            self.build_package(&mut package)?;
        }

        self.package.clone_into(&mut package);
        self.build_package(&mut package)?;

        Ok(())
    }

    /// Builds the given [`Package`] into an executable or library.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - an error occured while compiling the project,
    /// - an error occured while linking the project
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[tracing::instrument(skip_all, fields(project = %self.package.path.display()), err)]
    pub fn build_package(&self, package: &mut Package) -> Result<()> {
        if !package.dependencies.no_std {
            package.add_std_sources();
        }

        package.add_project_sources()?;

        self.dcx
            .with(|dcx| Compiler::build_package(package, &self.options, dcx))
    }
}

#[allow(unused)]
pub struct Compiler<'a> {
    /// Defines the specific [`Package`] instance to compile.
    package: &'a Package,

    /// Defines the compilations options to use.
    options: &'a Options,

    /// Defines the diagnostics handler context.
    dcx: DiagCtxHandle,

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
    pub fn build_package(package: &'a Package, options: &'a Options, dcx: DiagCtxHandle) -> Result<()> {
        let mut compiler = Self {
            package,
            options,
            dcx,
            source_map: SourceMap::default(),
        };

        let sources = compiler.parse()?;
        let thir_ctx = compiler.type_check(sources)?;

        compiler.analyze(&thir_ctx)?;
        compiler.codegen()?;
        compiler.link()?;

        Ok(())
    }

    /// Parses all the source files within the current [`Package`] into HIR.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn parse(&mut self) -> Result<lume_hir::map::Map> {
        self.dcx
            .with(|dcx| lume_hir_lower::LowerState::lower(self.package, &mut self.source_map, dcx))
    }

    /// Type checks all the given source files.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn type_check(&mut self, hir: lume_hir::map::Map) -> Result<lume_typech::ThirBuildCtx> {
        let mut thir_ctx = self.dcx.with_res(|dcx| lume_typech::ThirBuildCtx::new(hir, dcx))?;

        // Defines the types of all nodes within the HIR maps.
        thir_ctx.define_types()?;

        // Then, make sure they're all valid.
        thir_ctx.typecheck()?;

        Ok(thir_ctx)
    }

    /// Analyzes all the modules within the given state object.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn analyze(&mut self, thir: &ThirBuildCtx) -> Result<()> {
        if self.options.print_type_context {
            println!("{:#?}", thir.tdb());
        }

        Ok(())
    }

    /// Generates LLVM IR for all the modules within the given state object.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn codegen(&mut self) -> Result<()> {
        Ok(())
    }

    /// Links all the modules within the given state object into a single executable or library.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn link(&mut self) -> Result<()> {
        Ok(())
    }
}
