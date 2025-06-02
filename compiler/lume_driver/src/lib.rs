use std::path::Path;

use arc::{Package, Project};
use error_snippet::Result;
use lume_errors::{DiagCtx, DiagOutputFormat};
use lume_span::SourceMap;
use lume_typech::ThirBuildCtx;

#[derive(Default)]
pub struct Options {
    /// Defines whether the [`ThirBuildCtx`] should be printed to `stdio`, after
    /// it's been inferred and type checked.
    pub print_type_context: bool,
}

pub struct Driver {
    /// Defines the structure of the Arcfile within the project.
    pub project: Project,

    /// Defines the compilations options to use.
    pub options: Options,
}

impl Driver {
    /// Creates a new compilation driver from the given project root.
    ///
    /// This function will look for Arcfiles within the given root folder, and build the project accordingly.
    /// If no Arcfile is found, an error will be returned. Any other compilation errors will also be returned.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the given path has no `Arcfile` within it.
    pub fn from_root(root: &Path) -> Result<Self> {
        let mut dcx = DiagCtx::new(DiagOutputFormat::Graphical);
        dcx.exit_on_error();

        let project = dcx.with(|handle| Project::locate(root, handle))?;

        Ok(Driver::from_project(project))
    }

    /// Creates a new compilation driver from the given [`Project`].
    pub fn from_project(project: Project) -> Self {
        Driver {
            project,
            options: Options::default(),
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
    pub fn build_project(root: &Path) -> Result<()> {
        let mut driver = Self::from_root(root)?;

        driver.build()
    }

    /// Builds the given compiler state into an executable or library.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - the given path has no `Arcfile` within it,
    /// - an error occured while compiling the project,
    /// - an error occured while linking the project
    /// - or some unexpected error occured which hasn't been handled gracefully.
    pub fn build(&mut self) -> Result<()> {
        for package in self.project.packages_mut() {
            let mut dcx = DiagCtx::new(DiagOutputFormat::Graphical);
            dcx.exit_on_error();

            package.add_std_sources();
            package.add_project_sources()?;

            Compiler::build_package(package, &self.options, dcx)?;
        }

        Ok(())
    }
}

#[allow(unused)]
pub struct Compiler<'a> {
    /// Defines the specific [`Package`] instance to compile.
    package: &'a Package,

    /// Defines the compilations options to use.
    options: &'a Options,

    /// Defines the diagnostics handler context.
    dcx: DiagCtx,

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
    pub fn build_package(package: &'a Package, options: &'a Options, dcx: DiagCtx) -> Result<()> {
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
    fn parse(&mut self) -> Result<lume_hir::map::Map> {
        self.dcx
            .with(|dcx| lume_hir_lower::LowerState::lower(self.package, &mut self.source_map, dcx))
    }

    /// Type checks all the given source files.
    fn type_check(&mut self, hir: lume_hir::map::Map) -> Result<lume_typech::ThirBuildCtx> {
        let mut thir_ctx = self.dcx.with(|handle| lume_typech::ThirBuildCtx::new(hir, handle));

        // Defines the types of all nodes within the HIR maps.
        thir_ctx.define_types()?;

        // Then, make sure they're all valid.
        thir_ctx.typecheck()?;

        Ok(thir_ctx)
    }

    /// Analyzes all the modules within the given state object.
    #[expect(clippy::unnecessary_wraps, reason = "unimplemented")]
    fn analyze(&mut self, thir: &ThirBuildCtx) -> Result<()> {
        if self.options.print_type_context {
            println!("{:#?}", thir.tcx());
        }

        Ok(())
    }

    /// Generates LLVM IR for all the modules within the given state object.
    #[expect(clippy::unnecessary_wraps, clippy::unused_self, reason = "unimplemented")]
    fn codegen(&mut self) -> Result<()> {
        Ok(())
    }

    /// Links all the modules within the given state object into a single executable or library.
    #[expect(clippy::unnecessary_wraps, clippy::unused_self, reason = "unimplemented")]
    fn link(&mut self) -> Result<()> {
        Ok(())
    }
}
