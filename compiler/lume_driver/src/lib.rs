use std::path::Path;

use arc::Project;
use error_snippet::Result;
use lume_errors::{DiagCtx, DiagOutputFormat};
use lume_state::State;

pub(crate) struct Options {
    /// Defines the structure of the Arcfile within the project.
    pub project: Project,
}

impl Options {
    /// Creates a new set of options from the given root directory.
    pub(crate) fn from_project(project: Project) -> Self {
        Options { project }
    }
}

pub struct Driver {
    /// Defines the current stage of the compilation process.
    pub(crate) opts: Options,

    /// Defines the current build context.
    pub(crate) state: State,
}

#[allow(dead_code)]
impl Driver {
    fn new(opts: Options) -> Self {
        let mut dcx = DiagCtx::new(DiagOutputFormat::Graphical);
        dcx.exit_on_error();

        Driver {
            opts,
            state: State::new(dcx),
        }
    }

    /// Builds the given project into an executable or library.
    ///
    /// This function will look for Arcfiles within the given root folder, and build the project accordingly.
    /// If no Arcfile is found, an error will be returned. Any other compilation errors will also be returned.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - the given path has no `Arcfile` within it,
    /// - an error occured while compiling the project,
    /// - an error occured while linking the project
    /// - or some unexpected error occured which hasn't been handled gracefully.
    pub fn build_project(root: &Path) -> Result<Self> {
        let mut driver = Driver::from_root(root)?;

        driver.build()?;

        Ok(driver)
    }

    /// Creates a new compilation driver from the given project root.
    ///
    /// This function will look for Arcfiles within the given root folder, and build the project accordingly.
    /// If no Arcfile is found, an error will be returned. Any other compilation errors will also be returned.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the given path has no `Arcfile` within it.
    pub fn from_root(root: &Path) -> Result<Self> {
        let project = Project::locate(root)?;
        let opts = Options::from_project(project);

        Ok(Driver::new(opts))
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
        let sources = self.parse()?;
        let _thir_ctx = self.type_check(sources)?;

        Ok(())
    }

    /// Parses all the modules within the given state object.
    fn parse(&mut self) -> Result<lume_hir::map::Map> {
        lume_hir::lower::LowerState::lower(&self.opts.project, &mut self.state)
    }

    /// Type checks all the given source files.
    fn type_check(&mut self, mut hir: lume_hir::map::Map) -> Result<lume_typech::ThirBuildCtx<'_>> {
        let mut thir_ctx = lume_typech::ThirBuildCtx::new(&mut self.state);

        // Defines the types of all nodes within the HIR maps.
        thir_ctx.define_types(&mut hir)?;

        // Then, make sure they're all valid.
        thir_ctx.typecheck(&hir)?;

        println!("{:#?}", thir_ctx.tcx());

        Ok(thir_ctx)
    }

    /// Analyzes all the modules within the given state object.
    #[expect(clippy::unnecessary_wraps, clippy::unused_self, reason = "unimplemented")]
    fn analyze(&mut self) -> Result<()> {
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
