use std::path::Path;

use arc::Project;
use lume_diag::Result;

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
}

#[allow(dead_code)]
impl Driver {
    fn new(opts: Options) -> Self {
        Driver { opts }
    }

    /// Builds the given project into an executable or library.
    ///
    /// This function will look for Arcfiles within the given root folder, and build the project accordingly.
    /// If no Arcfile is found, an error will be returned. Any other compilation errors will also be returned.
    pub fn build_project(root: &Path) -> Result<Self> {
        let mut driver = Driver::from_root(root)?;

        driver.build()?;

        Ok(driver)
    }

    /// Creates a new compilation driver from the given project root.
    ///
    /// This function will look for Arcfiles within the given root folder, and build the project accordingly.
    /// If no Arcfile is found, an error will be returned. Any other compilation errors will also be returned.
    pub fn from_root(root: &Path) -> Result<Self> {
        let project = Project::locate(root)?;
        let opts = Options::from_project(project);

        Ok(Driver::new(opts))
    }

    /// Builds the given compiler state into an executable or library.
    pub fn build(&mut self) -> Result<()> {
        let mut state = lume_state::State::default();

        let sources = self.parse(&mut state)?;

        let _thir_ctx = self.type_check(&mut state, sources)?;

        Ok(())
    }

    /// Parses all the modules within the given state object.
    fn parse(&mut self, state: &mut lume_state::State) -> Result<lume_hir::map::Map> {
        lume_hir::lower::LowerState::lower(&self.opts.project, state)
    }

    /// Type checks all the given source files.
    fn type_check<'a>(
        &mut self,
        state: &'a mut lume_state::State,
        mut hir: lume_hir::map::Map,
    ) -> Result<lume_typech::ThirBuildCtx<'a>> {
        let mut thir_ctx = lume_typech::ThirBuildCtx::new(state);

        // Defines the types of all nodes within the HIR maps.
        thir_ctx.define_types(&mut hir)?;

        // Then, make sure they're all valid.
        thir_ctx.typecheck(&hir)?;

        println!("{:#?}", thir_ctx);

        Ok(thir_ctx)
    }

    /// Analyzes all the modules within the given state object.
    fn analyze(&mut self) -> Result<()> {
        Ok(())
    }

    /// Generates LLVM IR for all the modules within the given state object.
    fn codegen(&mut self) -> Result<()> {
        Ok(())
    }

    /// Links all the modules within the given state object into a single executable or library.
    fn link(&mut self) -> Result<()> {
        Ok(())
    }
}
