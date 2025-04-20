use ::std::path::PathBuf;
use std::Assets;

use arc::Project;
use lume_diag::{Result, source::NamedSource};
use lume_hir::id::{ModuleFileId, ModuleId};
use lume_parser::parser::Parser;

mod std;

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
    pub fn build_project(root: &PathBuf) -> Result<Self> {
        let mut driver = Driver::from_root(root)?;

        driver.build()?;

        Ok(driver)
    }

    /// Creates a new compilation driver from the given project root.
    ///
    /// This function will look for Arcfiles within the given root folder, and build the project accordingly.
    /// If no Arcfile is found, an error will be returned. Any other compilation errors will also be returned.
    pub fn from_root(root: &PathBuf) -> Result<Self> {
        let project = Project::locate(root)?;
        let opts = Options::from_project(project);

        Ok(Driver::new(opts))
    }

    /// Builds the given compiler state into an executable or library.
    pub fn build(&mut self) -> Result<()> {
        let sources = self.parse()?;

        let _thir_ctx = self.type_check(sources)?;

        Ok(())
    }

    /// Gets all the source files to include in the compilation.
    fn source_files(&self) -> Result<Vec<NamedSource>> {
        // Add all the source files within the standard library.
        let mut sources_files = Assets::as_sources()?;

        // As well as all the files within the project itself
        let project_file_names = self.opts.project.files()?;

        sources_files.extend(
            project_file_names
                .into_iter()
                .map(|s| NamedSource::from_file(s))
                .collect::<Result<Vec<_>>>()?,
        );

        Ok(sources_files)
    }

    /// Parses all the modules within the given state object.
    fn parse(&mut self) -> Result<lume_hir::map::Map> {
        let module_id = ModuleId::from(self.opts.project.id);

        // Create a new HIR map for the module.
        let mut hir = lume_hir::map::Map::empty(module_id);

        // Read all the sources files before parsing, so if any of them
        // are inaccessible, we don't waste effort parsing them.
        let source_files = self.source_files()?;

        for named_source in source_files {
            let source_id = ModuleFileId::from(module_id, named_source.name.clone());

            // Parse the contents of the source file.
            let expressions = Parser::new(named_source.clone()).parse()?;

            // Lowers the parsed module expressions down to HIR.
            lume_hir::lower::LowerModule::lower(&mut hir, source_id, &named_source, expressions)?;
        }

        Ok(hir)
    }

    /// Type checks all the given source files.
    fn type_check(&mut self, hir: lume_hir::map::Map) -> Result<lume_types::ThirBuildCtx> {
        let mut thir_ctx = lume_types::ThirBuildCtx::new(hir);

        // Infers the types of all expressions within the HIR maps.
        thir_ctx.infer()?;

        thir_ctx.typecheck()?;

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
