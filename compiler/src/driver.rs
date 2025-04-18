use std::{collections::HashMap, path::PathBuf};

use crate::{hir, id::hash_id, std::Assets, thir};
use arc::{Project, ProjectId};
use ast::{
    ast::{Import, TopLevelExpression},
    parser::Parser,
};
use diag::{Result, source::NamedSource};

/// Defines the default imports from the `std` module.
const DEFAULT_STD_IMPORTS: &[&'static str] = &[
    "Boolean", "Float", "Double", "Int8", "UInt8", "Int16", "UInt16", "Int32", "UInt32", "Int64", "UInt64", "Pointer",
];

/// Uniquely identifies a module within a compilation job.
#[derive(serde::Serialize, Hash, Clone, Copy, PartialEq, Eq)]
pub struct ModuleId(pub u64);

impl ModuleId {
    pub fn empty() -> Self {
        Self(0)
    }
}

impl From<ProjectId> for ModuleId {
    fn from(value: ProjectId) -> Self {
        ModuleId(value.0)
    }
}

impl std::fmt::Debug for ModuleId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "mod({})", self.0)
    }
}

/// Uniquely identifies a source file within a module.
#[derive(serde::Serialize, Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct ModuleFileId(pub ModuleId, pub u64);

impl ModuleFileId {
    pub fn empty() -> Self {
        Self(ModuleId::empty(), 0)
    }

    /// Creates a new [`ModuleFileId`] from a string, by taking it's hash value.
    pub fn from(module: ModuleId, value: String) -> ModuleFileId {
        ModuleFileId(module, hash_id(value.as_bytes()))
    }
}

pub(crate) struct ModuleFile {
    /// Defines the name and content of the source file.
    pub(crate) source: NamedSource,

    /// Defines the parsed HIR (High-Level Intermediate Representation) within the source file.
    pub(crate) hir: hir::Map,
}

impl ModuleFile {
    /// Creates a new source file from the given named source and parsed HIR.
    pub fn new(source: NamedSource, hir: hir::Map) -> Self {
        ModuleFile { source, hir }
    }
}

/// Defines a collection of source files.
pub(crate) struct Sources {
    /// Defines the unique identifier of the module containing the source files.
    pub id: ModuleId,

    /// Defines all the files within the module.
    pub files: HashMap<ModuleFileId, ModuleFile>,
}

impl Sources {
    /// Creates a new empty source collection.
    pub fn new(id: ModuleId) -> Self {
        Sources {
            id,
            files: HashMap::new(),
        }
    }
}

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
        // Apppend all the files within the project itself
        let project_file_names = self.opts.project.files()?;

        let mut sources_files = project_file_names
            .into_iter()
            .map(|s| NamedSource::from_file(s))
            .collect::<Result<Vec<_>>>()?;

        // As well as all the source files within the standard library
        sources_files.extend(Assets::as_sources()?);

        Ok(sources_files)
    }

    /// Imports the standard library into the current file, so it's always available.
    fn std_imports(&mut self) -> Vec<TopLevelExpression> {
        let mut imports = Vec::new();

        imports.push(Import::from_names(&["std"], DEFAULT_STD_IMPORTS));

        imports
            .into_iter()
            .map(|import| TopLevelExpression::Import(Box::new(import)))
            .collect()
    }

    /// Parses all the modules within the given state object.
    fn parse(&mut self) -> Result<Sources> {
        let module_id = ModuleId::from(self.opts.project.id);

        // Read all the sources files before parsing, so if any of them
        // are inaccessible, we don't waste effort parsing them.
        let source_files = self.source_files()?;

        let mut sources = Sources::new(module_id);

        for named_source in source_files {
            let source_id = ModuleFileId::from(module_id, named_source.name.clone());

            // Parse the contents of the source file.
            let expressions = Parser::new(named_source.clone()).parse()?;

            // Lowers the parsed module expressions down to HIR.
            let map = hir::lower::LowerModuleFile::lower(source_id, &named_source, expressions)?;

            sources.files.insert(source_id, ModuleFile::new(named_source, map));
        }

        Ok(sources)
    }

    /// Type checks all the given source files.
    fn type_check(&mut self, sources: Sources) -> Result<thir::ThirBuildCtx> {
        let mut thir_ctx = thir::ThirBuildCtx::from_sources(sources);

        thir_ctx.evaluate()?;

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
