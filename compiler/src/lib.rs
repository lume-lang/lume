use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::id::hash_id;
use arc::{Project, ProjectId};
use ast::parser::Parser;
use diag::{Result, source::NamedSource};

pub mod hir;
pub mod id;
pub mod thir;

/// Uniquely identifies a module within a compilation job.
#[derive(serde::Serialize, Hash, Clone, Copy, PartialEq, Eq)]
pub struct ModuleId(pub u64);

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

/// Represents a single project or module with one-or-more Lume source files.
#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Module {
    /// Uniquely identifies the module within a [`State`] instance.
    pub mod_id: ModuleId,

    /// Defines the module's project metadata (such as `Arcfile`s)
    project: Project,

    /// Defines the source files for the module.
    pub files: Vec<ModuleFile>,

    /// Defines the current state of the module.
    pub state: ModuleState,
}

impl Module {
    /// Creates a new module from a [`Project`] instance.
    pub fn from_project(project: Project) -> Result<Self> {
        let mut files = Vec::new();

        for file in project.files()? {
            files.push(ModuleFile::from_path(file)?);
        }

        Ok(Module {
            mod_id: project.id.into(),
            project,
            files,
            state: ModuleState::Read,
        })
    }

    /// Gets the module source file with the given ID.
    pub fn file(&self, file: ModuleFileId) -> Option<&ModuleFile> {
        self.files.iter().find(|&f| f.source_id == file)
    }
}

/// Uniquely identifies a source file within a module.
#[derive(serde::Serialize, Hash, Clone, Copy, PartialEq, Eq)]
pub struct ModuleFileId(pub u64);

impl From<String> for ModuleFileId {
    /// Creates a new [`ModuleFileId`] from a string, by taking it's hash value.
    fn from(value: String) -> ModuleFileId {
        ModuleFileId(hash_id(value.as_bytes()))
    }
}

impl std::fmt::Debug for ModuleFileId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "mod_file({})", self.0)
    }
}

/// Represents a single source file within a module.
#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct ModuleFile {
    /// Uniquely identifies the source file within a module.
    pub source_id: ModuleFileId,

    /// Defines the source file for the module.
    source: NamedSource,
}

impl ModuleFile {
    /// Creates a new module file with the given source file.
    pub fn new(source: NamedSource) -> Self {
        let source_id: ModuleFileId = source.content.clone().into();

        ModuleFile { source_id, source }
    }

    /// Creates a new module file from a single file path.
    pub fn from_path(path: PathBuf) -> Result<Self> {
        let source = NamedSource::from_file(path)?;

        Ok(ModuleFile::new(source))
    }
}

/// Represents the current state of a single module.
#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum ModuleState {
    /// The project Arcfile has been read, deserialized, and has located
    /// all the files to include within the compilation.
    Read,

    /// All the files within the module has been read and parsed into
    /// top-level expressions, which can then be analyzed more
    /// thoroughly and perform other checks and optimizations.
    Parsed { map: hir::Map },
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct State {
    /// Defines the modules to compile.
    modules: Vec<Module>,
}

impl State {
    /// Creates a new [`State`] object, with the given modules.
    fn from_modules(modules: Vec<Module>) -> Self {
        State { modules }
    }

    /// Creates a new state from one-or-more projects.
    pub fn from_projects(projects: Vec<Project>) -> Result<Self> {
        let mut modules = Vec::new();

        for project in projects {
            modules.push(Module::from_project(project)?);
        }

        Ok(State::from_modules(modules))
    }

    /// Creates a new state from a single project.
    pub fn from_project(project: Project) -> Result<Self> {
        Self::from_projects(vec![project])
    }

    /// Gets all the modules within the state.
    pub fn modules(&self) -> Vec<&Module> {
        self.modules.iter().collect::<Vec<_>>()
    }

    /// Gets IDs of all the modules within the state.
    pub fn module_ids(&self) -> Vec<ModuleId> {
        self.modules.iter().map(|m| m.mod_id).collect::<Vec<_>>()
    }

    /// Gets the module with the given ID.
    pub fn module(&self, module: ModuleId) -> Option<&Module> {
        self.modules.iter().find(|&m| m.mod_id == module)
    }

    /// Gets the mutable module with the given ID.
    pub fn module_mut(&mut self, module: ModuleId) -> Option<&mut Module> {
        self.modules.iter_mut().find(|m| m.mod_id == module)
    }

    /// Print a human-readable representation of the state to the console.
    pub fn inspect(&self) {
        println!("{:#?}", self);
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Stage {
    Parse,
    Analyze,
    Codegen,
    Link,
}

pub struct Driver {
    /// Defines the current stage of the compilation process.
    current_stage: Stage,

    /// Defines the current compilation state.
    pub state: State,
}

impl Driver {
    fn new(state: State) -> Self {
        Driver {
            current_stage: Stage::Parse,
            state,
        }
    }

    /// Creates a new compilation driver from the given project root.
    ///
    /// This function will look for Arcfiles within the given root folder, and build the project accordingly.
    /// If no Arcfile is found, an error will be returned. Any other compilation errors will also be returned.
    pub fn from_root(root: &Path) -> Result<Self> {
        let project = Project::locate(root)?;
        let state = State::from_project(project)?;

        Ok(Driver::new(state))
    }

    /// Builds the given project into an executable or library.
    pub fn build_project(root: &Path) -> Result<State> {
        let mut driver = Driver::from_root(root)?;

        driver.build()?;

        Ok(driver.state)
    }

    /// Builds the given compiler state into an executable or library.
    pub fn build(&mut self) -> Result<()> {
        self.run_stage(self.current_stage)?;

        loop {
            let next_stage = match Driver::next_stage(self.current_stage) {
                Some(stage) => stage,
                None => break,
            };

            self.run_stage(next_stage)?;
        }

        Ok(())
    }

    /// Gets the next stage of the compilation process.
    fn next_stage(current: Stage) -> Option<Stage> {
        match current {
            Stage::Parse => Some(Stage::Analyze),
            Stage::Analyze => Some(Stage::Codegen),
            Stage::Codegen => Some(Stage::Link),
            Stage::Link => None,
        }
    }

    /// Executes the given stage of the compilation process.
    fn run_stage(&mut self, stage: Stage) -> Result<()> {
        self.current_stage = stage;

        match stage {
            Stage::Parse => self.parse(),
            Stage::Analyze => self.analyze(),
            Stage::Codegen => self.codegen(),
            Stage::Link => self.link(),
        }
    }

    /// Parses all the modules within the given state object.
    fn parse(&mut self) -> Result<()> {
        let module_ids = self.state.module_ids();

        for module_id in module_ids {
            let module = match self.state.module(module_id) {
                Some(module) => module,
                None => continue,
            };

            // If parse modules in the `Read` state, as we might otherwise
            // re-parse modules, which have already been fully compiled.
            if !matches!(module.state, ModuleState::Read) {
                continue;
            }

            let mut expressions = HashMap::new();

            // Reserve at least the amount of files within module.
            expressions.reserve(module.files.len());

            for module_file in module.files.iter() {
                let file_id = module_file.source_id;
                let source = module_file.source.clone();

                let file_expressions = Parser::new(source).parse()?;

                expressions.insert(file_id, file_expressions);
            }

            // Lowers the parsed module expressions down to HIR.
            let map = hir::lower::HirLowering::lower(&self.state, module_id, expressions)?;

            if let Some(module) = self.state.module_mut(module_id) {
                module.state = ModuleState::Parsed { map };
            }
        }

        Ok(())
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
