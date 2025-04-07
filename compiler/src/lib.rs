use std::path::{Path, PathBuf};

use arc::Project;
use ast::ast::TopLevelExpression;
use ast::parser::Parser;
use diag::{Result, source::NamedSource};

/// Represents a single project or module with one-or-more Lume source files.
#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Module {
    /// Defines the module's project metadata (such as `Arcfile`s)
    project: Project,

    /// Defines the source files for the module.
    files: Vec<ModuleFile>,
}

impl Module {
    /// Creates a new module from a [`Project`] instance.
    pub fn from_project(project: Project) -> Result<Self> {
        let mut files = Vec::new();

        for file in project.files()? {
            files.push(ModuleFile::from_path(file)?);
        }

        Ok(Module { project, files })
    }
}

/// Represents a single source file within a module.
#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct ModuleFile {
    /// Defines the source file for the module.
    source: NamedSource,

    /// Defines the parsed top-level expressions in the module.
    expressions: Vec<TopLevelExpression>,
}

impl ModuleFile {
    /// Creates a new module file with the given source file.
    pub fn new(source: NamedSource) -> Self {
        ModuleFile {
            source,
            expressions: Vec::new(),
        }
    }

    /// Creates a new module file from a single file path.
    pub fn from_path(path: PathBuf) -> Result<Self> {
        let source = NamedSource::from_file(path)?;

        Ok(ModuleFile::new(source))
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct State {
    /// Defines the modules to compile.
    modules: Vec<Module>,
}

impl State {
    pub fn new() -> Self {
        State { modules: Vec::new() }
    }

    /// Creates a new state from one-or-more projects.
    pub fn from_projects(projects: Vec<Project>) -> Result<Self> {
        let mut modules = Vec::new();

        for project in projects {
            modules.push(Module::from_project(project)?);
        }

        Ok(State { modules })
    }

    /// Creates a new state from a single project.
    pub fn from_project(project: Project) -> Result<Self> {
        Self::from_projects(vec![project])
    }

    /// Print a human-readable representation of the state to the console.
    pub fn inspect(&self) {
        let config = ron::ser::PrettyConfig::default();

        let repr = match ron::ser::to_string_pretty(self, config) {
            Ok(r) => r,
            Err(err) => {
                eprintln!("Failed to serialize state object: {:?}", err);
                return;
            }
        };

        println!("{}", repr);
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
}

impl Driver {
    pub fn new() -> Self {
        Driver {
            current_stage: Stage::Parse,
        }
    }

    /// Builds the given project into an executable or library.
    ///
    /// This function will look for Arcfiles within the given root folder, and build the project accordingly.
    /// If no Arcfile is found, an error will be returned. Any other compilation errors will also be returned.
    pub fn build_project(&mut self, root: &Path) -> Result<State> {
        let project = Project::locate(root)?;
        let state = State::from_project(project)?;

        self.build(state)
    }

    /// Builds the given compiler state into an executable or library.
    pub fn build(&mut self, mut state: State) -> Result<State> {
        self.run_stage(&mut state, self.current_stage)?;

        loop {
            let next_stage = match Driver::next_stage(self.current_stage) {
                Some(stage) => stage,
                None => break,
            };

            self.run_stage(&mut state, next_stage)?;
        }

        Ok(state)
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
    fn run_stage(&mut self, state: &mut State, stage: Stage) -> Result<()> {
        self.current_stage = stage;

        match stage {
            Stage::Parse => self.parse(state),
            Stage::Analyze => self.analyze(state),
            Stage::Codegen => self.codegen(state),
            Stage::Link => self.link(state),
        }
    }

    /// Parses all the modules within the given state object.
    fn parse(&mut self, state: &mut State) -> Result<()> {
        for module in state.modules.iter_mut() {
            for module_file in module.files.iter_mut() {
                let source = module_file.source.clone();
                module_file.expressions = Parser::new(source).parse()?;
            }
        }

        Ok(())
    }

    /// Analyzes all the modules within the given state object.
    fn analyze(&mut self, _state: &mut State) -> Result<()> {
        Ok(())
    }

    /// Generates LLVM IR for all the modules within the given state object.
    fn codegen(&mut self, _state: &mut State) -> Result<()> {
        Ok(())
    }

    /// Links all the modules within the given state object into a single executable or library.
    fn link(&mut self, _state: &mut State) -> Result<()> {
        Ok(())
    }
}
