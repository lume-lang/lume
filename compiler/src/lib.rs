mod ast;
pub mod lexer;
mod parser;

use std::path::{Path, PathBuf};

use arc::Project;
use ast::TopLevelExpression;
use diag::{Result, source::NamedSource};

use crate::parser::Parser;

#[derive(Debug, PartialEq)]
pub struct Module {
    /// Defines the source file for the module.
    source: NamedSource,

    /// Defines the parsed top-level expressions in the module.
    expressions: Vec<TopLevelExpression>,
}

impl Module {
    /// Creates a new module with the given source file.
    pub fn new(source: NamedSource) -> Self {
        Module {
            source,
            expressions: Vec::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct State {
    /// Defines the source files to compile.
    modules: Vec<Module>,
}

impl State {
    pub fn new() -> Self {
        State { modules: Vec::new() }
    }

    /// Creates a new state from a list of source files.
    pub fn from_files(files: Vec<PathBuf>) -> Result<Self> {
        let mut sources = Vec::new();

        for file in files {
            let source = match NamedSource::from_file(file) {
                Ok(source) => source,
                Err(err) => return Err(err.into()),
            };
            sources.push(source);
        }

        let state = State {
            modules: sources.into_iter().map(Module::new).collect(),
        };

        Ok(state)
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
        let state = State::from_files(project.files()?)?;

        self.build(state)
    }

    /// Builds the given file into an executable or library.
    pub fn build_file(&mut self, mut state: State, entry: NamedSource) -> Result<State> {
        state.modules.push(Module::new(entry));

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
        Parser::parse(state)
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
