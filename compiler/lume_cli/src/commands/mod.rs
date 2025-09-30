pub(crate) mod arc;
pub(crate) mod build;
pub(crate) mod run;

use std::env::current_dir;
use std::path::PathBuf;

pub(crate) use arc::ArcCommand;
pub(crate) use build::BuildCommand;
use clap::ValueHint;
use error_snippet::{IntoDiagnostic, Result};
use lume_session::OptimizationLevel;
pub(crate) use run::RunCommand;

use crate::error::*;

/// Gets the absolute path of the given project directory, or current working
/// directory if not specified.
pub fn project_or_cwd(path: Option<&PathBuf>) -> Result<String> {
    let cwd: PathBuf = match current_dir() {
        Ok(cwd) => cwd,
        Err(err) => {
            return Err(CouldNotDetermineBuildPath {
                inner: err.into_diagnostic(),
            }
            .into());
        }
    };

    // If no path is specified, use the current working directory.
    let Some(path) = path else {
        return Ok(cwd.to_string_lossy().into_owned());
    };

    // If the path is absolute, return it as-is.
    if path.is_absolute() {
        return Ok(path.to_string_lossy().into_owned());
    }

    // Otherwise, resolve it from the current working directory.
    let path = match cwd.join(path).canonicalize() {
        Ok(path) => path,
        Err(err) => {
            return Err(CouldNotDetermineBuildPath {
                inner: err.into_diagnostic(),
            }
            .into());
        }
    };

    Ok(path.to_string_lossy().into_owned())
}

#[derive(Debug, clap::Parser)]
pub struct BuildOptions {
    #[arg(help = "Path to the project", value_name = "DIR", value_hint = ValueHint::DirPath)]
    pub path: Option<PathBuf>,

    #[arg(long, help = "Print the type context before analyzing")]
    pub print_type_ctx: bool,

    #[arg(
        long,
        help = "Prints the generated MIR",
        long_help = "Prints the generated MIR for all functions.
Optionally, can supply the name of a pass, where the MIR will be printed before executing.",
        value_name = "PASS",
        value_delimiter = ',',
        required = false,
        num_args = 0..=1
    )]
    pub dump_mir: Option<Vec<String>>,

    #[arg(
        short = 'O',
        long = "optimize",
        default_value = "2",
        help = "Optimization level",
        value_parser = clap::builder::PossibleValuesParser::new(["0", "1", "2", "3", "s", "z"])
    )]
    pub optimize: String,

    #[arg(long, help = "Path to the runner executable to fuse with", value_name = "LIB", value_hint = ValueHint::FilePath)]
    pub runner_path: Option<PathBuf>,
}

impl BuildOptions {
    pub fn options(&self) -> lume_session::Options {
        lume_session::Options {
            print_type_context: self.print_type_ctx,
            dump_mir: self.dump_mir.clone(),
            optimize: match self.optimize.as_str() {
                "0" => OptimizationLevel::O0,
                "1" => OptimizationLevel::O1,
                "2" => OptimizationLevel::O2,
                "3" => OptimizationLevel::O3,
                "s" => OptimizationLevel::Os,
                "z" => OptimizationLevel::Oz,
                _ => unreachable!(),
            },
            runner_path: self.runner_path.clone(),
            source_overrides: None,
        }
    }
}
