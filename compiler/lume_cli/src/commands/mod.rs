pub(crate) mod arc;
pub(crate) mod build;
pub(crate) mod run;

use clap::ValueHint;

use crate::error::*;
use error_snippet::{IntoDiagnostic, Result};
use lume_session::{Backend, DebugInfo, LinkerPreference, MirPrinting, OptimizationLevel};
use std::env::current_dir;
use std::path::PathBuf;

pub(crate) use arc::ArcCommand;
pub(crate) use build::BuildCommand;
pub(crate) use run::RunCommand;

/// Gets the absolute path of the given project directory, or current working directory if not specified.
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
        default_value = "none",
        default_missing_value = "pretty",
        help = "Print the generated MIR",
        num_args(0..=1),
        require_equals = true
    )]
    pub print_mir: MirPrinting,

    #[arg(long, help = "Print the generated codegen IR")]
    pub print_codegen_ir: bool,

    #[arg(
        long,
        short = 'g',
        help = "Debug info emission in executable",
        default_value_t = DebugInfo::default(),
        default_missing_value = "full",
        value_parser = clap::value_parser!(DebugInfo),
        value_name = "LEVEL",
        num_args(0..=1),
        require_equals = true
    )]
    pub debug_info: DebugInfo,

    #[arg(
        short = 'O',
        long = "optimize",
        default_value = "2",
        help = "Optimization level",
        value_parser = clap::builder::PossibleValuesParser::new(["0", "1", "2", "3", "s", "z"])
    )]
    pub optimize: String,

    #[arg(long, default_value = None, help = "Linker to use when linking objects")]
    pub linker: Option<LinkerPreference>,

    #[arg(long, help = "Path to the runner executable to fuse with", value_name = "LIB", value_hint = ValueHint::FilePath)]
    pub runner_path: Option<PathBuf>,

    #[arg(
        long = "codegen-backend",
        help = "Code generation backend",
        default_value_t = Backend::default(),
        value_parser = clap::value_parser!(Backend)
    )]
    pub backend: Backend,
}

impl BuildOptions {
    pub fn options(&self) -> lume_session::Options {
        lume_session::Options {
            print_type_context: self.print_type_ctx,
            print_mir: self.print_mir,
            print_codegen_ir: self.print_codegen_ir,
            debug_info: self.debug_info,
            optimize: match self.optimize.as_str() {
                "0" => OptimizationLevel::O0,
                "1" => OptimizationLevel::O1,
                "2" => OptimizationLevel::O2,
                "3" => OptimizationLevel::O3,
                "s" => OptimizationLevel::Os,
                "z" => OptimizationLevel::Oz,
                _ => unreachable!(),
            },
            backend: self.backend,
            linker: self.linker,
            runner_path: self.runner_path.clone(),
        }
    }
}
