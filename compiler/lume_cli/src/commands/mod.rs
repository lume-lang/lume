pub(crate) mod arc;
pub(crate) mod build;
pub(crate) mod run;

use crate::error::*;
use error_snippet::{IntoDiagnostic, Result};
use lume_session::{Backend, LinkerPreference, MirPrinting, OptimizationLevel};
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
    #[arg(help = "Path to the project", value_name = "DIR", value_hint = clap::ValueHint::DirPath)]
    pub path: Option<std::path::PathBuf>,

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
        short = 'O',
        long = "optimize",
        default_value = "2",
        help = "Optimization level",
        value_parser = clap::builder::PossibleValuesParser::new(["0", "1", "2", "3", "s", "z"])
    )]
    pub optimize: String,

    #[arg(long, default_value = None, help = "Linker to use when linking objects")]
    pub linker: Option<LinkerPreference>,

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
        }
    }
}
