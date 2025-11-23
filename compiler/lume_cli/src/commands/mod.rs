pub(crate) mod arc;
pub(crate) mod build;
pub(crate) mod fmt;
pub(crate) mod run;

use std::env::current_dir;
use std::path::PathBuf;

pub(crate) use arc::ArcCommand;
pub(crate) use build::BuildCommand;
use clap::{ValueEnum, ValueHint};
use error_snippet::{IntoDiagnostic, Result};
pub(crate) use fmt::FormatCommand;
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
    /// Path to the project
    #[arg(value_name = "DIR", value_hint = ValueHint::DirPath)]
    pub path: Option<PathBuf>,

    /// Path to the runtime library to link with
    #[arg(long, value_name = "LIB", value_hint = ValueHint::FilePath)]
    pub runtime_path: Option<PathBuf>,

    /// Whether to disable incremental compilation.
    #[arg(long)]
    pub no_incremental: bool,

    /// Optimization level
    #[arg(
        short = 'O',
        long = "optimize",
        default_value = "2",
        value_parser = clap::builder::PossibleValuesParser::new(["0", "1", "2", "3", "s", "z"])
    )]
    pub optimize: String,

    /// Generate source-level debug information
    #[arg(
        long,
        value_enum,
        num_args = 0..=1,
        required = false,
        default_value = "partial",
        default_missing_value = "partial",
    )]
    pub debug_info: DebugInfo,

    /// Which linker to use when linking objects together
    #[arg(long, value_enum)]
    pub linker: Option<Linker>,

    /// Print the type context before analyzing
    #[arg(long, help_heading = "Development")]
    pub print_type_ctx: bool,

    /// Prints the generated MIR
    ///
    /// Optionally, can supply the name of a pass, where the MIR will be printed
    /// before executing.
    #[arg(
        long,
        help_heading = "Development",
        value_name = "PASS",
        value_delimiter = ',',
        required = false,
        num_args = 0..=1,
    )]
    pub dump_mir: Option<Vec<String>>,
}

#[derive(ValueEnum, Debug, Clone, Copy, PartialEq, Eq)]
pub enum DebugInfo {
    /// Produces no debug information at all.
    None,

    /// Include source-level debug information, useful for debugging. This
    /// includes line number tables, symbol names and source file references.
    Partial,

    /// Include extra information for debugging, including a full copy of all
    /// source files.
    Full,
}

#[derive(ValueEnum, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Linker {
    Clang,
    Gcc,
}

impl BuildOptions {
    pub fn options(&self) -> lume_session::Options {
        lume_session::Options {
            print_type_context: self.print_type_ctx,
            dump_mir: self.dump_mir.clone(),
            enable_incremental: !self.no_incremental,
            optimize: match self.optimize.as_str() {
                "0" => OptimizationLevel::O0,
                "1" => OptimizationLevel::O1,
                "2" => OptimizationLevel::O2,
                "3" => OptimizationLevel::O3,
                "s" => OptimizationLevel::Os,
                "z" => OptimizationLevel::Oz,
                _ => unreachable!(),
            },
            debug_info: match self.debug_info {
                DebugInfo::None => lume_session::DebugInfo::None,
                DebugInfo::Partial => lume_session::DebugInfo::Partial,
                DebugInfo::Full => lume_session::DebugInfo::Full,
            },
            linker: match self.linker {
                Some(Linker::Clang) => Some(lume_session::LinkerPreference::Clang),
                Some(Linker::Gcc) => Some(lume_session::LinkerPreference::Gcc),
                None => None,
            },
            runtime_path: self.runtime_path.clone(),
            source_overrides: None,
        }
    }
}
