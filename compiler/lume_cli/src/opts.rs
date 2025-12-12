use std::path::PathBuf;

use clap::{ValueEnum, ValueHint};
use lume_session::OptimizationLevel;

#[derive(Debug, clap::Parser)]
pub struct BuildOptions {
    /// Path to the project
    #[arg(value_name = "DIR", value_hint = ValueHint::DirPath)]
    pub path: Option<PathBuf>,

    /// Whether to disable incremental compilation.
    #[arg(long)]
    pub no_incremental: bool,

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

    /// Optimization level
    #[arg(
        short = 'O',
        long = "optimize",
        default_value = "2",
        value_parser = clap::builder::PossibleValuesParser::new(["0", "1", "2", "3", "s", "z"])
    )]
    pub optimize: String,

    #[clap(flatten, next_help_heading = "Codegen")]
    pub codegen: CodegenOptions,

    #[cfg(debug_assertions)]
    #[clap(flatten, next_help_heading = "Development")]
    pub dev: DevelopmentBuildOptions,
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

#[derive(Debug, clap::Parser)]
pub struct CodegenOptions {
    /// Path to the runtime library to link with
    #[arg(long, value_name = "LIB", value_hint = ValueHint::FilePath)]
    pub runtime_path: Option<PathBuf>,

    /// Which linker to use when linking objects together
    #[arg(long, value_enum)]
    pub linker: Option<Linker>,
}

#[cfg(debug_assertions)]
#[derive(Debug, clap::Parser)]
pub struct DevelopmentBuildOptions {
    /// Print the type context before analyzing.
    #[arg(long)]
    pub print_type_ctx: bool,

    /// Prints the generated MIR
    ///
    /// Optionally, can supply the name of a pass, where the MIR will be printed
    /// before executing.
    #[arg(
        long,
        value_name = "PASS",
        value_delimiter = ',',
        required = false,
        num_args = 0..=1
    )]
    pub dump_mir: Option<Vec<String>>,

    /// Filters the dumped MIR functions
    ///
    /// Comma-separated list of the unmangled function names to dump.
    #[arg(long, value_name = "FUNC", value_delimiter = ',')]
    pub dump_mir_func: Vec<String>,

    /// Dumps the generated codegen IR
    #[arg(long)]
    pub dump_codegen_ir: bool,
}

impl BuildOptions {
    pub fn options(&self) -> lume_session::Options {
        lume_session::Options {
            print_type_context: self.dev.print_type_ctx,
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
            linker: match self.codegen.linker {
                Some(Linker::Clang) => Some(lume_session::LinkerPreference::Clang),
                Some(Linker::Gcc) => Some(lume_session::LinkerPreference::Gcc),
                None => None,
            },
            runtime_path: self.codegen.runtime_path.clone(),
            dump_mir: self.dev.dump_mir.clone(),
            dump_mir_func: self.dev.dump_mir_func.clone(),
            dump_codegen_ir: self.dev.dump_codegen_ir,
            source_overrides: None,
        }
    }
}
