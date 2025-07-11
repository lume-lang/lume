#![feature(exclusive_wrapper)]

pub(crate) mod commands;
pub(crate) mod error;

#[cfg(debug_assertions)]
mod tracing;

use std::env;

use clap::Parser;
use lume_errors::{DiagCtx, DiagOutputFormat};

#[derive(Debug, Parser)]
#[clap(
    name = "lume",
    version = env!("CARGO_PKG_VERSION"),
    about = "Lume's toolchain and package manager",
    long_about = None
)]
#[command(
    subcommand_required(true),
    arg_required_else_help(true),
    allow_missing_positional(true)
)]
pub(crate) struct LumeCli {
    #[clap(subcommand)]
    pub subcommand: LumeSubcommands,

    #[arg(long = "trace", help = "Enables tracing of the compiler", global = true)]
    pub trace: bool,

    #[arg(value_enum, long = "tracer", help = "Defines which tracer to use", global = true)]
    pub tracer: Option<tracing::Tracer>,
}

#[derive(Debug, clap::Parser)]
pub enum LumeSubcommands {
    Arc(commands::ArcCommand),
    Run(commands::RunCommand),
}

pub fn lume_cli_entry() {
    let matches = LumeCli::parse();

    #[cfg(debug_assertions)]
    if matches.trace {
        tracing::register_global_tracer(tracing::Tracer::default());
    } else if let Some(val) = matches.tracer {
        tracing::register_global_tracer(val);
    }

    let dcx = DiagCtx::new(DiagOutputFormat::Graphical);

    let _ = dcx.with_res(|dcx| match matches.subcommand {
        LumeSubcommands::Arc(cmd) => cmd.run(dcx),
        LumeSubcommands::Run(cmd) => cmd.run(dcx),
    });
}
