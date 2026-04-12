pub(crate) mod commands;
pub(crate) mod error;
pub(crate) mod opts;

use std::env;
use std::path::PathBuf;

use clap::{ArgAction, Parser};
use lume_errors::DiagCtx;

#[derive(Debug, Parser)]
#[clap(
    name = "lume",
    version = env!("CARGO_PKG_VERSION"),
    about = "Lume's toolchain and package manager",
    long_about = None
)]
#[command(subcommand_required(true), arg_required_else_help(true))]
pub(crate) struct LumeCli {
    #[clap(subcommand)]
    pub subcommand: LumeSubcommands,

    #[clap(flatten)]
    pub dev: LumeDevelopmentCommands,
}

#[derive(Debug, clap::Parser)]
pub enum LumeSubcommands {
    Arc(commands::ArcCommand),
    Check(commands::CheckCommand),
    Build(commands::BuildCommand),
    Format(commands::FormatCommand),
    New(commands::NewCommand),
    Run(commands::RunCommand),

    #[cfg(feature = "lsp")]
    Lsp(commands::LanguageServerCommand),
}

#[derive(Debug, Parser)]
pub(crate) struct LumeDevelopmentCommands {
    /// Panics the compiler when an error is emitted instead of printing it.
    ///
    /// This allows a developer to see a stacktrace of where the error was
    /// emitted from.
    #[arg(long, global = true, help_heading = "Development")]
    #[cfg_attr(not(debug_assertions), arg(hide = true))]
    pub panic_on_error: bool,

    /// Prints the location of where diagnostics are emitted.
    ///
    /// This allows a developer to see where an error is emitted from without
    /// having to look through a stack trace.
    #[arg(long, global = true, help_heading = "Development")]
    #[cfg_attr(not(debug_assertions), arg(hide = true))]
    pub track_diagnostics: bool,

    /// Log file for traces
    #[arg(long, global = true, help_heading = "Development")]
    #[cfg_attr(not(debug_assertions), arg(hide = true))]
    pub log_file: Option<PathBuf>,

    /// Increases verbosity of tracing spans and events
    #[arg(long, short = 'v', global = true, help_heading = "Development", action = ArgAction::Count)]
    #[cfg_attr(not(debug_assertions), arg(hide = true))]
    pub verbose: u8,
}

pub fn lume_cli_entry() {
    let matches = LumeCli::parse();
    let dcx = DiagCtx::new();

    let _guard = lume_tracing::init_subscriber(lume_tracing::Options {
        default_filter: match matches.dev.verbose {
            0 => None,
            1 => Some(tracing::level_filters::LevelFilter::INFO),
            2 => Some(tracing::level_filters::LevelFilter::DEBUG),
            _ => Some(tracing::level_filters::LevelFilter::TRACE),
        },
        log_file: matches.dev.log_file,
    });

    if matches.dev.panic_on_error {
        dcx.panic_on_error();
    }

    if matches.dev.track_diagnostics {
        dcx.track_diagnostics();
    }

    dcx.with_none(|dcx| match matches.subcommand {
        LumeSubcommands::Arc(cmd) => cmd.run(dcx),
        LumeSubcommands::Check(cmd) => cmd.run(dcx),
        LumeSubcommands::Build(cmd) => cmd.run(dcx),
        LumeSubcommands::Format(cmd) => cmd.run(dcx),
        LumeSubcommands::New(cmd) => {
            if let Err(err) = cmd.run() {
                dcx.emit_and_push(err);
            }
        }
        LumeSubcommands::Run(cmd) => cmd.run(dcx),

        #[cfg(feature = "lsp")]
        LumeSubcommands::Lsp(mut cmd) => cmd.run(dcx),
    });

    let mut renderer = error_snippet::GraphicalRenderer::new();
    renderer.use_colors = true;
    renderer.highlight_source = true;

    dcx.render_stderr(&mut renderer);
    dcx.clear();
}
