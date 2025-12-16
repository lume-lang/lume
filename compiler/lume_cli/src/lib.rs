pub(crate) mod commands;
pub(crate) mod error;
pub(crate) mod opts;

use std::env;

use clap::Parser;
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

    #[cfg(debug_assertions)]
    #[clap(flatten)]
    pub dev: LumeDevelopmentCommands,
}

#[derive(Debug, clap::Parser)]
pub enum LumeSubcommands {
    Arc(commands::ArcCommand),
    Build(commands::BuildCommand),
    Format(commands::FormatCommand),
    Run(commands::RunCommand),

    #[cfg(feature = "lsp")]
    Lsp(commands::LanguageServerCommand),
}

#[cfg(debug_assertions)]
#[derive(Debug, Parser)]
pub(crate) struct LumeDevelopmentCommands {
    /// Panics the compiler when an error is emitted instead of printing it.
    ///
    /// This allows a developer to see a stacktrace of where the error was
    /// emitted from.
    #[arg(long, global = true, help_heading = "Development")]
    pub panic_on_error: bool,

    /// Prints the location of where diagnostics are emitted.
    ///
    /// This allows a developer to see where an error is emitted from without
    /// having to look through a stack trace.
    #[arg(long, global = true, help_heading = "Development")]
    pub track_diagnostics: bool,
}

pub fn lume_cli_entry() {
    #[cfg(feature = "tracing")]
    libftrace::set_filter(
        libftrace::filter::from_env("LUMEC_LOG")
            .or_else(|err| {
                eprintln!("error: could not parse trace filter: {err:?}");
                libftrace::filter::parse("info")
            })
            .unwrap(),
    );

    let matches = LumeCli::parse();
    let dcx = DiagCtx::new();

    #[cfg(debug_assertions)]
    if matches.dev.panic_on_error {
        dcx.panic_on_error();
    }

    #[cfg(debug_assertions)]
    if matches.dev.track_diagnostics {
        dcx.track_diagnostics();
    }

    dcx.with_none(|dcx| match matches.subcommand {
        LumeSubcommands::Arc(cmd) => cmd.run(dcx),
        LumeSubcommands::Build(cmd) => cmd.run(dcx),
        LumeSubcommands::Format(cmd) => cmd.run(dcx),
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
