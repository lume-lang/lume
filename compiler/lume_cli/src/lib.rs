pub(crate) mod commands;
pub(crate) mod error;

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
}

#[derive(Debug, clap::Parser)]
pub enum LumeSubcommands {
    Arc(commands::ArcCommand),
    Build(commands::BuildCommand),
    Format(commands::FormatCommand),
    Run(commands::RunCommand),
}

pub fn lume_cli_entry() {
    let matches = LumeCli::parse();

    lume_trace::init();

    let dcx = DiagCtx::new();

    dcx.with_none(|dcx| match matches.subcommand {
        LumeSubcommands::Arc(cmd) => cmd.run(dcx),
        LumeSubcommands::Build(cmd) => cmd.run(dcx),
        LumeSubcommands::Format(cmd) => cmd.run(dcx),
        LumeSubcommands::Run(cmd) => cmd.run(dcx),
    });

    let mut renderer = error_snippet::GraphicalRenderer::new();
    renderer.use_colors = true;
    renderer.highlight_source = true;

    dcx.render_stderr(&mut renderer);
    dcx.clear();
}
