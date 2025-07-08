pub(crate) mod clean;

use lume_errors::DiagCtxHandle;

#[derive(Debug, clap::Parser)]
#[command(name = "arc", about = "Commands for the Arc package manager", long_about = None)]
#[command(
    subcommand_required(true),
    arg_required_else_help(true),
    allow_missing_positional(true)
)]
pub struct ArcCommand {
    #[clap(subcommand)]
    pub subcommand: ArcSubcommands,
}

#[derive(Debug, clap::Parser)]
pub enum ArcSubcommands {
    Clean(clean::ArcCleanCommand),
}

impl ArcCommand {
    pub(crate) fn run(&self, dcx: DiagCtxHandle) {
        match &self.subcommand {
            ArcSubcommands::Clean(cmd) => cmd.run(dcx),
        }
    }
}
