pub(crate) mod commands;
pub(crate) mod error;
mod tracing;

use std::env;

use clap::{Arg, ArgAction, Command};
use commands::run;
use error_snippet::{GraphicalRenderer, Result, handler::Handler};

fn run() -> Result<()> {
    let command = Command::new("Lume")
        .about("Lume's toolchain and package manager")
        .version(env!("CARGO_PKG_VERSION"))
        .subcommand_required(true)
        .arg_required_else_help(true)
        .allow_missing_positional(true)
        .disable_version_flag(true)
        .arg(
            Arg::new("trace")
                .long("trace")
                .help("Enables tracing of the compiler")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("version")
                .short('v')
                .long("version")
                .help("Prints the current version of Lume")
                .action(ArgAction::Version),
        )
        .subcommand(run::command());

    let matches = command.get_matches();

    if let Some(true) = matches.get_one("trace") {
        tracing::register_default_tracer();
    }

    match matches.subcommand() {
        Some(("run", sub_matches)) => run::run(sub_matches)?,
        _ => unreachable!(),
    }

    Ok(())
}

fn main() {
    match run() {
        Ok(()) => {}
        Err(err) => {
            let renderer = Box::new(GraphicalRenderer::new());
            let mut handler = error_snippet::handler::DiagnosticHandler::with_renderer(renderer);
            handler.exit_on_error();

            // We're expecting an error here, since the handler will *always* return
            // a [`error_snippet::handler::DrainError::CompoundError`], which we also need to print.
            //
            // We could also get an error of [`std::fmt::Error`], which we should also report.
            if let Err(drain_err) = handler.report_and_drain(err) {
                let compound_diag = error_snippet::SimpleDiagnostic::new(drain_err.to_string());
                let _ = handler.report_and_drain(compound_diag.into());
            }
        }
    }
}
