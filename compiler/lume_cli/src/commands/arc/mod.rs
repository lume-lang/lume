pub(crate) mod clean;

use clap::{ArgMatches, Command};
use error_snippet::Result;

pub(crate) fn command() -> Command {
    Command::new("arc")
        .about("Commands for the Arc package manager")
        .subcommand_required(true)
        .arg_required_else_help(true)
        .allow_missing_positional(true)
        .subcommand(clean::command())
}

pub(crate) fn run(matches: &ArgMatches) -> Result<()> {
    match matches.subcommand() {
        Some(("clean", sub_matches)) => clean::run(sub_matches)?,
        _ => unreachable!(),
    }

    Ok(())
}
