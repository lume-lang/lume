pub(crate) mod commands;
pub(crate) mod error;
mod tracing;

use std::env;

use clap::{Arg, ArgAction, Command};
use lume_errors::{DiagCtx, DiagOutputFormat};

fn main() {
    use std::io::Write;

    // Initialize logger
    let env = env_logger::Env::default()
        .filter_or("LUMEC_LOG_LEVEL", "warn")
        .write_style_or("LUMEC_LOG_STYLE", "auto");

    env_logger::builder()
        .parse_env(env)
        .format(|buf, rec| {
            let style = buf.default_level_style(rec.level());
            let level = rec.level().to_string().to_lowercase();

            writeln!(buf, "{style}{level}{style:#}: {}", rec.args())
        })
        .init();

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
        .subcommand(commands::arc::command())
        .subcommand(commands::run::command());

    let matches = command.get_matches();

    if let Some(true) = matches.get_one("trace") {
        tracing::register_default_tracer();
    }

    let dcx = DiagCtx::new(DiagOutputFormat::Graphical);

    let _ = dcx.with_res(|dcx| match matches.subcommand() {
        Some(("arc", sub_matches)) => commands::arc::run(sub_matches, dcx),
        Some(("run", sub_matches)) => commands::run::run(sub_matches, dcx),
        _ => unreachable!(),
    });
}
