pub(crate) mod commands;
pub(crate) mod error;

use std::env;

use commands::run;
use diag::{Result, handler::Handler};
use error::{InvalidCliError, UnknownCommandError};
use getopts::{Options, ParsingStyle};

const USAGE: &str = "Usage: lume [OPTIONS] [COMMAND | FILE]

Commands:

    build    Compile Lume source file(s)

Examples:

    lume build hello.lm    # Compile the file into an executable";

fn print_usage() {
    println!("{}", USAGE);

    std::process::exit(0)
}

fn run() -> Result<i32> {
    let args: Vec<String> = env::args().collect();
    let mut opts = Options::new();

    opts.parsing_style(ParsingStyle::StopAtFirstFree);
    opts.optflag("h", "help", "Shows this help screen");
    opts.optflag("v", "version", "Prints the current compiler version");

    let matches = match opts.parse(&args[1..]) {
        Ok(matches) => matches,
        Err(err) => return Err(InvalidCliError { inner: err }.into()),
    };

    if matches.opt_present("h") {
        print_usage();
    }

    if matches.opt_present("v") {
        println!("{}", env!("CARGO_PKG_VERSION"));
        std::process::exit(0)
    }

    match matches.free.first().map(|s| s.as_str()) {
        Some("build") => run::run(&matches.free[1..]),
        Some(cmd) => Err(UnknownCommandError { command: cmd.into() }.into()),
        None => {
            print_usage();
            Ok(0)
        }
    }
}

fn main() {
    match run() {
        Ok(status) => std::process::exit(status),
        Err(err) => {
            let mut handler = diag::handler::DiagnosticHandler::new();
            handler.exit_on_error();
            handler.report_and_drain(err.into_diag());
        }
    }
}
