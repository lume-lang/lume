pub(crate) mod commands;
pub(crate) mod error;

use crate::error::Error;
use std::env;

use commands::run;
use error::CliError;
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

fn run() -> Result<i32, Error> {
    let args: Vec<String> = env::args().collect();
    let mut opts = Options::new();

    opts.parsing_style(ParsingStyle::StopAtFirstFree);
    opts.optflag("h", "help", "Shows this help screen");
    opts.optflag("v", "version", "Prints the current compiler version");

    let matches = match opts.parse(&args[1..]) {
        Ok(matches) => matches,
        Err(err) => return Err(Error::CliError(CliError::ParsingError(err))),
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
        Some(cmd) => Err(Error::CliError(CliError::UnknownCommand(cmd.into()))),
        None => {
            print_usage();
            Ok(0)
        }
    }
}

fn main() {
    match run() {
        Ok(status) => std::process::exit(status),
        Err(err) => err.report(),
    }
}
