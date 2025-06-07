pub(crate) mod commands;
pub(crate) mod error;
mod tracing;

use std::env;

use commands::run;
use error::{InvalidCliError, UnknownCommandError};
use error_snippet::{GraphicalRenderer, IntoDiagnostic, Result, handler::Handler};
use getopts::{Options, ParsingStyle};

const USAGE: &str = "Usage: lume [OPTIONS] [COMMAND | FILE]

Commands:

    build    Compile Lume source file(s)

Examples:

    lume build hello.lm    # Compile the file into an executable";

fn print_usage() {
    println!("{USAGE}");

    std::process::exit(0)
}

fn run() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    let mut opts = Options::new();

    opts.parsing_style(ParsingStyle::FloatingFrees);
    opts.optflag("h", "help", "Shows this help screen");
    opts.optflag("v", "version", "Prints the current compiler version");
    opts.optflag("", "trace", "Enables tracing of the compiler");

    let matches = match opts.parse(&args[1..]) {
        Ok(matches) => matches,
        Err(err) => {
            return Err(InvalidCliError {
                inner: vec![err.into_diagnostic()],
            }
            .into());
        }
    };

    if matches.opt_present("h") {
        print_usage();
    }

    if matches.opt_present("v") {
        println!("{}", env!("CARGO_PKG_VERSION"));
        std::process::exit(0)
    }

    if matches.opt_present("trace") {
        tracing::register_default_tracer();
    }

    match matches.free.first().map(String::as_str) {
        Some("run") => run::run(&matches.free[1..]),
        Some(cmd) => Err(UnknownCommandError { command: cmd.into() }.into()),
        None => {
            print_usage();
            Ok(())
        }
    }
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
