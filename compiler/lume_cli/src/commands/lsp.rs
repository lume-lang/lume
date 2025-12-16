use clap::{ArgAction, ValueHint};
use lume_errors::*;

#[derive(Debug, clap::Parser)]
#[command(name = "lsp", about = "Starts the language server", long_about = None)]
pub struct LanguageServerCommand {
    #[arg(long, help = "Writes log output to the given file", value_hint = ValueHint::FilePath)]
    pub log_file: Option<String>,

    #[arg(long, help = "Writes log output to standard output")]
    pub log_stdout: bool,

    #[arg(long, short = 'v', help = "Enables verbose output", action = ArgAction::Count)]
    pub verbose: u8,
}

impl LanguageServerCommand {
    pub(crate) fn run(&mut self, dcx: DiagCtxHandle) {
        let verbosity = match self.verbose {
            0 => lume_lsp::Verbosity::Warning,
            1 => lume_lsp::Verbosity::Info,
            2 => lume_lsp::Verbosity::Debug,
            _ => lume_lsp::Verbosity::Trace,
        };

        // If the user defined `-v` but no logging was defined, assume standard output.
        let log_stdout = if self.verbose > 0 && !self.log_stdout && self.log_file.is_none() {
            true
        } else {
            self.log_stdout
        };

        let options = lume_lsp::Options {
            log_file: self.log_file.take(),
            log_stdout,
            verbosity,
        };

        if let Err(err) = lume_lsp::start_server(options) {
            dcx.emit_and_push(err.into_diagnostic());
        }
    }
}
