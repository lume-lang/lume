use lume_errors::*;

#[derive(Debug, clap::Parser)]
#[command(name = "lsp", about = "Starts the language server", long_about = None)]
pub struct LanguageServerCommand {}

impl LanguageServerCommand {
    #[expect(clippy::unused_self)]
    pub(crate) fn run(&mut self, dcx: DiagCtxHandle) {
        if let Err(err) = lume_lsp::start_server() {
            dcx.emit_and_push(err.into_diagnostic());
        }
    }
}
