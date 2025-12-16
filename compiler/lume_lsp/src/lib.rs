use std::error::Error;
use std::str::FromStr;

use lsp_server::Connection;
use lsp_types::*;
use lume_errors::{Result, SimpleDiagnostic};

use crate::state::State;

pub(crate) mod diagnostics;
pub(crate) mod listen;
pub(crate) mod pos;
pub(crate) mod state;

mod symbols {
    pub(crate) mod definition;
    pub(crate) mod hover;
    pub(crate) mod lookup;
}

mod handlers {
    pub(crate) mod notification;
    pub(crate) mod request;
}

pub(crate) use pos::*;

/// Options for the LSP server.
pub struct Options {
    /// Writes log output to the given file.
    pub log_file: Option<String>,

    /// Writes log output to standard output.
    pub log_stdout: bool,

    /// Defines how verbose the server should be.
    pub verbosity: Verbosity,
}

/// Verbosity level for the LSP server.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Verbosity {
    Warning,
    Info,
    Debug,
    Trace,
}

pub fn start_server(options: Options) -> std::result::Result<(), Box<dyn Error + Sync + Send>> {
    let level_filter = match options.verbosity {
        Verbosity::Warning => log::LevelFilter::Warn,
        Verbosity::Info => log::LevelFilter::Info,
        Verbosity::Debug => log::LevelFilter::Debug,
        Verbosity::Trace => log::LevelFilter::Trace,
    };

    if let Some(log_file) = options.log_file {
        simple_logging::log_to_file(log_file, level_filter)?;
    }

    if options.log_stdout {
        simple_logging::log_to(std::io::stdout(), level_filter);
    }

    let (conn, io) = Connection::stdio();
    let capabilities = capabilities();

    log::info!("starting up!");

    let params_json = conn.initialize(serde_json::json!(capabilities))?;
    let params = serde_json::from_value(params_json)?;

    std::panic::set_hook(Box::new(|panic_info| {
        if let Some(payload) = panic_info.payload_as_str() {
            log::error!("LSP server panicked: {payload}");
        } else {
            log::error!("LSP server panicked: <no payload>");
        }

        if let Some(location) = panic_info.location() {
            log::error!(
                "panic occurred in file '{}' at line {}",
                location.file(),
                location.line(),
            );
        }
    }));

    if let Err(err) = initialize(conn, params) {
        return Err(Box::new(std::io::Error::other(err.message())));
    }

    io.join().expect("joining lsp threads");
    log::info!("shutting down server...");

    Ok(())
}

pub fn capabilities() -> ServerCapabilities {
    ServerCapabilities {
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(false),
            ..Default::default()
        }),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        text_document_sync: Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
            open_close: Some(true),
            change: Some(TextDocumentSyncKind::FULL),
            save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                include_text: Some(true),
            })),
            ..Default::default()
        })),
        ..Default::default()
    }
}

fn initialize(connection: Connection, mut params: InitializeParams) -> Result<()> {
    let Some(workspace_root) = params.workspace_folders.take().map(|mut folders| folders.remove(0)) else {
        return Err(SimpleDiagnostic::new("no workspace root defined").into());
    };

    let workspace_root = ensure_trailing_slash(workspace_root);

    let mut state = State::new(connection.sender, workspace_root);
    state.compile_workspace();
    state.listen(connection.receiver)
}

fn ensure_trailing_slash(folder: WorkspaceFolder) -> Uri {
    if folder.uri.path().as_str().ends_with('/') {
        folder.uri.clone()
    } else {
        let uri = folder.uri.as_str();

        Uri::from_str(&format!("{uri}/")).unwrap()
    }
}
