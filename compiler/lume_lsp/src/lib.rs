use std::error::Error;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::{Arc, Mutex};

use env_logger::Target;
use lsp_server::Connection;
use lsp_types::*;

pub(crate) mod engine;
pub(crate) mod listener;
pub(crate) mod pos;
pub(crate) mod server;

mod symbols {
    pub(crate) mod definition;
    pub(crate) mod format;
    pub(crate) mod hover;
    pub(crate) mod lookup;
}

pub(crate) use pos::*;

/// Options for the LSP server.
pub struct Options {
    /// Writes log output to the given file.
    pub log_file: Option<String>,

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
    initialize_logging(&options)?;

    let (conn, io) = Connection::stdio();
    let capabilities = capabilities();

    log::info!("starting up!");

    let params_json = conn.initialize(serde_json::json!(capabilities))?;
    let params = serde_json::from_value(params_json)?;

    std::panic::set_hook(Box::new(|panic_info| {
        if let Some(payload) = panic_info.payload().downcast_ref::<&str>() {
            log::error!("LSP server panicked: {payload}");
        } else if let Some(payload) = panic_info.payload().downcast_ref::<String>() {
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

    let mut server = server::Server::new(params, conn.sender);
    server.listen(conn.receiver);

    if let Err(err) = io.join() {
        log::error!("failed to join lsp threads: {err:?}");
    }

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
                include_text: Some(false),
            })),
            will_save: None,
            will_save_wait_until: None,
        })),
        definition_provider: Some(OneOf::Left(true)),
        document_formatting_provider: Some(OneOf::Left(true)),
        ..Default::default()
    }
}

fn initialize_logging(options: &Options) -> std::io::Result<()> {
    struct FileTarget {
        file: Arc<Mutex<File>>,
    }

    impl std::io::Write for FileTarget {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            match self.file.lock().unwrap().write(buf) {
                Ok(written) => Ok(written),
                Err(_err) => Ok(0),
            }
        }

        fn flush(&mut self) -> std::io::Result<()> {
            let mut file_handler = self.file.lock().unwrap();
            file_handler.flush()
        }
    }

    let level_filter = match options.verbosity {
        Verbosity::Warning => log::LevelFilter::Warn,
        Verbosity::Info => log::LevelFilter::Info,
        Verbosity::Debug => log::LevelFilter::Debug,
        Verbosity::Trace => log::LevelFilter::Trace,
    };

    let mut builder = env_logger::Builder::from_default_env();
    builder.filter_level(log::LevelFilter::Warn);
    builder.filter_module("lume_lsp", level_filter);

    if let Some(log_file) = &options.log_file {
        let file = std::fs::OpenOptions::new().create(true).append(true).open(log_file)?;

        let target = FileTarget {
            file: Arc::new(Mutex::new(file)),
        };

        builder.target(Target::Pipe(Box::new(target)));
    }

    builder.init();

    Ok(())
}

#[inline]
fn uri_to_path(uri: &Uri) -> PathBuf {
    if uri.scheme().is_some_and(|scheme| scheme.as_str() == "file") {
        PathBuf::from(uri.path().as_str())
    } else {
        PathBuf::from(uri.as_str())
    }
}

#[inline]
fn path_to_uri(path: &Path) -> Uri {
    Uri::from_str(&format!("file://{}", path.display())).unwrap()
}
