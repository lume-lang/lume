use std::error::Error;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use lsp_server::Connection;
use lsp_types::*;

pub(crate) mod engine;
pub(crate) mod listener;
pub(crate) mod pos;
pub(crate) mod server;
pub(crate) mod status;

mod symbols {
    pub(crate) mod completions;
    pub(crate) mod definition;
    pub(crate) mod format;
    pub(crate) mod hover;
    pub(crate) mod lookup;
}

pub(crate) use pos::*;

#[cfg(test)]
pub(crate) mod test;

pub fn start_server() -> std::result::Result<(), Box<dyn Error + Sync + Send>> {
    let (conn, io) = Connection::stdio();
    let capabilities = capabilities();

    tracing::info!("starting up!");

    let (init_request_id, params) = conn.initialize_start()?;
    conn.initialize_finish(
        init_request_id,
        serde_json::json!({
            "capabilities": capabilities,
            "serverInfo": {
                "name": std::env!("CARGO_PKG_NAME"),
                "version": std::env!("CARGO_PKG_VERSION")
            }
        }),
    )?;

    let params = serde_json::from_value(params)?;

    std::panic::set_hook(Box::new(|panic_info| {
        if let Some(payload) = panic_info.payload().downcast_ref::<&str>() {
            tracing::error!("LSP server panicked: {payload}");
        } else if let Some(payload) = panic_info.payload().downcast_ref::<String>() {
            tracing::error!("LSP server panicked: {payload}");
        } else {
            tracing::error!("LSP server panicked: <no payload>");
        }

        if let Some(location) = panic_info.location() {
            tracing::error!(
                "panic occurred in file '{}' at line {}",
                location.file(),
                location.line(),
            );
        }
    }));

    let mut server = server::Server::new(params, conn.sender);
    server.listen(conn.receiver);

    if let Err(err) = io.join() {
        tracing::error!("failed to join lsp threads: {err:?}");
    }

    tracing::info!("shutting down server...");

    Ok(())
}

pub fn capabilities() -> ServerCapabilities {
    ServerCapabilities {
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(true),
            trigger_characters: Some(vec![String::from(":"), String::from("."), String::from("(")]),
            completion_item: Some(CompletionOptionsCompletionItem {
                label_details_support: Some(true),
            }),
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
