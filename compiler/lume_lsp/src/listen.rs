use crossbeam::channel::Receiver;
use lsp_server::{Message, Response};
use lsp_types::request::{Request, Shutdown};
use lume_errors::{IntoDiagnostic, Result};

use crate::handlers;
use crate::state::State;

impl State {
    /// Starts listening on the given [`Connection`] for LSP requests and
    /// notifications.
    pub fn listen(&mut self, receiver: Receiver<Message>) -> Result<()> {
        for msg in &receiver {
            match msg {
                Message::Request(req) => {
                    if req.method == Shutdown::METHOD {
                        log::info!("received shutdown request");

                        let resp = Response::new_ok(req.id.clone(), ());
                        let _ = self.dispatcher.send(resp.into());

                        break;
                    }

                    if let Err(err) = self.handle_request(&req) {
                        log::error!("request {} failed: {err}", &req.method);
                    }
                }
                Message::Notification(req) => {
                    if let Err(err) = self.handle_notification(&req) {
                        log::error!("notification {} failed: {err}", &req.method);
                    }
                }
                Message::Response(resp) => {
                    log::error!("got unexpected response: {resp:?}");
                }
            }
        }

        Ok(())
    }

    fn handle_request(&mut self, request: &lsp_server::Request) -> Result<()> {
        log::warn!("received request: {}", request.method);

        match request.method.as_str() {
            lsp_types::request::HoverRequest::METHOD => {
                let params: lsp_types::HoverParams = match serde_json::from_value(request.params.clone()) {
                    Ok(params) => params,
                    Err(err) => return Err(err.into_diagnostic()),
                };

                handlers::request::on_hover(self, request.id.clone(), params)?;
            }
            lsp_types::request::GotoDefinition::METHOD => {
                let params: lsp_types::GotoDefinitionParams = match serde_json::from_value(request.params.clone()) {
                    Ok(params) => params,
                    Err(err) => return Err(err.into_diagnostic()),
                };

                handlers::request::go_to_definition(self, request.id.clone(), params)?;
            }
            _ => {}
        }

        Ok(())
    }

    fn handle_notification(&mut self, notification: &lsp_server::Notification) -> Result<()> {
        use lsp_types::notification::Notification;

        log::debug!("received notification: {}", notification.method);

        match notification.method.as_str() {
            lsp_types::notification::DidOpenTextDocument::METHOD => {
                let params: lsp_types::DidOpenTextDocumentParams =
                    match serde_json::from_value(notification.params.clone()) {
                        Ok(params) => params,
                        Err(err) => return Err(err.into_diagnostic()),
                    };

                handlers::notification::open_document(self, params);
            }
            lsp_types::notification::DidCloseTextDocument::METHOD => {
                let params: lsp_types::DidCloseTextDocumentParams =
                    match serde_json::from_value(notification.params.clone()) {
                        Ok(params) => params,
                        Err(err) => return Err(err.into_diagnostic()),
                    };

                handlers::notification::close_document(self, params);
            }
            lsp_types::notification::DidSaveTextDocument::METHOD => {
                let params: lsp_types::DidSaveTextDocumentParams =
                    match serde_json::from_value(notification.params.clone()) {
                        Ok(params) => params,
                        Err(err) => return Err(err.into_diagnostic()),
                    };

                handlers::notification::save_document(self, params);
            }
            lsp_types::notification::DidChangeTextDocument::METHOD => {
                let params: lsp_types::DidChangeTextDocumentParams =
                    match serde_json::from_value(notification.params.clone()) {
                        Ok(params) => params,
                        Err(err) => return Err(err.into_diagnostic()),
                    };

                handlers::notification::change_document(self, params);
            }
            _ => {}
        }

        Ok(())
    }
}
