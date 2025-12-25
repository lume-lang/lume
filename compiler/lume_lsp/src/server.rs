use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::path::{Path, PathBuf};

use crossbeam::channel::{Receiver, Sender};
use lsp_server::RequestId;
use lsp_types::{InitializeParams, Uri};
use lume_errors::{IntoDiagnostic, Result};

use crate::engine::Engine;
use crate::listener::*;

pub(crate) struct Server {
    params: InitializeParams,
    sender: Sender<lsp_server::Message>,
    engines: HashMap<PathBuf, Engine>,
}

impl Server {
    pub(crate) fn new(params: InitializeParams, sender: Sender<lsp_server::Message>) -> Self {
        Self {
            params,
            sender,
            engines: HashMap::new(),
        }
    }

    /// Starts listening on the given [`Connection`] for LSP requests and
    /// notifications.
    pub fn listen(&mut self, mut receiver: Receiver<lsp_server::Message>) {
        if let Some(client_info) = self.params.client_info.as_ref() {
            let client = match client_info.version.as_ref() {
                Some(version) => format!("{} v{version}", client_info.name),
                None => client_info.name.clone(),
            };

            log::info!("listening for events from {client}");
        }

        loop {
            match crate::listener::receive(&mut receiver) {
                Handling::Shutdown(req_id) => {
                    log::info!("received shutdown request");

                    let resp = lsp_server::Response::new_ok(req_id, ());
                    let _ = self.sender.send(resp.into());

                    break;
                }

                Handling::Message(message) => {
                    if let Err(err) = self.handle_message(message) {
                        log::error!("error handling message: {err}");
                    }
                }

                Handling::Empty => {}
            }
        }
    }

    fn handle_message(&mut self, message: Message) -> Result<()> {
        match message {
            Message::Request(id, request) => self.handle_request(id, request),
            Message::Notification(notification) => {
                self.handle_notification(notification);
                Ok(())
            }
        }
    }

    fn handle_request(&mut self, id: lsp_server::RequestId, request: Request) -> Result<()> {
        let response = match request {
            Request::Hover(location) => self.hover(location)?,
            Request::GoToDefinition(location) => self.goto_definition(location)?,
            Request::Unknown => return Ok(()),
        };

        self.respond(id, response)
    }

    fn handle_notification(&mut self, notification: Notification) {
        match notification {
            Notification::OpenDocument { uri, text } => self.open_document(uri, text),
            Notification::CloseDocument { uri } => self.close_document(uri),
            Notification::CommitDocument { uri } => self.save_document(uri),
            Notification::ChangeDocument { uri, content } => self.change_document(uri, content),
            Notification::Unknown => {}
        }
    }

    /// Returns the engine for the given path. If no engine is found, creates a
    /// new one.
    ///
    /// The path is traversed to find the package root. If the package root is
    /// not found, returns [`None`].
    fn engine_for_path(&mut self, path: PathBuf) -> Option<&mut Engine> {
        let package_root = locate_package_root(path)?;

        Some(match self.engines.entry(package_root.clone()) {
            Entry::Occupied(engine) => engine.into_mut(),
            Entry::Vacant(entry) => {
                log::info!("creating new engine for workspace {}", package_root.display());

                let mut engine = Engine::new(package_root, self.sender.clone());
                engine.compile();

                entry.insert(engine)
            }
        })
    }

    /// Responds to the given request ID with the given JSON-encoded value.
    fn respond(&self, id: RequestId, value: serde_json::Value) -> Result<()> {
        let resp = lsp_server::Response::new_ok(id, value);

        match self.sender.send(lsp_server::Message::Response(resp)) {
            Ok(()) => Ok(()),
            Err(err) => Err(err.into_diagnostic()),
        }
    }

    /// Invokes the given closure with the engine for the given path.
    ///
    /// If an engine is not found for the given path, a new engine is created.
    fn with_engine<R, F: FnOnce(&mut Engine) -> R>(&mut self, path: PathBuf, f: F) -> Option<R> {
        let engine = self.engine_for_path(path)?;
        Some(f(engine))
    }

    /// Creates a new JSON-encoded response from the return value of the given
    /// closure. Note: the response is *not* sent to the client - only
    /// serialized.
    ///
    /// The closure is invoked with the engine for the given path. If an engine
    /// is not found for the given path, a new engine is created.
    fn respond_with_engine<R: serde::Serialize, F: FnOnce(&mut Engine) -> Result<R>>(
        &mut self,
        path: PathBuf,
        f: F,
    ) -> Result<serde_json::Value> {
        if let Some(engine) = self.engine_for_path(path.clone()) {
            let result = f(engine)?;
            let json = serde_json::to_value(result).expect("serializing response");

            Ok(json)
        } else {
            log::error!("could not find engine for path {}", path.display());

            Ok(serde_json::Value::Null)
        }
    }
}

impl Server {
    fn open_document(&mut self, uri: Uri, content: String) {
        let path = crate::uri_to_path(&uri);
        self.with_engine(path, |engine| engine.open_document(uri, content));
    }

    fn close_document(&mut self, uri: Uri) {
        let path = crate::uri_to_path(&uri);
        self.with_engine(path, |engine| engine.close_document(uri));
    }

    fn save_document(&mut self, uri: Uri) {
        let path = crate::uri_to_path(&uri);
        self.with_engine(path, |engine| engine.save_document(uri));
    }

    fn change_document(&mut self, uri: Uri, content: String) {
        let path = crate::uri_to_path(&uri);
        self.with_engine(path, |engine| engine.update_document(uri, content));
    }
}

impl Server {
    fn hover(&mut self, location: FileLocation) -> Result<serde_json::Value> {
        let path = crate::uri_to_path(&location.uri);
        self.respond_with_engine(path, |engine| engine.hover(location))
    }

    fn goto_definition(&mut self, location: FileLocation) -> Result<serde_json::Value> {
        let path = crate::uri_to_path(&location.uri);
        self.respond_with_engine(path, |engine| engine.go_to_definition(location))
    }
}

fn locate_package_root(mut path: PathBuf) -> Option<PathBuf> {
    if path.starts_with("file://") {
        let path_str = path.to_str().unwrap();
        path = PathBuf::from(&path_str[7..]);
    }

    if !path.has_root() {
        path = PathBuf::from("/").join(path);
    }

    if is_package_root(&path) {
        return Some(path);
    }

    while let Some(parent) = path.parent() {
        if !is_package_root(parent) {
            path = parent.to_path_buf();
            continue;
        }

        return Some(parent.to_path_buf());
    }

    None
}

fn is_package_root(path: &Path) -> bool {
    path.join("Arcfile").exists()
}
