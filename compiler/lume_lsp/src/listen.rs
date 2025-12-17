use std::time::Duration;

use crossbeam::channel::Receiver;
use lsp_types::{Position, Uri};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Message {
    Request(lsp_server::RequestId, Request),
    Notification(Notification),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Request {
    Hover(FileLocation),
    GoToDefinition(FileLocation),
    Unknown,
}

impl Request {
    pub fn from_message(message: lsp_server::Request) -> Self {
        use lsp_types::request::Request;

        match message.method.as_str() {
            lsp_types::request::HoverRequest::METHOD => {
                let params = cast_request::<lsp_types::request::HoverRequest>(message);
                let uri = params.text_document_position_params.text_document.uri;
                let position = params.text_document_position_params.position;

                Self::Hover(FileLocation { uri, position })
            }

            lsp_types::request::GotoDefinition::METHOD => {
                let params = cast_request::<lsp_types::request::GotoDefinition>(message);
                let uri = params.text_document_position_params.text_document.uri;
                let position = params.text_document_position_params.position;

                Self::GoToDefinition(FileLocation { uri, position })
            }

            _ => Self::Unknown,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Notification {
    OpenDocument {
        uri: Uri,
        text: String,
    },

    CloseDocument {
        uri: Uri,
    },

    /// A source file was modified and changes have been saved to disk.
    SaveDocument {
        uri: Uri,
        content: String,
    },

    /// A source file was modified in-memory, without being committed to disk.
    ChangeDocument {
        uri: Uri,
        content: String,
    },

    Unknown,
}

impl Notification {
    pub fn from_message(message: lsp_server::Notification) -> Self {
        use lsp_types::notification::Notification;

        match message.method.as_str() {
            lsp_types::notification::DidOpenTextDocument::METHOD => {
                let params = cast_notification::<lsp_types::notification::DidOpenTextDocument>(message);
                let lsp_types::TextDocumentItem { uri, text, .. } = params.text_document;

                Self::OpenDocument { uri, text }
            }

            lsp_types::notification::DidCloseTextDocument::METHOD => {
                let params = cast_notification::<lsp_types::notification::DidCloseTextDocument>(message);
                let lsp_types::TextDocumentIdentifier { uri } = params.text_document;

                Self::CloseDocument { uri }
            }

            lsp_types::notification::DidSaveTextDocument::METHOD => {
                let params = cast_notification::<lsp_types::notification::DidSaveTextDocument>(message);
                let lsp_types::TextDocumentIdentifier { uri } = params.text_document;
                let content = params.text.expect("expected full document content on save");

                Self::SaveDocument { uri, content }
            }

            lsp_types::notification::DidChangeTextDocument::METHOD => {
                let params = cast_notification::<lsp_types::notification::DidChangeTextDocument>(message);
                let lsp_types::VersionedTextDocumentIdentifier { uri, .. } = params.text_document;
                let content = params.content_changes.first().unwrap().text.clone();

                Self::ChangeDocument { uri, content }
            }

            _ => Self::Unknown,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileLocation {
    /// Defines the URI of the file
    pub uri: Uri,

    /// Defines the position of the file
    pub position: Position,
}

fn cast_request<R>(request: lsp_server::Request) -> R::Params
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    let (_, params) = request.extract(R::METHOD).expect("cast request");
    params
}

fn cast_notification<R>(notification: lsp_server::Notification) -> R::Params
where
    R: lsp_types::notification::Notification,
    R::Params: serde::de::DeserializeOwned,
{
    notification.extract::<R::Params>(R::METHOD).expect("cast notification")
}

#[derive(Default)]
pub(crate) struct Listener {
    // ..
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Handling {
    Message(Message),

    /// Currently no messages in the buffer.
    Empty,

    /// Received shutdown request.
    Shutdown(lsp_server::RequestId),
}

impl Handling {
    pub fn from_request(request: lsp_server::Request) -> Self {
        let request_id = request.id.clone();
        let request = Request::from_message(request);

        Handling::Message(Message::Request(request_id, request))
    }

    pub fn from_notification(notification: lsp_server::Notification) -> Self {
        let notification = Notification::from_message(notification);

        Handling::Message(Message::Notification(notification))
    }
}

impl Listener {
    pub fn receive(&mut self, receiver: &mut Receiver<lsp_server::Message>) -> Handling {
        use lsp_types::request::Request;

        let timeout = Duration::from_millis(100);

        match receiver.recv_timeout(timeout).ok() {
            Some(lsp_server::Message::Request(request)) => {
                if request.method == lsp_types::request::Shutdown::METHOD {
                    return Handling::Shutdown(request.id);
                }

                Handling::from_request(request)
            }
            Some(lsp_server::Message::Notification(notification)) => Handling::from_notification(notification),

            Some(lsp_server::Message::Response(resp)) => {
                log::error!("got unexpected response: {resp:?}");
                Handling::Empty
            }

            None => Handling::Empty,
        }
    }
}
