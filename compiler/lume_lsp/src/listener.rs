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
    Completion(Completion),
    Hover(FileLocation),
    GoToDefinition(FileLocation),
    Format { uri: Uri, config: lume_fmt::Config },
    Unknown,
}

impl Request {
    pub fn from_message(message: lsp_server::Request) -> Self {
        use lsp_types::request::Request;

        match message.method.as_str() {
            lsp_types::request::Completion::METHOD => {
                let params = cast_request::<lsp_types::request::Completion>(message);
                let uri = params.text_document_position.text_document.uri;
                let position = params.text_document_position.position;

                let trigger_character = params
                    .context
                    .and_then(|ctx| ctx.trigger_character)
                    .and_then(|str| str.chars().next());

                let kind = match trigger_character {
                    Some('.') => CompletionKind::Instance,
                    Some(':') => CompletionKind::Static,
                    _ => CompletionKind::Scope,
                };

                Self::Completion(Completion {
                    location: FileLocation { uri, position },
                    kind,
                    trigger_character,
                })
            }

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

            lsp_types::request::Formatting::METHOD => {
                let params = cast_request::<lsp_types::request::Formatting>(message);
                let uri = params.text_document.uri;
                let config = crate::symbols::format::parse_formatting_config(params.options);

                Self::Format { uri, config }
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
    CommitDocument {
        uri: Uri,
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

                Self::CommitDocument { uri }
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
pub struct Completion {
    /// Defines the location of the completion
    pub location: FileLocation,

    /// Defines the kind of completion, based on the trigger character.
    pub kind: CompletionKind,

    /// Defines the character which triggered the completion, if any.
    pub trigger_character: Option<char>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompletionKind {
    /// Expand completions on the instance of a type
    Instance,

    /// Expand completions on a static type or path.
    Static,

    /// Expand completions from the surrounding scope and context.
    Scope,
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

pub fn receive(receiver: &mut Receiver<lsp_server::Message>) -> Handling {
    use lsp_types::request::Request;

    let timeout = Duration::from_millis(100);

    match receiver.recv_timeout(timeout).ok() {
        Some(lsp_server::Message::Request(request)) => {
            if request.method == lsp_types::request::Shutdown::METHOD {
                return Handling::Shutdown(request.id);
            }

            tracing::info!("received request: {}", request.method);

            Handling::from_request(request)
        }
        Some(lsp_server::Message::Notification(notification)) => {
            tracing::info!("received notification: {}", notification.method);

            Handling::from_notification(notification)
        }

        Some(lsp_server::Message::Response(resp)) => {
            tracing::error!("got unexpected response: {resp:?}");
            Handling::Empty
        }

        None => Handling::Empty,
    }
}
