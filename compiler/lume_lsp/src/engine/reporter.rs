use std::collections::HashSet;

use crossbeam::channel::Sender;
use lsp_server::RequestId;
use lsp_types::notification::Notification;
use lsp_types::request::Request;

pub const PACKAGE_CHECKING: &str = "package-check";

pub(crate) struct Reporter {
    sender: Sender<lsp_server::Message>,
    created_tokens: HashSet<&'static str>,
}

impl Reporter {
    pub fn new(sender: Sender<lsp_server::Message>) -> Self {
        Reporter {
            sender,
            created_tokens: HashSet::new(),
        }
    }

    /// Creates a new progress token.
    fn create_token(&mut self, token: &'static str) {
        // If we already created this token, don't create it again.
        if !self.created_tokens.insert(token) {
            return;
        }

        let params = lsp_types::WorkDoneProgressCreateParams {
            token: lsp_types::NumberOrString::String(token.to_string()),
        };

        let message = lsp_server::Request::new(
            RequestId::from(format!("create-work-token--{token}")),
            lsp_types::request::WorkDoneProgressCreate::METHOD.into(),
            params,
        );

        self.sender
            .send(lsp_server::Message::Request(message))
            .expect("create work progress token");
    }

    /// Send a progress beginning message to the client, identified by the given
    /// token.
    fn begin_progress(&mut self, token: &'static str, title: String, message: Option<String>) {
        self.create_token(token);

        self.send_progress(
            token,
            lsp_types::WorkDoneProgress::Begin(lsp_types::WorkDoneProgressBegin {
                title,
                cancellable: None,
                message,
                percentage: None,
            }),
        );
    }

    /// Send a progress update message to the client, identified by the given
    /// token.
    fn update_progress(&mut self, token: &'static str, message: Option<String>) {
        self.create_token(token);

        self.send_progress(
            token,
            lsp_types::WorkDoneProgress::Report(lsp_types::WorkDoneProgressReport {
                message,
                cancellable: None,
                percentage: None,
            }),
        );
    }

    /// Send a progress ending message to the client, identified by the given
    /// token.
    fn end_progress(&mut self, token: &'static str, message: Option<String>) {
        self.create_token(token);

        self.send_progress(
            token,
            lsp_types::WorkDoneProgress::End(lsp_types::WorkDoneProgressEnd { message }),
        );
    }

    /// Send a progress update to the client, identified by the given token.
    fn send_progress(&self, token: &'static str, progress: lsp_types::WorkDoneProgress) {
        let params = lsp_types::ProgressParams {
            token: lsp_types::NumberOrString::String(token.to_string()),
            value: lsp_types::ProgressParamsValue::WorkDone(progress),
        };

        let message = lsp_server::Notification::new(lsp_types::notification::Progress::METHOD.into(), params);

        self.sender
            .send(lsp_server::Message::Notification(message))
            .expect("send progress update");
    }
}

impl Reporter {
    /// Sends a progress update to the client, letting them know that
    /// package checking for a given workspace has started.
    pub fn check_started(&mut self) {
        self.begin_progress(PACKAGE_CHECKING, String::from("lume: checking packages"), None);
    }

    /// Sends a progress update to the client, letting them know that
    /// the checked packages are being indexed.
    pub fn check_indexing(&mut self) {
        self.update_progress(PACKAGE_CHECKING, Some(String::from("indexing")));
    }

    /// Sends a progress update to the client, letting them know that
    /// package checking for a given workspace has finished.
    pub fn check_finished(&mut self) {
        self.end_progress(PACKAGE_CHECKING, None);
    }
}
