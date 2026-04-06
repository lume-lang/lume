mod compile;

use crossbeam::channel::Receiver;
use lsp_types::notification::Notification as _;
use lsp_types::request::Request as _;

use crate::engine::Engine;
use crate::*;

type MessageFilter = fn(&lsp_server::Message) -> bool;

struct EngineFixture {
    receiver: Receiver<lsp_server::Message>,
    engine: Engine,
    filter: Option<MessageFilter>,
}

impl EngineFixture {
    pub fn new() -> Self {
        let compiler_root = lume_assets::compiler_root_dir().unwrap();
        let (sender, receiver) = crossbeam::channel::unbounded();

        Self {
            receiver,
            engine: Engine::new(compiler_root, sender),
            filter: None,
        }
    }

    pub fn set_filter(&mut self, filter: MessageFilter) {
        self.filter = Some(filter);
    }

    pub fn recv(&mut self) -> Option<lsp_server::Message> {
        while let Some(msg) = self.receiver.try_recv().ok() {
            if self.filter.is_some_and(|filter| filter(&msg)) {
                return Some(msg);
            }
        }

        None
    }

    pub fn add_document<P: AsRef<Path>, C: AsRef<str>>(&mut self, path: P, content: C) {
        let path = path.as_ref().as_os_str().to_str().unwrap();
        let uri = Uri::from_str(path).unwrap();

        self.engine.open_document(uri, content.as_ref().to_string());
    }

    pub fn add_arcfile<S: AsRef<str>>(&mut self, package_name: S) {
        self.add_document(
            "Arcfile",
            format!(
                r#"[package]
                name = "{}"
                version = "1.0.0"
                lume_version = "^0"
                "#,
                package_name.as_ref()
            ),
        );
    }
}

fn filter_ignore_progress(msg: &lsp_server::Message) -> bool {
    match msg {
        lsp_server::Message::Notification(notif) => {
            !matches!(notif.method.as_str(), lsp_types::notification::Progress::METHOD)
        }
        lsp_server::Message::Request(req) => {
            !matches!(req.method.as_str(), lsp_types::request::WorkDoneProgressCreate::METHOD)
        }
        lsp_server::Message::Response(_res) => true,
    }
}

#[test]
fn compile_empty_directory() {
    let mut fixture = EngineFixture::new();
    fixture.set_filter(filter_ignore_progress);
    fixture.engine.compile();

    let msg = fixture.recv().expect("expected 'window/showMessage' notification");
    let lsp_server::Message::Notification(notification) = msg else {
        panic!("expected notification");
    };

    assert_eq!(notification.method, lsp_types::notification::ShowMessage::METHOD);

    let params: lsp_types::ShowMessageParams = serde_json::from_value(notification.params).unwrap();
    assert!(params.message.starts_with("missing Arcfile within /"));
}
