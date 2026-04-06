use super::*;

#[test]
fn compile_hello_world() {
    let mut fixture = EngineFixture::new();
    fixture.set_filter(filter_ignore_progress);

    fixture.add_arcfile("hello-world");
    fixture.add_document(
        "src/main.lm",
        r#"fn main() {
            std::io::println("Hello, world!");
        }
        "#,
    );

    fixture.engine.compile();
    assert!(fixture.recv().is_none());
}

#[test]
fn compile_invalid_arcfile() {
    let mut fixture = EngineFixture::new();
    fixture.set_filter(filter_ignore_progress);

    fixture.add_document("Arcfile", "");
    fixture.add_document("src/main.lm", "fn main() { }");

    fixture.engine.compile();

    let msg = fixture.recv().expect("expected 'publishDiagnostics' notification");
    let lsp_server::Message::Notification(notification) = msg else {
        panic!("expected notification");
    };

    assert_eq!(notification.method, lsp_types::notification::PublishDiagnostics::METHOD);

    let params: lsp_types::PublishDiagnosticsParams = serde_json::from_value(notification.params).unwrap();
    assert!(params.uri.as_str().ends_with("/Arcfile"));
}

#[test]
fn compile_parse_failure() {
    let mut fixture = EngineFixture::new();
    fixture.set_filter(filter_ignore_progress);

    fixture.add_arcfile("hello-world");
    fixture.add_document("src/main.lm", "fn() { }");

    fixture.engine.compile();

    let msg = fixture.recv().expect("expected 'publishDiagnostics' notification");
    let lsp_server::Message::Notification(notification) = msg else {
        panic!("expected notification");
    };

    assert_eq!(notification.method, lsp_types::notification::PublishDiagnostics::METHOD);

    let mut params: lsp_types::PublishDiagnosticsParams = serde_json::from_value(notification.params).unwrap();
    assert!(params.uri.as_str().ends_with("/src/main.lm"));

    let diag = params.diagnostics.pop().unwrap();
    assert_eq!(diag.message, "error occurred here");
    assert_eq!(diag.range.start, Position::new(0, 2));
    assert_eq!(diag.range.end, Position::new(0, 3));
}

#[test]
fn compile_invalid_variable() {
    let mut fixture = EngineFixture::new();
    fixture.set_filter(filter_ignore_progress);

    fixture.add_arcfile("hello-world");
    fixture.add_document("src/main.lm", "fn main() { a = 4; }");

    fixture.engine.compile();

    let msg = fixture.recv().expect("expected 'publishDiagnostics' notification");
    let lsp_server::Message::Notification(notification) = msg else {
        panic!("expected notification");
    };

    assert_eq!(notification.method, lsp_types::notification::PublishDiagnostics::METHOD);

    let mut params: lsp_types::PublishDiagnosticsParams = serde_json::from_value(notification.params).unwrap();
    assert!(params.uri.as_str().ends_with("/src/main.lm"));

    let diag = params.diagnostics.pop().unwrap();
    assert_eq!(diag.message, "could not find a variable named a in the current scope");
    assert_eq!(diag.range.start, Position::new(0, 12));
    assert_eq!(diag.range.end, Position::new(0, 13));
}
