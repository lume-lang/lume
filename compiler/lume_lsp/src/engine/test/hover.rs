use super::*;

#[test]
fn hover_function_definition() {
    let mut fixture = EngineFixture::new();
    fixture.set_filter(filter_ignore_progress);

    fixture.add_arcfile("hello-world");
    fixture.add_document("src/main.lm", "fn main() { }");

    fixture.engine.compile();

    let root = fixture.engine.root.display();
    let uri = Uri::from_str(&format!("file://{root}/src/main.lm")).unwrap();

    let hover = fixture
        .engine
        .hover(FileLocation {
            uri,
            position: Position { line: 0, character: 5 },
        })
        .unwrap();

    insta::assert_debug_snapshot!(hover.contents);
}

#[test]
fn hover_variable_declaration() {
    let mut fixture = EngineFixture::new();
    fixture.set_filter(filter_ignore_progress);

    fixture.add_arcfile("hello-world");
    fixture.add_document(
        "src/main.lm",
        r#"fn main() {
            let a = 3_i32;
        }"#,
    );

    fixture.engine.compile();

    let root = fixture.engine.root.display();
    let uri = Uri::from_str(&format!("file://{root}/src/main.lm")).unwrap();

    let hover = fixture
        .engine
        .hover(FileLocation {
            uri,
            position: Position { line: 1, character: 16 },
        })
        .unwrap();

    insta::assert_debug_snapshot!(hover.contents);
}

#[test]
fn hover_variable_reference() {
    let mut fixture = EngineFixture::new();
    fixture.set_filter(filter_ignore_progress);

    fixture.add_arcfile("hello-world");
    fixture.add_document(
        "src/main.lm",
        r#"fn main() {
            let a = 3_i32;
            let b = a;
        }"#,
    );

    fixture.engine.compile();

    let root = fixture.engine.root.display();
    let uri = Uri::from_str(&format!("file://{root}/src/main.lm")).unwrap();

    let hover = fixture
        .engine
        .hover(FileLocation {
            uri,
            position: Position { line: 2, character: 20 },
        })
        .unwrap();

    insta::assert_debug_snapshot!(hover.contents);
}
