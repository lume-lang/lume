use std::sync::Arc;

use lume_span::SourceFile;

use super::*;

impl crate::SourceFile {
    pub fn parse(text: &str) -> lume_syntax::SyntaxTree {
        lume_parser::Parser::from_source(Arc::new(SourceFile::internal(text)))
            .map(|parser| parser.parse(lume_parser::Target::Item))
            .expect("failed to tokenize input")
    }
}

impl crate::Stmt {
    pub fn parse(text: &str) -> lume_syntax::SyntaxTree {
        lume_parser::Parser::from_source(Arc::new(SourceFile::internal(text)))
            .map(|parser| parser.parse(lume_parser::Target::Statement))
            .expect("failed to tokenize input")
    }
}

#[test]
fn visibility() {
    let tree = crate::SourceFile::parse(
        r#"
        pub fn test() {}
        pub(internal) fn test() {}
        priv fn test() {}
        "#,
    );

    let mut fn_iter = tree.syntax().descendants().filter_map(Fn::cast);

    let fn1 = fn_iter.next().unwrap();
    let fn2 = fn_iter.next().unwrap();
    let fn3 = fn_iter.next().unwrap();

    assert_eq!(fn1.visibility().unwrap().syntax().text(), r#"pub "#);
    assert_eq!(fn2.visibility().unwrap().syntax().text(), r#"pub(internal)"#);
    assert_eq!(fn3.visibility().unwrap().syntax().text(), r#"priv"#);
}

#[test]
fn imports() {
    let tree = crate::SourceFile::parse("import std::io (File, Buffer)");

    let imp = tree.syntax().descendants().find_map(Import::cast).unwrap();

    let mut path_segs = imp.import_path().unwrap().path();
    assert_eq!(path_segs.next().unwrap().syntax().text(), "std");
    assert_eq!(path_segs.next().unwrap().syntax().text(), "io");

    let mut items = imp.import_list().unwrap().items();
    assert_eq!(items.next().unwrap().syntax().text(), "File");
    assert_eq!(items.next().unwrap().syntax().text(), "Buffer");
}

#[test]
fn namespace() {
    let tree = crate::SourceFile::parse("namespace std::io");

    let ns = tree.syntax().descendants().find_map(Namespace::cast).unwrap();

    let mut segments = ns.import_path().unwrap().path();
    assert_eq!(segments.next().unwrap().syntax().text(), "std");
    assert_eq!(segments.next().unwrap().syntax().text(), "io");
}

#[test]
fn attributes() {
    let tree = crate::SourceFile::parse("![unused()] fn foo() {}");

    let ns = tree.syntax().descendants().find_map(Fn::cast).unwrap();
    let attr = ns.attr().next().unwrap();
    assert_eq!(attr.name().unwrap().syntax().text(), "unused");
}

#[test]
fn if_block_condition() {
    let tree = crate::SourceFile::parse(
        r#"
        fn test() {
            if true { "if" }
            else if { false } { "first elif" }
            else if true { "second elif" }
            else if (true) { "third elif" }
            else { "else" }
        }
        "#,
    );

    let if_ = tree.syntax().descendants().find_map(IfExpr::cast).unwrap();
    let mut cases = if_.cases();

    let case = cases.next().unwrap();
    assert_eq!(case.condition().unwrap().syntax().text(), r#"true"#);
    assert_eq!(case.block().unwrap().syntax().text(), r#"{ "if" }"#);

    let case = cases.next().unwrap();
    assert_eq!(case.condition().unwrap().syntax().text(), r#"{ false }"#);
    assert_eq!(case.block().unwrap().syntax().text(), r#"{ "first elif" }"#);

    let case = cases.next().unwrap();
    assert_eq!(case.condition().unwrap().syntax().text(), r#"true"#);
    assert_eq!(case.block().unwrap().syntax().text(), r#"{ "second elif" }"#);

    let case = cases.next().unwrap();
    assert_eq!(case.condition().unwrap().syntax().text(), r#"(true)"#);
    assert_eq!(case.block().unwrap().syntax().text(), r#"{ "third elif" }"#);
}
