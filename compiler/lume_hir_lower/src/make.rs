use std::sync::Arc;

use lume_ast::AstNode;
use lume_parser::Target;

#[track_caller]
pub(crate) fn parse_from_text<N: AstNode>(text: &str, target: Target) -> N {
    let parse = lume_parser::Parser::from_source(Arc::new(lume_span::SourceFile::internal(text)))
        .map(|parser| parser.parse(target))
        .expect("failed to tokenize input");

    let node: N = if let Some(it) = parse.syntax().descendants().find_map(N::cast) {
        it
    } else {
        let node = std::any::type_name::<N>();
        panic!("Failed to make ast node `{node}` from text {text}")
    };

    let node = node.clone_subtree();
    assert_eq!(node.syntax().text_range().start(), 0.into());

    node
}
