use std::fmt::Debug;

pub use cstree::build::Checkpoint;
use cstree::prelude::*;
pub use cstree::text::{TextRange, TextSize};

pub mod syntax_node;
pub use syntax_node::*;

impl cstree::Syntax for SyntaxKind {
    fn from_raw(raw: cstree::RawSyntaxKind) -> Self {
        assert!(raw.0 <= SyntaxKind::EOF as u32);

        unsafe { std::mem::transmute::<u32, SyntaxKind>(raw.0) }
    }

    fn into_raw(self) -> cstree::RawSyntaxKind {
        self.into()
    }

    fn static_text(self) -> Option<&'static str> {
        None
    }
}

pub type SyntaxNode = cstree::syntax::SyntaxNode<SyntaxKind>;
pub type SyntaxToken = cstree::syntax::SyntaxToken<SyntaxKind>;
pub type SyntaxNodeChildren<'n> = cstree::syntax::SyntaxNodeChildren<'n, SyntaxKind>;

#[derive(Default, Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextSpan(pub usize, pub usize);

impl std::fmt::Display for TextSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.0, self.1)
    }
}

pub struct SyntaxTree {
    root: GreenNode,
    errors: Vec<SyntaxError>,
}

impl SyntaxTree {
    #[inline]
    pub fn new(root: GreenNode, errors: Vec<SyntaxError>) -> Self {
        Self { root, errors }
    }

    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.root.clone())
    }

    pub fn errors(&self) -> &[SyntaxError] {
        &self.errors
    }
}

impl Debug for SyntaxTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.syntax())?;

        if !self.errors.is_empty() {
            write!(
                f,
                "\n{}",
                self.errors
                    .iter()
                    .map(|err| err.to_string())
                    .collect::<Vec<_>>()
                    .join("\n")
            )?;
        }

        Ok(())
    }
}

#[derive(Default)]
pub struct SyntaxTreeBuilder {
    pub errors: Vec<SyntaxError>,
    pub inner: GreenNodeBuilder<'static, 'static, SyntaxKind>,
}

impl SyntaxTreeBuilder {
    #[inline]
    pub fn finish(self) -> SyntaxTree {
        SyntaxTree::new(self.inner.finish().0, self.errors)
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct SyntaxError(String, TextSpan);

impl SyntaxError {
    pub fn new<M: Into<String>>(message: M, range: TextSpan) -> Self {
        Self(message.into(), range)
    }

    pub fn new_at_offset<M: Into<String>>(message: M, offset: usize) -> Self {
        Self(message.into(), TextSpan(offset, offset))
    }

    pub fn span(&self) -> TextSpan {
        self.1
    }

    pub fn with_span(mut self, span: TextSpan) -> Self {
        self.1 = span;
        self
    }
}

impl std::fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}@{}", self.0, self.1)
    }
}

impl std::error::Error for SyntaxError {}
