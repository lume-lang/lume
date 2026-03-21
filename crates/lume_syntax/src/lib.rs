use std::fmt::Debug;

pub use rowan::{Checkpoint, TextRange, TextSize};
use rowan::{GreenNode, GreenNodeBuilder};

pub mod syntax_node;
pub use syntax_node::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LumeLang {}

impl rowan::Language for LumeLang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::EOF as u16);

        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<LumeLang>;
pub type SyntaxToken = rowan::SyntaxToken<LumeLang>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<LumeLang>;

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

pub struct SyntaxTreeBuilder {
    pub errors: Vec<SyntaxError>,
    pub inner: GreenNodeBuilder<'static>,
}

impl SyntaxTreeBuilder {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn finish(self) -> SyntaxTree {
        SyntaxTree::new(self.inner.finish(), self.errors)
    }
}

impl Default for SyntaxTreeBuilder {
    fn default() -> Self {
        Self {
            errors: Vec::new(),
            inner: GreenNodeBuilder::new(),
        }
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
