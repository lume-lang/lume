use std::hash::Hash;

// #[macro_use]
// pub mod macros;
pub mod support;

pub mod generated {
    pub mod ast;
}

pub use generated::ast::*;

#[cfg(test)]
mod tests;

pub trait AstNode {
    /// Attempts to cast the given syntax node into the current node type.
    ///
    /// If the conversion fails, [`None`] is returned.
    fn cast(syntax: lume_syntax::SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    /// Gets the underlying [`lume_syntax::SyntaxNode`] for this AST node.
    fn syntax(&self) -> &lume_syntax::SyntaxNode;

    /// Gets the source text which this node encapsulates.
    fn as_text(&self) -> String {
        self.syntax().text().to_string()
    }

    /// Gets the text range of this node in the source file.
    fn range(&self) -> std::ops::Range<usize> {
        let text_range = self.syntax().text_range();

        text_range.start().into()..text_range.end().into()
    }

    /// Gets the location of this node in the source file.
    #[inline]
    fn location(&self) -> Location {
        Location(self.range())
    }

    /// Returns an independent copy of the subtree rooted at this node.
    ///
    /// The parent of the returned node will be [`None`], the start offset will
    /// be zero, but, otherwise, it'll be equivalent to the source node.
    fn clone_subtree(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_subtree()).unwrap()
    }
}

/// An iterator over `SyntaxNode` children of a particular AST type.
#[derive(Debug, Clone)]
pub struct AstChildren<N> {
    inner: lume_syntax::SyntaxNodeChildren,
    _data: std::marker::PhantomData<N>,
}

impl<N> AstChildren<N> {
    fn new(parent: &lume_syntax::SyntaxNode) -> Self {
        AstChildren {
            inner: parent.children(),
            _data: std::marker::PhantomData,
        }
    }
}

impl<N: AstNode> Iterator for AstChildren<N> {
    type Item = N;

    fn next(&mut self) -> Option<N> {
        self.inner.find_map(N::cast)
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Location(pub std::ops::Range<usize>);

impl Location {
    #[inline]
    #[must_use]
    pub fn start(&self) -> usize {
        self.0.start
    }

    #[inline]
    #[must_use]
    pub fn end(&self) -> usize {
        self.0.end
    }

    #[inline]
    #[must_use]
    pub fn len(&self) -> usize {
        self.0.end - self.0.start
    }

    #[inline]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl From<std::ops::Range<usize>> for Location {
    fn from(range: std::ops::Range<usize>) -> Location {
        Location(range)
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum IntKind {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum FloatKind {
    F32,
    F64,
}
