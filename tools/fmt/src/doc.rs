#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Document<'a> {
    /// Renders the number of newlines, as defined by the value within the
    /// variant.
    Line(usize),

    /// A slice of UTF-8 text to display, such as `fn`, `return`, etc.
    ///
    /// When emitting `Text` or `String` nodes, they will be rendered without
    /// any added newlines or whitespace. In fact, they aren't altered by the
    /// formatter at all.
    Text(&'a str),

    /// An owned `String` of UTF-8 text to render.
    ///
    /// When emitting `Text` or `String` nodes, they will be rendered without
    /// any added newlines or whitespace. In fact, they aren't altered by the
    /// formatter at all.
    String(String),

    /// Renders a list of documents sequentually. This document type does not
    /// add any newlines or other whitespace.
    Vec(Vec<Self>),

    /// Defines a group of documents.
    ///
    /// When emitting a group from the formatter, it will attempt to place the
    /// entire group within a single line. If the length of the group
    /// exceeds the maximum line length, all `Break` nodes within the group
    /// will be broken. When a group is broken, `Break` nodes from any
    /// nested groups will *not* be broken.
    Group(Box<Self>),

    /// When emitting the inner document, force it to break all `Break` nodes
    /// within it. The inner document does not have to contain a `Group` for it
    /// to break, but often are.
    ForceBroken(Box<Self>),

    /// A group of nodes where each node is indented, if the given condition is
    /// met. If the condition is met, the inner document will be indented by
    /// `indent` spaces
    Nested {
        indent: isize,
        condition: NestCondition,
        doc: Box<Self>,
    },

    /// Condtional rendering, used to render different text slices depending on
    /// whether the surrounding group is broken or not. This node does not add
    /// any newlines or spaces.
    ///
    /// This is useful when emittimg array slices, where you'd like for the last
    /// element to only contain a comma, if the array expression is broken.
    Conditional { broken: &'a str, unbroken: &'a str },

    /// Condtional rendering, used to render different text slices depending on
    /// whether the surrounding group is broken or not.
    ///
    /// As opposed to `Conditional`, `Break` will emit a newline whenever the
    /// parent group is broken.
    Break {
        broken: &'a str,
        unbroken: &'a str,
        kind: BreakKind,
    },
}

/// Represents a condition which has to be met before a [`Document::Nested`]
/// node is nested.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NestCondition {
    /// Always apply the nesting.
    ///
    /// This is mostly used for blocks and scopes, where the content should
    /// always be indented, regardless of whether the wrapping group is
    /// broken.
    Always,

    /// Only nest the contained document if the parent group is broken.
    ///
    /// This is mostly used for arrays, parameter- and argument-lists, where
    /// the content should only be indented if it cannot fit the line.
    IfBroken,
}

/// Defines what kind sort of break to use in [`Document::Break`] nodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BreakKind {
    /// Causes the node to break if the parent group is broken.
    ///
    /// For example, given a maximum length of `40`, the following call
    /// expression would break all arguments:
    /// ```lm
    /// callee.method(
    ///     argument_a,
    ///     argument_b,
    ///     argument_c,
    /// )
    /// ```
    Strict,

    /// If the parent group is broken, only break the node if node still cannot
    /// find on the line.
    ///
    /// For example, given a maximum length of `40`, the following call
    /// expression would keep the arguments unbroken until the maximum line
    /// length was hit:
    /// ```lm
    /// callee.method(argument_a, argument_b,
    ///     argument_c)
    /// ```
    Flex,
}

/// Returns an empty node.
pub fn empty<'a>() -> Document<'a> {
    Document::Vec(vec![])
}

/// Returns a single newline.
pub fn line<'a>() -> Document<'a> {
    Document::Line(1)
}

/// Returns a document with the given amount of newlines.
pub fn lines<'a>(num: usize) -> Document<'a> {
    Document::Line(num)
}

/// Returns a node with the given text content.
pub fn str(s: &str) -> Document<'_> {
    Document::Text(s)
}

/// Create a conditional rendering node, where the emitted content
/// depends on whether the wrapping group is broken or not.
///
/// For more information, see [`Document::Conditional`].
pub fn cond<'a>(broken: &'a str, unbroken: &'a str) -> Document<'a> {
    Document::Conditional { broken, unbroken }
}

/// Create a conditional rendering node, where the emitted content
/// depends on whether the wrapping group is broken or not.
///
/// For more information, see [`Document::Break`] and [`BreakKind::Strict`].
pub fn break_<'a>(broken: &'a str, unbroken: &'a str) -> Document<'a> {
    Document::Break {
        broken,
        unbroken,
        kind: BreakKind::Strict,
    }
}

/// Create a conditional rendering node, where the emitted content
/// depends on whether the wrapping group is broken or not.
///
/// For more information, see [`Document::Break`] and [`BreakKind::Flex`].
pub fn flex_break<'a>(broken: &'a str, unbroken: &'a str) -> Document<'a> {
    Document::Break {
        broken,
        unbroken,
        kind: BreakKind::Flex,
    }
}

/// Concatenates all the documents from the given iterator into a single
/// document.
pub fn concat<'a, I: IntoIterator<Item = Document<'a>>>(iter: I) -> Document<'a> {
    Document::Vec(iter.into_iter().collect())
}

/// Concatenates all the documents from the given iterator into a single
/// document, where the given separator is interspersed between each document.
pub fn join<'a, I: IntoIterator<Item = Document<'a>>, S: AsDocument<'a>>(iter: I, sep: S) -> Document<'a> {
    Document::Vec(iter_tools::intersperse(iter, sep.as_doc()).collect())
}

pub trait AsDocument<'a> {
    fn as_doc(self) -> Document<'a>;
}

impl<'a> AsDocument<'a> for bool {
    fn as_doc(self) -> Document<'a> {
        Document::String(format!("{self}"))
    }
}

impl<'a> AsDocument<'a> for char {
    fn as_doc(self) -> Document<'a> {
        Document::String(format!("{self}"))
    }
}

impl<'a> AsDocument<'a> for &'a str {
    fn as_doc(self) -> Document<'a> {
        Document::Text(self)
    }
}

impl<'a> AsDocument<'a> for String {
    fn as_doc(self) -> Document<'a> {
        Document::String(self)
    }
}

impl<'a> AsDocument<'a> for i64 {
    fn as_doc(self) -> Document<'a> {
        Document::String(format!("{self}"))
    }
}

impl<'a> AsDocument<'a> for isize {
    fn as_doc(self) -> Document<'a> {
        Document::String(format!("{self}"))
    }
}

impl<'a> AsDocument<'a> for usize {
    fn as_doc(self) -> Document<'a> {
        Document::String(format!("{self}"))
    }
}

impl<'a> AsDocument<'a> for f64 {
    fn as_doc(self) -> Document<'a> {
        Document::String(format!("{self}"))
    }
}

impl<'a> AsDocument<'a> for Document<'a> {
    fn as_doc(self) -> Document<'a> {
        self
    }
}

impl<'a> AsDocument<'a> for Vec<Document<'a>> {
    fn as_doc(self) -> Document<'a> {
        Document::Vec(self)
    }
}

impl<'a> Document<'a> {
    /// Places the current document into a group containing itself.
    ///
    /// When emitting a group from the formatter, it will attempt to place the
    /// entire group within a single line. If the length of the group
    /// exceeds the maximum line length, all `Break` nodes within the group
    /// will be broken. When a group is broken, `Break` nodes from any
    /// nested groups will *not* be broken.
    pub fn group(self) -> Self {
        Self::Group(Box::new(self))
    }

    /// Appends another document to the current document.
    pub fn append(self, second: impl AsDocument<'a>) -> Self {
        match self {
            Self::Vec(mut vec) => {
                vec.push(second.as_doc());
                Self::Vec(vec)
            }
            first => Self::Vec(vec![first, second.as_doc()]),
        }
    }

    /// Nests the current document into a [`Document::Nested`] node with the
    /// given indentation.
    pub fn nest(self, indent: isize) -> Self {
        Self::Nested {
            indent,
            condition: NestCondition::Always,
            doc: Box::new(self),
        }
    }

    /// Nests the current document into a [`Document::Nested`] node with the
    /// given indentation, but only if the current group is broken.
    pub fn nest_if_broken(self, indent: isize) -> Self {
        Self::Nested {
            indent,
            condition: NestCondition::IfBroken,
            doc: Box::new(self),
        }
    }

    /// Nests the current document into a [`Document::ForceBroken`] node,
    /// forcing wrapped [`Document::Break`] nodes to break.
    pub fn force_break(self) -> Self {
        Self::ForceBroken(Box::new(self))
    }

    /// Inserts a single space in the current document.
    #[inline]
    pub fn space(self) -> Self {
        self.append(" ")
    }
}
