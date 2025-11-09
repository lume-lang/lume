use std::collections::{HashSet, VecDeque};
use std::fmt::Display;
use std::ops::Range;
use std::sync::Arc;

use lume_ast::{Node as _, *};
use lume_errors::{DiagCtx, Result};
use lume_span::SourceFile;
use serde::Deserialize;

#[derive(Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(default, deny_unknown_fields)]
pub struct Config {
    /// Defines the maximum line length, in characters.
    pub max_width: usize,

    /// Defines what indentation to use.
    #[serde(flatten)]
    pub indentation: Indentation,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            max_width: 120,
            indentation: Indentation::default(),
        }
    }
}

#[derive(Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Indentation {
    /// Defines whether to use tabs for identation (or spaces).
    pub use_tabs: bool,

    /// Amount of spaces to use per tab.
    pub tab_width: usize,
}

impl Indentation {
    #[inline]
    pub const fn width(self) -> usize {
        if self.use_tabs { 4 } else { self.tab_width }
    }
}

impl Default for Indentation {
    fn default() -> Self {
        Self {
            use_tabs: false,
            tab_width: 4,
        }
    }
}

impl Display for Indentation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.use_tabs {
            write!(f, "\t")
        } else {
            write!(f, "{: >n$}", "", n = self.tab_width)
        }
    }
}

pub fn format_src(content: &str, config: &Config) -> Result<String> {
    let document = parse_document(content)?;
    let root = Builder::build(content, document);

    let renderer = Renderer::new(config);
    let formatted = renderer.render(root);

    Ok(formatted)
}

#[derive(Default, Debug, Clone)]
struct Document<'src> {
    /// Defines all the top-level nodes within the original source file.
    pub items: Vec<TopLevelExpression>,

    /// Defines a list of all comments from the original source file, which
    /// were discarded during parsing.
    ///
    /// Each entry has a range, depicting the actual position of the comment in
    /// the original source file, and a slice of the comment (including
    /// backslashes).
    pub comments: Vec<(Range<usize>, &'src str)>,
}

fn parse_document(content: &str) -> Result<Document<'_>> {
    let dcx = DiagCtx::new();
    let source_file = Arc::new(SourceFile::internal(content));

    let mut tokens = lume_lexer::Lexer::lex_ref(content)?;

    let comments = tokens
        .extract_if(.., |token| token.as_type() == lume_lexer::TokenType::Comment)
        .map(|token| {
            let lume_lexer::TokenKind::Comment(content) = token.kind else {
                unreachable!()
            };

            (token.index, content)
        })
        .collect::<Vec<_>>();

    let items = dcx.with(|handle| {
        let mut parser = lume_parser::Parser::new(source_file, tokens, handle)?;
        parser.parse()
    })?;

    Ok(Document { items, comments })
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Node {
    pub kind: NodeKind,
    pub leading_comment: Option<String>,
    pub trailing_comment: Option<String>,
}

impl Node {
    #[inline]
    pub fn empty() -> Node {
        Self {
            kind: NodeKind::Empty,
            leading_comment: None,
            trailing_comment: None,
        }
    }

    #[inline]
    pub fn text(text: String) -> Node {
        Self {
            kind: NodeKind::Text(text),
            leading_comment: None,
            trailing_comment: None,
        }
    }

    #[inline]
    pub fn text_str(text: &str) -> Node {
        Self::text(text.into())
    }

    #[inline]
    pub fn line() -> Node {
        Self {
            kind: NodeKind::Line,
            leading_comment: None,
            trailing_comment: None,
        }
    }

    #[inline]
    pub fn space() -> Node {
        Self {
            kind: NodeKind::Space,
            leading_comment: None,
            trailing_comment: None,
        }
    }

    #[inline]
    pub fn space_or_line() -> Node {
        Self {
            kind: NodeKind::SpaceOrLine,
            leading_comment: None,
            trailing_comment: None,
        }
    }

    #[inline]
    pub fn empty_line() -> Node {
        Self {
            kind: NodeKind::EmptyLine,
            leading_comment: None,
            trailing_comment: None,
        }
    }

    #[inline]
    pub fn forced_line() -> Node {
        Self {
            kind: NodeKind::ForcedLine,
            leading_comment: None,
            trailing_comment: None,
        }
    }

    #[inline]
    pub fn nodes(nodes: Vec<Node>) -> Node {
        Self {
            kind: NodeKind::Nodes(nodes),
            leading_comment: None,
            trailing_comment: None,
        }
    }

    #[inline]
    pub fn indent(nodes: Vec<Node>) -> Node {
        Self {
            kind: NodeKind::Indent(nodes),
            leading_comment: None,
            trailing_comment: None,
        }
    }

    #[inline]
    pub fn forced_indent(nodes: Vec<Node>) -> Node {
        Self {
            kind: NodeKind::ForcedIndent(nodes),
            leading_comment: None,
            trailing_comment: None,
        }
    }

    #[inline]
    pub fn group(id: usize, nodes: Vec<Node>) -> Node {
        Self {
            kind: NodeKind::Group(id, nodes),
            leading_comment: None,
            trailing_comment: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum NodeKind {
    /// Empty node - prints nothing.
    Empty,

    /// A slice of UTF-8 text to display, such as `fn`, `return`, etc.
    Text(String),

    /// Renders a newline, if wrapping is needed. Otherwise, renders nothing.
    Line,

    /// Renders a single space, if wrapping is not needed. Otherwise, renders
    /// nothing.
    Space,

    /// Renders a space or a line, depending on whether the surrounding
    /// group needs to be wrapped.
    SpaceOrLine,

    /// Renders two newlines, so there's an empty line between lines.
    EmptyLine,

    /// Always renders a single newline.
    ForcedLine,

    /// A group of nodes where each node is indented, if wrapping is enabled.
    Indent(Vec<Node>),

    /// A group of nodes where each node must be indented.
    ForcedIndent(Vec<Node>),

    /// A group of nodes which we try to fit into a single line.
    Group(usize, Vec<Node>),

    /// A group of nodes without any special handling.
    Nodes(Vec<Node>),

    /// A conditional rendering node.
    IfWrap(usize, Box<Node>, Box<Node>),
}

struct Builder<'src> {
    source: Arc<SourceFile>,
    comments: VecDeque<(Range<usize>, &'src str)>,

    current_group: usize,
}

impl<'src> Builder<'src> {
    pub fn new(source: &'src str, mut comments: Vec<(Range<usize>, &'src str)>) -> Self {
        comments.sort_by_key(|c| c.0.start);

        Self {
            source: Arc::new(SourceFile::internal(source)),
            comments: comments.into_iter().collect(),
            current_group: 0,
        }
    }

    pub fn build_root(&mut self, items: Vec<TopLevelExpression>) -> Node {
        let items_len = items.len();
        let mut nodes = Vec::with_capacity(items.len());

        for (idx, item) in items.into_iter().enumerate() {
            nodes.push(self.build_item(item));

            if idx < items_len - 1 {
                nodes.push(Node::empty_line());
            }
        }

        Node::nodes(nodes)
    }

    pub fn build(source: &str, doc: Document) -> Node {
        let mut builder = Builder::new(source, doc.comments);

        builder.build_root(doc.items)
    }

    fn next_group(&mut self) -> usize {
        let id = self.current_group;
        self.current_group += 1;

        id
    }

    fn leading_comment(&mut self, span: &Location) -> Option<String> {
        if let Some((comment_span, _)) = self.comments.front()
            && comment_span.start < span.0.start
        {
            let (_, slice) = self.comments.pop_front().unwrap();

            return Some(slice.to_string());
        }

        None
    }

    fn trailing_comment(&mut self, span: &Location) -> Option<String> {
        let span_coords = lume_span::source::Location {
            file: self.source.clone(),
            index: span.0.clone(),
        };

        if let Some((comment_span, _)) = self.comments.front() {
            let comment_coords = lume_span::source::Location {
                file: self.source.clone(),
                index: comment_span.clone(),
            };

            if comment_coords.is_trailing(&span_coords) {
                let (_, slice) = self.comments.pop_front().unwrap();

                return Some(slice.to_string());
            }
        }

        None
    }

    /// Creates a group of nodes, possibly wrapping, depending on the
    /// surrounding group. In practice, it allows creating lists like this:
    ///
    /// ```ignore
    /// (Int8, Int16, Int32, Int64)
    /// ```
    /// if not wrapping. If the group is wrapping:
    ///
    /// ```ignore
    /// (
    ///     Int8,
    ///     Int16,
    ///     Int32,
    ///     Int64,
    /// )
    /// ```
    ///
    /// This can be used for parameter lists, arrays, blocks, etc.
    ///
    /// The method takes a list of items which should be contained within the
    /// group, as well as a closure for creating each node within the group.
    ///
    /// # Spacing
    ///
    /// When wrapping, there is at least one line between each node in the list.
    /// If two items in the given list have one-or-more lines between them,
    /// the output node will put a single line between them.
    fn idented<T: lume_ast::Node>(
        &mut self,
        items: Vec<T>,
        open: &str,
        close: &str,
        mut f: impl FnMut(&mut Builder<'src>, &mut Vec<Node>, T, usize),
    ) -> Node {
        if items.is_empty() {
            return Node::nodes(vec![Node::text_str(open), Node::space(), Node::text_str(close)]);
        }

        let mut nodes = vec![Node::text_str(open)];
        let mut group_items = Vec::with_capacity(items.len());

        let mut iter = items.into_iter().peekable();
        let group_id = self.next_group();

        while let Some(stmt) = iter.next() {
            let stmt_loc = lume_span::source::Location {
                file: self.source.clone(),
                index: stmt.location().0.clone(),
            };

            f(self, &mut group_items, stmt, group_id);

            if let Some(next) = iter.peek() {
                let next_loc = lume_span::source::Location {
                    file: self.source.clone(),
                    index: next.location().0.clone(),
                };

                let sep = if next_loc.coordinates().0 - stmt_loc.coordinates().0 > 1 {
                    Node::empty_line()
                } else {
                    Node::line()
                };

                group_items.push(sep);
            }
        }

        nodes.push(Node::line());
        nodes.push(Node::indent(group_items));
        nodes.push(Node::line());
        nodes.push(Node::text_str(close));

        Node::group(group_id, nodes)
    }

    /// Creates a group of nodes which will always wrap, unless there are no
    /// elements in the given list. For example, it can create nodes like:
    ///
    /// ```ignore
    /// { }
    /// ```
    ///
    /// ```ignore
    /// {
    ///     func();
    ///
    ///     let a = foo();
    ///     a.bar();
    /// }
    /// ```
    ///
    /// This can be used for parameter lists, arrays, blocks, etc.
    ///
    /// The method takes a list of items which should be contained within the
    /// group, as well as a closure for creating each node within the group.
    ///
    /// # Spacing
    ///
    /// When wrapping, there is at least one line between each node in the list.
    /// If two items in the given list have one-or-more lines between them,
    /// the output node will put a single line between them.
    fn idented_force<T: lume_ast::Node>(
        &mut self,
        items: Vec<T>,
        open: &str,
        close: &str,
        mut f: impl FnMut(&mut Builder<'src>, &mut Vec<Node>, T),
    ) -> Node {
        if items.is_empty() {
            return Node::nodes(vec![Node::text_str(open), Node::space(), Node::text_str(close)]);
        }

        let mut nodes = vec![Node::text_str(open), Node::forced_line()];
        let mut group_items = Vec::with_capacity(items.len());

        let mut iter = items.into_iter().peekable();

        while let Some(item) = iter.next() {
            let item_span = item.location().0.clone();

            f(self, &mut group_items, item);

            if let Some(next) = iter.peek() {
                let curr_loc = lume_span::source::Location {
                    file: self.source.clone(),
                    index: item_span,
                };

                let next_loc = lume_span::source::Location {
                    file: self.source.clone(),
                    index: next.location().0.clone(),
                };

                let sep = if next_loc.coordinates().0 - curr_loc.coordinates().0 > 1 {
                    Node::empty_line()
                } else {
                    Node::forced_line()
                };

                group_items.push(sep);
            }
        }

        nodes.push(Node::forced_indent(group_items));
        nodes.push(Node::forced_line());
        nodes.push(Node::text_str(close));

        Node::group(self.next_group(), nodes)
    }

    /// Creates a group of nodes, possibly wrapping, depending on the
    /// surrounding group. Between each node, there is a delimiter, defined in
    /// `delim`, In practice, it allows creating lists like this:
    ///
    /// ```ignore
    /// (Int8, Int16, Int32, Int64)
    /// ```
    /// if not wrapping. If the group is wrapping:
    ///
    /// ```ignore
    /// (
    ///     Int8,
    ///     Int16,
    ///     Int32,
    ///     Int64,
    /// )
    /// ```
    ///
    /// This can be used for parameter lists, arrays, blocks, etc.
    ///
    /// The method takes a list of items which should be contained within the
    /// group, as well as a closure for creating each node within the group.
    ///
    /// # Spacing
    ///
    /// When wrapping, there is at least one line between each node in the list.
    /// If two items in the given list have one-or-more lines between them,
    /// the output node will put a single line between them.
    fn idented_delimited<T: lume_ast::Node>(
        &mut self,
        items: Vec<T>,
        open: &str,
        close: &str,
        delim: &str,
        mut f: impl FnMut(&mut Builder<'src>, &mut Vec<Node>, T),
    ) -> Node {
        let len = items.len();
        let mut i = 0;

        self.idented(items, open, close, |builder, nodes, value, group| {
            f(builder, nodes, value);

            if i < len - 1 {
                nodes.push(Node::text_str(delim));
                nodes.push(Node::space());
            } else {
                nodes.push(Node {
                    kind: NodeKind::IfWrap(
                        group,
                        Box::new(Node::nodes(vec![Node::text_str(delim), Node::space()])),
                        Box::new(Node::empty()),
                    ),
                    leading_comment: None,
                    trailing_comment: None,
                });
            }

            i += 1;
        })
    }
}

impl Builder<'_> {
    fn build_item(&mut self, item: TopLevelExpression) -> Node {
        match item {
            TopLevelExpression::Import(item) => self.build_import(*item),
            TopLevelExpression::Namespace(item) => build_namespace(*item),
            TopLevelExpression::FunctionDefinition(item) => self.build_function_definition(*item),
            TopLevelExpression::TypeDefinition(type_def) => match *type_def {
                TypeDefinition::Struct(item) => self.build_struct_definition(*item),
                TypeDefinition::Trait(item) => self.build_trait_definition(*item),
                TypeDefinition::Enum(item) => self.build_enum_definition(*item),
            },
            TopLevelExpression::Impl(item) => self.build_implementation(*item),
            TopLevelExpression::TraitImpl(item) => self.build_trait_implementation(*item),
        }
    }

    fn build_import(&mut self, item: Import) -> Node {
        let mut nodes = vec![Node::text_str("import"), Node::space()];
        let path_len = item.path.path.len();

        for (idx, path) in item.path.path.into_iter().enumerate() {
            nodes.push(Node::text(path.name));

            if idx < path_len - 1 {
                nodes.push(Node::text_str("::"));
            }
        }

        nodes.push(Node::space());
        nodes.push(self.idented_delimited(item.names, "(", ")", ",", |_, nodes, name| {
            nodes.push(Node::text(name.name));
        }));

        Node::nodes(nodes)
    }

    fn build_function_definition(&mut self, item: FunctionDefinition) -> Node {
        let mut nodes = Vec::new();

        if let Some(doc_comment) = item.documentation {
            nodes.push(write_doc_comment(doc_comment));
        }

        nodes.push(build_visibility(item.visibility));
        nodes.push(Node::text_str("fn"));
        nodes.push(Node::space());

        if item.external {
            nodes.push(Node::text_str("external"));
            nodes.push(Node::space());
        }

        nodes.push(Node::text(item.name.name));

        nodes.push(self.build_type_parameters(item.type_parameters));
        nodes.push(self.build_parameter_list(item.parameters));

        if let Some(ret_ty) = item.return_type {
            nodes.push(Node::nodes(vec![
                Node::space(),
                Node::text_str("->"),
                Node::space(),
                self.build_type(*ret_ty),
            ]));
        }

        if let Some(block) = item.block {
            nodes.push(Node::space());
            nodes.push(self.build_block(block));
        }

        Node::nodes(nodes)
    }

    fn build_struct_definition(&mut self, item: StructDefinition) -> Node {
        let mut nodes = Vec::new();

        if let Some(doc_comment) = item.documentation {
            nodes.push(write_doc_comment(doc_comment));
        }

        nodes.push(build_visibility(item.visibility));
        nodes.push(Node::text_str("struct"));
        nodes.push(Node::space());

        if item.builtin {
            nodes.push(Node::text_str("builtin"));
            nodes.push(Node::space());
        }

        nodes.push(Node::text(item.name.name));
        nodes.push(self.build_type_parameters(item.type_parameters));
        nodes.push(Node::space());

        let fields = self.idented_force(item.fields, "{", "}", |builder, nodes, field| {
            if let Some(doc) = field.documentation {
                nodes.push(write_doc_comment(doc));
            }

            nodes.push(build_visibility(field.visibility));

            nodes.push(Node::text(field.name.name));
            nodes.push(Node::text_str(":"));
            nodes.push(Node::space());

            nodes.push(builder.build_type(field.field_type));

            if let Some(default_value) = field.default_value {
                nodes.push(Node::space());
                nodes.push(Node::text_str("="));
                nodes.push(Node::space());

                nodes.push(builder.build_expression(default_value));
            }

            nodes.push(Node::text_str(";"));
        });

        nodes.push(fields);

        Node::nodes(nodes)
    }

    fn build_trait_definition(&mut self, item: TraitDefinition) -> Node {
        let mut nodes = Vec::new();

        if let Some(doc_comment) = item.documentation {
            nodes.push(write_doc_comment(doc_comment));
        }

        nodes.push(build_visibility(item.visibility));
        nodes.push(Node::text_str("trait"));
        nodes.push(Node::space());

        nodes.push(Node::text(item.name.name));
        nodes.push(self.build_type_parameters(item.type_parameters));
        nodes.push(Node::space());

        let methods = self.idented_force(item.methods, "{", "}", |builder, nodes, method| {
            nodes.push(builder.build_trait_method_definition(method));
        });

        nodes.push(methods);

        Node::nodes(nodes)
    }

    fn build_trait_method_definition(&mut self, item: TraitMethodDefinition) -> Node {
        let mut nodes = Vec::new();

        if let Some(doc_comment) = item.documentation {
            nodes.push(write_doc_comment(doc_comment));
        }

        nodes.push(Node::text_str("fn"));
        nodes.push(Node::space());

        nodes.push(Node::text(item.name.name));

        nodes.push(self.build_type_parameters(item.type_parameters));
        nodes.push(self.build_parameter_list(item.parameters));

        if let Some(ret_ty) = item.return_type {
            nodes.push(Node::nodes(vec![
                Node::space(),
                Node::text_str("->"),
                Node::space(),
                self.build_type(*ret_ty),
            ]));
        }

        if let Some(block) = item.block {
            nodes.push(Node::space());
            nodes.push(self.build_block(block));
        } else {
            nodes.push(Node::text_str(";"));
        }

        Node::nodes(nodes)
    }

    fn build_enum_definition(&mut self, item: EnumDefinition) -> Node {
        let mut nodes = Vec::new();

        if let Some(doc_comment) = item.documentation {
            nodes.push(write_doc_comment(doc_comment));
        }

        nodes.push(build_visibility(item.visibility));
        nodes.push(Node::text_str("enum"));
        nodes.push(Node::space());

        nodes.push(Node::text(item.name.name));
        nodes.push(self.build_type_parameters(item.type_parameters));
        nodes.push(Node::space());

        let cases = self.idented_force(item.cases, "{", "}", |builder, nodes, case| {
            nodes.push(builder.build_enum_definition_case(case));
        });

        nodes.push(cases);

        Node::nodes(nodes)
    }

    fn build_enum_definition_case(&mut self, item: EnumDefinitionCase) -> Node {
        let mut nodes = Vec::new();

        if let Some(doc_comment) = item.documentation {
            nodes.push(write_doc_comment(doc_comment));
        }

        nodes.push(Node::text(item.name.name));

        if !item.parameters.is_empty() {
            let parameters = item.parameters.into_iter().map(|p| *p).collect();

            nodes.push(
                self.idented_delimited(parameters, "(", ")", ",", |builder, nodes, value| {
                    nodes.push(builder.build_type(value));
                }),
            );
        }

        nodes.push(Node::text_str(","));

        Node::nodes(nodes)
    }

    fn build_implementation(&mut self, item: Implementation) -> Node {
        let mut nodes = vec![
            Node::text_str("impl"),
            self.build_type_parameters(item.type_parameters),
            Node::space(),
            self.build_type(*item.name),
            Node::space(),
        ];

        let methods = self.idented_force(item.methods, "{", "}", |builder, nodes, method| {
            nodes.push(builder.build_method_definition(method));
        });

        nodes.push(methods);

        Node::nodes(nodes)
    }

    fn build_method_definition(&mut self, item: MethodDefinition) -> Node {
        let mut nodes = Vec::new();

        if let Some(doc_comment) = item.documentation {
            nodes.push(write_doc_comment(doc_comment));
        }

        nodes.push(Node::text_str("fn"));
        nodes.push(Node::space());

        if item.external {
            nodes.push(Node::text_str("external"));
            nodes.push(Node::space());
        }

        nodes.push(Node::text(item.name.name));
        nodes.push(self.build_type_parameters(item.type_parameters));
        nodes.push(self.build_parameter_list(item.parameters));

        if let Some(ret_ty) = item.return_type {
            nodes.push(Node::nodes(vec![
                Node::space(),
                Node::text_str("->"),
                Node::space(),
                self.build_type(*ret_ty),
            ]));
        }

        if let Some(block) = item.block {
            nodes.push(Node::space());
            nodes.push(self.build_block(block));
        }

        Node::nodes(nodes)
    }

    fn build_trait_implementation(&mut self, item: TraitImplementation) -> Node {
        let mut nodes = vec![
            Node::text_str("use"),
            self.build_type_parameters(item.type_parameters),
            Node::space(),
            self.build_type(*item.name),
            Node::space(),
            Node::text_str("in"),
            self.build_type(*item.target),
            Node::space(),
        ];

        let methods = self.idented_force(item.methods, "{", "}", |builder, nodes, method| {
            nodes.push(builder.build_trait_method_implementation(method));
        });

        nodes.push(methods);

        Node::nodes(nodes)
    }

    fn build_trait_method_implementation(&mut self, item: TraitMethodImplementation) -> Node {
        let mut nodes = Vec::new();

        nodes.push(Node::text_str("fn"));
        nodes.push(Node::space());

        if item.external {
            nodes.push(Node::text_str("external"));
            nodes.push(Node::space());
        }

        nodes.push(Node::text(item.name.name));
        nodes.push(self.build_type_parameters(item.type_parameters));
        nodes.push(self.build_parameter_list(item.parameters));

        if let Some(ret_ty) = item.return_type {
            nodes.push(Node::nodes(vec![
                Node::space(),
                Node::text_str("->"),
                Node::space(),
                self.build_type(*ret_ty),
            ]));
        }

        if let Some(block) = item.block {
            nodes.push(Node::space());
            nodes.push(self.build_block(block));
        }

        Node::nodes(nodes)
    }

    fn build_parameter_list(&mut self, params: Vec<Parameter>) -> Node {
        self.idented_delimited(params, "(", ")", ",", |builder, nodes, value| {
            nodes.push(builder.build_parameter(value));
        })
    }

    fn build_parameter(&mut self, param: Parameter) -> Node {
        let mut nodes = Vec::new();

        if param.param_type.is_self() {
            nodes.push(Node::text_str("self"));
        } else {
            if param.vararg {
                nodes.push(Node::text_str("..."));
            }

            nodes.push(Node::text(param.name.name));
            nodes.push(Node::text_str(":"));
            nodes.push(Node::space());
            nodes.push(self.build_type(param.param_type));
        }

        Node::nodes(nodes)
    }
}

fn build_namespace(item: Namespace) -> Node {
    let mut nodes = vec![Node::text_str("namespace"), Node::space()];
    let path_len = item.path.path.len();

    for (idx, path) in item.path.path.into_iter().enumerate() {
        nodes.push(Node::text(path.name));

        if idx < path_len - 1 {
            nodes.push(Node::text_str("::"));
        }
    }

    Node::nodes(nodes)
}

fn build_visibility(item: Option<Visibility>) -> Node {
    match item {
        Some(Visibility::Public { .. }) => Node::text_str("pub "),
        Some(Visibility::Internal { .. }) => Node::text_str("pub(internal) "),
        Some(Visibility::Private { .. }) => Node::text_str("priv "),
        None => Node::empty(),
    }
}

impl Builder<'_> {
    fn build_path(&mut self, item: Path) -> Node {
        let mut nodes = Vec::new();

        for segment in item.root {
            nodes.push(self.build_path_segment(segment));
            nodes.push(Node::text_str("::"));
        }

        nodes.push(self.build_path_segment(item.name));

        Node::nodes(nodes)
    }

    fn build_path_segment(&mut self, item: PathSegment) -> Node {
        match item {
            PathSegment::Namespace { name } | PathSegment::Variant { name, .. } => Node::text(name.name),
            PathSegment::Type {
                name, type_arguments, ..
            }
            | PathSegment::Callable {
                name, type_arguments, ..
            } => {
                if type_arguments.is_empty() {
                    Node::text(name.name)
                } else {
                    let mut nodes = vec![Node::text(name.name)];
                    nodes.extend(self.build_type_arguments(type_arguments));

                    Node::nodes(nodes)
                }
            }
        }
    }
}

impl Builder<'_> {
    fn build_pattern(&mut self, item: Pattern) -> Node {
        match item {
            Pattern::Literal(pat) => {
                let index = pat.location().0.clone();
                let slice = self.source.content.get(index).unwrap();

                Node::text(slice.to_string())
            }
            Pattern::Identifier(pat) => Node::text(pat.name),
            Pattern::Variant(pat) => {
                if pat.fields.is_empty() {
                    return self.build_path(pat.name);
                }

                let mut nodes = vec![self.build_path(pat.name), Node::text_str("("), Node::line()];
                let mut fields = Vec::with_capacity(pat.fields.len());

                for (idx, field) in pat.fields.into_iter().enumerate() {
                    let node = self.build_pattern(field);

                    if idx < fields.capacity() - 1 {
                        fields.push(Node::nodes(vec![node, Node::text_str(","), Node::space_or_line()]));
                    } else {
                        fields.push(node);
                    }
                }

                nodes.push(Node::indent(fields));
                nodes.push(Node::line());
                nodes.push(Node::text_str(")"));

                Node::group(self.next_group(), nodes)
            }
            Pattern::Wildcard(_) => Node::text_str(".."),
        }
    }
}

impl Builder<'_> {
    fn build_type(&mut self, ty: Type) -> Node {
        match ty {
            Type::Named(mut ty) => {
                if ty.name.type_arguments().is_empty() {
                    return Node::text(format!("{}", ty.name));
                }

                let type_args = ty.name.take_type_arguments();

                let mut nodes = vec![Node::text(format!("{}", ty.name))];
                nodes.extend(self.build_type_arguments(type_args));

                Node::group(self.next_group(), nodes)
            }
            Type::Array(ty) => Node::nodes(vec![
                Node::text_str("["),
                self.build_type(*ty.element_type),
                Node::text_str("]"),
            ]),
            Type::SelfType(_) => Node::text_str("self"),
        }
    }

    fn build_type_arguments(&mut self, type_args: Vec<Type>) -> Vec<Node> {
        let mut nodes = Vec::new();
        nodes.push(Node::text_str("<"));

        let type_arg_len = type_args.len();

        for (idx, type_arg) in type_args.into_iter().enumerate() {
            let node = self.build_type(type_arg);

            if idx < type_arg_len - 1 {
                nodes.push(Node::nodes(vec![node, Node::text_str(","), Node::space_or_line()]));
            } else {
                nodes.push(node);
            }
        }

        nodes.push(Node::text_str(">"));

        nodes
    }

    fn build_type_parameters(&mut self, type_params: Vec<TypeParameter>) -> Node {
        if type_params.is_empty() {
            return Node::empty();
        }

        self.idented_delimited(type_params, "<", ">", ",", |builder, nodes, value| {
            nodes.push(builder.build_type_parameter(value));
        })
    }

    fn build_type_parameter(&mut self, type_param: TypeParameter) -> Node {
        if type_param.constraints.is_empty() {
            return Node::text(type_param.name.name);
        }

        let mut nodes = vec![Node::text(type_param.name.name), Node::text_str(":"), Node::space()];
        let constraint_len = type_param.constraints.len();

        for (idx, constraint) in type_param.constraints.into_iter().enumerate() {
            let node = self.build_type(*constraint);

            if idx < constraint_len - 1 {
                nodes.push(Node::nodes(vec![
                    node,
                    Node::space(),
                    Node::text_str("+"),
                    Node::space(),
                ]));
            } else {
                nodes.push(node);
            }
        }

        Node::nodes(nodes)
    }
}

impl Builder<'_> {
    fn build_block(&mut self, block: Block) -> Node {
        self.idented_force(block.statements, "{", "}", |builder, nodes, stmt| {
            nodes.push(builder.build_statement(stmt));
        })
    }

    fn build_statement(&mut self, stmt: Statement) -> Node {
        match stmt {
            Statement::VariableDeclaration(decl) => {
                let mut nodes = vec![Node::text_str("let"), Node::space(), Node::text(decl.name.name)];

                if let Some(ty) = decl.variable_type {
                    nodes.push(Node::text_str(":"));
                    nodes.push(Node::space());
                    nodes.push(self.build_type(ty));
                }

                nodes.push(Node::space());
                nodes.push(Node::text_str("="));
                nodes.push(Node::space());
                nodes.push(self.build_expression(decl.value));
                nodes.push(Node::text_str(";"));

                Node::nodes(nodes)
            }
            Statement::Break(_) => Node::nodes(vec![Node::text_str("break"), Node::text_str(";")]),
            Statement::Continue(_) => Node::nodes(vec![Node::text_str("continue"), Node::text_str(";")]),
            Statement::Final(expr) => self.build_expression(expr.value),
            Statement::Return(ret) => {
                let mut nodes = vec![Node::text_str("return")];

                if let Some(value) = ret.value {
                    nodes.push(Node::space_or_line());
                    nodes.push(self.build_expression(value));
                }

                nodes.push(Node::text_str(";"));

                Node::nodes(nodes)
            }
            Statement::InfiniteLoop(stmt) => Node::nodes(vec![
                Node::text_str("loop"),
                Node::space(),
                self.build_block(stmt.block),
            ]),
            Statement::IteratorLoop(stmt) => Node::nodes(vec![
                Node::text_str("for"),
                Node::space(),
                Node::text(stmt.pattern.name),
                Node::space(),
                Node::text_str(" in "),
                Node::space(),
                self.build_expression(stmt.collection),
                self.build_block(stmt.block),
            ]),
            Statement::PredicateLoop(stmt) => Node::nodes(vec![
                Node::text_str("while"),
                Node::space(),
                self.build_expression(stmt.condition),
                Node::space(),
                self.build_block(stmt.block),
            ]),
            Statement::Expression(stmt) => {
                let needs_semi = !matches!(*stmt, Expression::Switch(_) | Expression::If(_));
                let expr = self.build_expression(*stmt);

                if needs_semi {
                    Node::nodes(vec![expr, Node::text_str(";")])
                } else {
                    expr
                }
            }
        }
    }
}

impl Builder<'_> {
    #[allow(clippy::too_many_lines)]
    fn build_expression(&mut self, expr: Expression) -> Node {
        let leading_comment = self.leading_comment(expr.location());
        let trailing_comment = self.trailing_comment(expr.location());

        let mut node = match expr {
            Expression::Array(expr) => self.idented_delimited(expr.values, "[", "]", ",", |builder, nodes, value| {
                nodes.push(builder.build_expression(value));
            }),
            Expression::Assignment(expr) => Node::nodes(vec![
                self.build_expression(expr.target),
                Node::space(),
                Node::text_str("="),
                Node::space(),
                self.build_expression(expr.value),
            ]),
            Expression::Binary(expr) => Node::nodes(vec![
                self.build_expression(expr.lhs),
                Node::space(),
                match expr.op.kind {
                    BinaryOperatorKind::And => Node::text_str("&"),
                    BinaryOperatorKind::Or => Node::text_str("|"),
                    BinaryOperatorKind::Xor => Node::text_str("^"),
                },
                Node::space(),
                self.build_expression(expr.rhs),
            ]),
            Expression::Call(expr) => {
                let mut nodes = Vec::new();

                if let Some(callee) = expr.callee {
                    nodes.push(self.build_expression(callee));
                    nodes.push(Node::text_str("."));
                }

                nodes.push(self.build_path(expr.name));
                nodes.push(Node::text_str("("));
                nodes.push(Node::line());

                let mut arg_list = Vec::with_capacity(expr.arguments.len());

                for (idx, arg) in expr.arguments.into_iter().enumerate() {
                    let node = self.build_expression(arg);

                    if idx < arg_list.capacity() - 1 {
                        arg_list.push(Node::nodes(vec![node, Node::text_str(","), Node::space_or_line()]));
                    } else {
                        arg_list.push(node);
                    }
                }

                nodes.push(Node::indent(arg_list));
                nodes.push(Node::line());
                nodes.push(Node::text_str(")"));

                Node::group(self.next_group(), nodes)
            }
            Expression::Cast(expr) => Node::nodes(vec![
                self.build_expression(expr.source),
                Node::space(),
                Node::text_str("as"),
                Node::space(),
                self.build_type(expr.target_type),
            ]),
            Expression::Construct(expr) => {
                let mut nodes = vec![
                    self.build_path(expr.path),
                    Node::space(),
                    Node::text_str("{"),
                    Node::space_or_line(),
                ];

                let mut fields = Vec::with_capacity(expr.fields.len());

                for (idx, field) in expr.fields.into_iter().enumerate() {
                    let node = if let Some(value) = field.value {
                        Node::nodes(vec![
                            Node::text(field.name.name),
                            Node::text_str(":"),
                            Node::space(),
                            self.build_expression(value),
                        ])
                    } else {
                        Node::text(field.name.name)
                    };

                    if idx < fields.capacity() - 1 {
                        fields.push(Node::nodes(vec![node, Node::text_str(","), Node::space_or_line()]));
                    } else {
                        fields.push(node);
                    }
                }

                nodes.push(Node::indent(fields));
                nodes.push(Node::space_or_line());
                nodes.push(Node::text_str("}"));

                Node::group(self.next_group(), nodes)
            }
            Expression::If(expr) => {
                let mut cases = Vec::new();

                for (idx, case) in expr.cases.into_iter().enumerate() {
                    let node = if idx == 0 {
                        Node::nodes(vec![
                            Node::text_str("if"),
                            Node::space(),
                            self.build_expression(case.condition.unwrap()),
                            Node::space(),
                            self.build_block(case.block),
                        ])
                    } else if let Some(condition) = case.condition {
                        Node::nodes(vec![
                            Node::space(),
                            Node::text_str("else"),
                            Node::space(),
                            Node::text_str("if"),
                            Node::space(),
                            self.build_expression(condition),
                            Node::space(),
                            self.build_block(case.block),
                        ])
                    } else {
                        Node::nodes(vec![
                            Node::space(),
                            Node::text_str("else"),
                            Node::space(),
                            self.build_block(case.block),
                        ])
                    };

                    cases.push(node);
                }

                Node::group(self.next_group(), cases)
            }
            Expression::IntrinsicCall(mut expr) => {
                if let Some(rhs) = expr.arguments.pop() {
                    Node::nodes(vec![
                        self.build_expression(expr.callee),
                        Node::space(),
                        self.build_path(expr.name),
                        Node::space(),
                        self.build_expression(rhs),
                    ])
                } else {
                    Node::nodes(vec![self.build_path(expr.name), self.build_expression(expr.callee)])
                }
            }
            Expression::Is(expr) => Node::nodes(vec![
                self.build_expression(expr.target),
                Node::space(),
                Node::text_str("is"),
                Node::space(),
                self.build_pattern(expr.pattern),
            ]),
            Expression::Literal(expr) => {
                let index = expr.location().0.clone();
                let slice = self.source.content.get(index).unwrap();

                Node::text_str(slice)
            }
            Expression::Logical(expr) => {
                let lhs = self.build_expression(expr.lhs);
                let rhs = self.build_expression(expr.rhs);

                let op = match expr.op.kind {
                    LogicalOperatorKind::And => Node::text_str("&&"),
                    LogicalOperatorKind::Or => Node::text_str("||"),
                };

                Node::group(self.next_group(), vec![lhs, Node::space(), op, Node::space(), rhs])
            }
            Expression::Member(expr) => Node::nodes(vec![
                self.build_expression(expr.callee),
                Node::text_str("."),
                Node::text(expr.name),
            ]),
            Expression::Range(expr) => {
                if expr.inclusive {
                    Node::nodes(vec![
                        self.build_expression(expr.lower),
                        Node::text_str(".."),
                        Node::text_str("="),
                        self.build_expression(expr.upper),
                    ])
                } else {
                    Node::nodes(vec![
                        self.build_expression(expr.lower),
                        Node::text_str(".."),
                        self.build_expression(expr.upper),
                    ])
                }
            }
            Expression::Scope(expr) => self.idented_force(expr.body, "{", "}", |builder, nodes, stmt| {
                nodes.push(builder.build_statement(stmt));
            }),
            Expression::Switch(expr) => {
                let mut nodes = vec![
                    Node::text_str("switch"),
                    Node::space(),
                    self.build_expression(expr.operand),
                    Node::space(),
                    Node::text_str("{"),
                    Node::forced_line(),
                ];

                let mut cases = Vec::with_capacity(expr.cases.len());
                let mut iter = expr.cases.into_iter().peekable();

                while let Some(case) = iter.next() {
                    let stmt_index = case.location().0.clone();

                    let node = Node::nodes(vec![
                        self.build_pattern(case.pattern),
                        Node::space(),
                        Node::text_str("=>"),
                        Node::space(),
                        self.build_expression(case.branch),
                        Node::text_str(","),
                    ]);

                    cases.push(node);

                    if let Some(next) = iter.peek() {
                        let stmt_loc = lume_span::source::Location {
                            file: self.source.clone(),
                            index: stmt_index,
                        };

                        let next_loc = lume_span::source::Location {
                            file: self.source.clone(),
                            index: next.location().0.clone(),
                        };

                        let sep = if next_loc.coordinates().0 - stmt_loc.coordinates().0 > 1 {
                            Node::empty_line()
                        } else {
                            Node::forced_line()
                        };

                        cases.push(sep);
                    }
                }

                nodes.push(Node::forced_indent(cases));
                nodes.push(Node::forced_line());
                nodes.push(Node::text_str("}"));

                Node::nodes(nodes)
            }
            Expression::Variable(expr) => Node::text(expr.name.name),
            Expression::Variant(expr) => {
                if expr.arguments.is_empty() {
                    return self.build_path(expr.name);
                }

                let mut nodes = vec![self.build_path(expr.name), Node::text_str("("), Node::line()];
                let mut arg_list = Vec::with_capacity(expr.arguments.len());

                for (idx, arg) in expr.arguments.into_iter().enumerate() {
                    let node = self.build_expression(arg);

                    if idx < arg_list.capacity() - 1 {
                        arg_list.push(Node::nodes(vec![node, Node::text_str(","), Node::space_or_line()]));
                    } else {
                        arg_list.push(node);
                    }
                }

                nodes.push(Node::indent(arg_list));
                nodes.push(Node::line());
                nodes.push(Node::text_str(")"));

                Node::group(self.next_group(), nodes)
            }
        };

        node.leading_comment = leading_comment;
        node.trailing_comment = trailing_comment;

        node
    }
}

fn write_doc_comment(doc: String) -> Node {
    let mut nodes = Vec::new();

    for line in doc.lines() {
        nodes.push(Node::text_str("/// "));
        nodes.push(Node::text_str(line.trim()));
        nodes.push(Node::forced_line());
    }

    Node::nodes(nodes)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Wrap {
    Enabled,
    Detect,
}

impl Wrap {
    pub fn enabled(self) -> bool {
        self == Wrap::Enabled
    }
}

struct Renderer<'cfg> {
    config: &'cfg Config,

    buf: String,
    indent: usize,
    line_size: usize,

    wrapped: HashSet<usize>,
}

impl<'cfg> Renderer<'cfg> {
    pub fn new(config: &'cfg Config) -> Self {
        Self {
            config,
            buf: String::new(),
            indent: 0,
            line_size: 0,
            wrapped: HashSet::new(),
        }
    }

    pub fn render(mut self, root: Node) -> String {
        self.node(root, Wrap::Enabled);

        self.buf
    }
}

impl Renderer<'_> {
    fn node(&mut self, node: Node, wrap: Wrap) {
        match node.kind {
            NodeKind::Empty => {}
            NodeKind::Text(text) => {
                self.text(&text);
            }
            NodeKind::Line => {
                if wrap.enabled() {
                    self.new_line();
                }
            }
            NodeKind::Space => self.space(),
            NodeKind::SpaceOrLine => {
                if wrap.enabled() {
                    self.new_line();
                } else {
                    self.space();
                }
            }
            NodeKind::ForcedLine => {
                self.new_line();
            }
            NodeKind::EmptyLine => {
                self.new_line();
                self.new_line();
            }
            NodeKind::Indent(nodes) => {
                if wrap.enabled() {
                    self.indent += 1;
                    self.line_size += self.config.indentation.width();
                    self.buf.push_str(&self.config.indentation.to_string());

                    for node in nodes {
                        self.node(node, wrap);
                    }

                    self.indent -= 1;
                } else {
                    for node in nodes {
                        self.node(node, wrap);
                    }
                }
            }
            NodeKind::ForcedIndent(nodes) => {
                self.indent += 1;
                self.line_size += self.config.indentation.width();
                self.buf.push_str(&self.config.indentation.to_string());

                for node in nodes {
                    self.node(node, wrap);
                }

                self.indent -= 1;
            }
            NodeKind::Group(group, nodes) => {
                let width: usize = nodes.iter().map(|n| self.width_of(n)).sum();

                let wrap = if self.line_size + width > self.config.max_width {
                    self.wrapped.insert(group);

                    Wrap::Enabled
                } else {
                    Wrap::Detect
                };

                for node in nodes {
                    self.node(node, wrap);
                }
            }
            NodeKind::Nodes(nodes) => {
                for node in nodes {
                    self.node(node, wrap);
                }
            }
            NodeKind::IfWrap(group, a, b) => {
                if self.wrapped.contains(&group) {
                    self.node(*a, wrap);
                } else {
                    self.node(*b, wrap);
                }
            }
        }
    }

    fn text(&mut self, text: &str) {
        self.line_size += text.len();
        self.buf.push_str(text);
    }

    fn new_line(&mut self) {
        self.line_size = self.config.indentation.width() * self.indent;
        self.buf.push('\n');

        for _ in 0..self.indent {
            self.buf.push_str(&self.config.indentation.to_string());
        }
    }

    #[inline]
    fn space(&mut self) {
        self.line_size += 1;
        self.buf.push(' ');
    }

    fn width_of(&self, node: &Node) -> usize {
        match &node.kind {
            NodeKind::Nodes(n) | NodeKind::Group(_, n) | NodeKind::Indent(n) | NodeKind::ForcedIndent(n) => {
                n.iter().map(|n| self.width_of(n)).sum()
            }
            NodeKind::Text(v) => {
                if v.contains('\n') {
                    v.lines().map(|l| l.len()).max().unwrap_or(v.len())
                } else {
                    v.len()
                }
            }
            NodeKind::IfWrap(group, a, b) => {
                if self.wrapped.contains(group) {
                    self.width_of(a)
                } else {
                    self.width_of(b)
                }
            }
            NodeKind::Space | NodeKind::SpaceOrLine => 1,
            NodeKind::Empty | NodeKind::Line | NodeKind::EmptyLine | NodeKind::ForcedLine => 0,
        }
    }
}
