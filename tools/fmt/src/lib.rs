//! Source code for the Lume formatter.
//!
//! This formatter is heavily based on the Gleam code formatter, found within
//! their GitHub repository: <https://github.com/gleam-lang/gleam/blob/main/compiler-core/src/format.rs>

pub(crate) mod doc;
pub(crate) mod print;
pub(crate) mod wrap;

use std::collections::VecDeque;
use std::ops::Range;
use std::sync::Arc;

use iter_tools::Itertools;
use lume_ast::*;
use lume_errors::{DiagCtxHandle, MapDiagnostic};
use lume_parser::Parser;
use lume_span::SourceFile;
use serde::Deserialize;

use crate::doc::*;
use crate::wrap::wrap_text_block;

#[derive(Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(default, deny_unknown_fields)]
pub struct Config {
    /// Defines the maximum line length, in characters.
    pub max_width: usize,

    /// Defines what indentation to use.
    #[serde(flatten)]
    pub indentation: Indentation,

    /// Defines whether doc-comments should be wrapped.
    pub wrap_comments: bool,

    /// If wrapping of comments is enabled, defines the maximum line length.
    ///
    /// If not specified, uses `max_width`.
    pub max_comment_width: Option<usize>,

    /// Defines whether to add an empty trailing line at the bottom of every
    /// file.
    pub trailing_line: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            max_width: 120,
            indentation: Indentation::default(),
            wrap_comments: true,
            max_comment_width: None,
            trailing_line: true,
        }
    }
}

#[derive(Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
#[serde(default)]
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

pub fn format_src(content: &str, config: &Config, dcx: DiagCtxHandle) -> lume_errors::Result<String> {
    let source = parse_source(content, dcx)?;
    let formatted = Formatter::new(config).source(&source).print(config).map_diagnostic()?;

    Ok(formatted)
}

#[derive(Debug, Clone)]
struct Source<'src> {
    /// Defines the content of the original source file.
    pub file: Arc<SourceFile>,

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

fn parse_source(content: &str, dcx: DiagCtxHandle) -> lume_errors::Result<Source<'_>> {
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

    let mut parser = Parser::new(source_file.clone(), tokens, dcx.handle());
    let items = parser.parse()?;

    dcx.ensure_untainted()?;

    Ok(Source {
        file: source_file,
        items,
        comments,
    })
}

#[derive(Default, Debug, Clone)]
struct Comments<'src> {
    /// Defines whether there should be a newline between the comment block and
    /// any following documents.
    pub trailing_newline: bool,

    /// Defines all the comment lines within the comment block. All lines within
    /// the block already have their corresponding `//` or `///` prefix
    /// attached.
    pub lines: Vec<(Range<usize>, &'src str)>,
}

pub struct Formatter<'cfg, 'src> {
    config: &'cfg Config,

    /// Defines the content of the original source file being formatted.
    pub source_file: Arc<SourceFile>,

    /// Defines a list of all comments from the original source file, which
    /// were discarded during parsing.
    ///
    /// Each entry has a range, depicting the actual position of the comment in
    /// the original source file, and a slice of the comment (including
    /// backslashes).
    pub comments: VecDeque<(Range<usize>, &'src str)>,
}

impl<'cfg, 'src> Formatter<'cfg, 'src> {
    pub fn new(config: &'cfg Config) -> Self {
        Self {
            config,
            source_file: Arc::new(SourceFile::empty()),
            comments: VecDeque::new(),
        }
    }

    /// Gets the current indentation as a [`isize`].
    #[inline]
    fn indent(&self) -> isize {
        self.config.indentation.width().cast_signed()
    }

    fn source<'a>(mut self, source: &'a Source<'src>) -> Document<'a> {
        let Source {
            file: source_file,
            items,
            comments,
        } = source;

        self.source_file = source_file.clone();
        self.comments = comments
            .iter()
            .map(|(span, content)| (span.clone(), *content))
            .collect();

        self.with_spacing(items.iter(), |fmt, item| fmt.top_level_expression(item))
    }

    /// Determines whether any current comments exist at or before the given
    /// character index.
    fn any_comments_before(&self, idx: usize) -> bool {
        self.comments.iter().any(|(span, _)| span.start <= idx)
    }

    fn pop_comment_if<P: FnOnce(&Range<usize>, &str) -> bool>(
        &mut self,
        predicate: P,
    ) -> Option<(Range<usize>, &'src str)> {
        let (span, comment) = self.comments.front()?;

        if predicate(span, comment) {
            self.comments.pop_front()
        } else {
            None
        }
    }

    /// Pops off all comments which start at or before the given character index
    /// and return them.
    fn pop_comments_before(&mut self, idx: usize) -> Comments<'src> {
        let mut comments = Vec::new();
        while let Some(comment) = self.pop_comment_if(|span, _comment| span.start <= idx) {
            comments.push(comment);
        }

        let Some(last) = comments.last() else {
            return Comments::default();
        };

        let (comment_line, _) = self.coordinates_of(last.0.end);
        let (index_line, _) = self.coordinates_of(idx);

        let trailing_newline = index_line.saturating_sub(comment_line) > 1;

        Comments {
            trailing_newline,
            lines: comments,
        }
    }

    /// Pops off the first comment which start at the same line as the given
    /// character index and returns it. If no comments were found on the line,
    /// returns [`None`].
    fn pop_comments_on_same_line(&mut self, idx: usize) -> Option<&'src str> {
        let (idx_line, _) = self.coordinates_of(idx);

        if let Some((span, _content)) = self.comments.front() {
            let (comment_line, _) = self.coordinates_of(span.start);

            if comment_line == idx_line {
                let (_span, comment) = self.comments.pop_front().unwrap();
                return Some(comment);
            }
        }

        None
    }

    fn doc_comment<'a>(&self, doc: &str) -> Document<'a> {
        let max_comment_width = self.config.max_comment_width.unwrap_or(self.config.max_width);
        let mut lines = Vec::new();

        let wrapped_line_str = if self.config.wrap_comments {
            wrap_text_block(String::from(doc), max_comment_width)
        } else {
            doc.to_string()
        };

        for doc_line in wrapped_line_str.lines() {
            if doc_line.trim().is_empty() {
                lines.push("///".as_doc().append(line()));
            } else {
                lines.push("/// ".as_doc().append(doc_line.trim_end().to_string()).append(line()));
            }
        }

        concat(lines)
    }

    /// Gets the file coordinates of the given character index in the current
    /// source file.
    ///
    /// The returned tuple has the coordinates as `(line, column)`, both being
    /// zero-indexed. If the given index is outside of the span of the file,
    /// `(0, 0)` is returned.
    fn coordinates_of(&self, idx: usize) -> (usize, usize) {
        if idx > self.source_file.content.len() {
            return (0, 0);
        }

        let src = &self.source_file.content;

        let mut line = 0;
        let mut col = 0;

        for (i, c) in src.char_indices() {
            if i == idx {
                return (line, col);
            }

            if c == '\n' {
                col = 0;
                line += 1;
            } else {
                col += 1;
            }
        }

        (line, col)
    }

    fn with_spacing<'a, T, I, F>(&mut self, iter: I, mut f: F) -> Document<'a>
    where
        T: lume_ast::Node + 'a,
        I: Iterator<Item = &'a T>,
        F: FnMut(&mut Self, &'a T) -> Document<'a>,
    {
        let mut prev_line = 0;
        let mut items = Vec::<Document<'a>>::new();

        for (idx, item) in iter.enumerate() {
            let (curr_line, _) = self.coordinates_of(item.location().start());

            if idx != 0 && curr_line.saturating_sub(prev_line) > 1 {
                items.push(lines(2));
            } else if idx != 0 {
                items.push(line());
            }

            (prev_line, _) = self.coordinates_of(item.location().end());
            items.push(f(self, item).group());
        }

        items.as_doc()
    }

    fn path<'a>(&mut self, path: &'a Path) -> Document<'a> {
        let head = self.path_segment(&path.name);
        let root = path.root.iter().map(|seg| self.path_segment(seg)).collect::<Vec<_>>();

        if root.is_empty() {
            head
        } else {
            join(root, "::").append("::").append(head)
        }
    }

    fn path_segment<'a>(&mut self, segment: &'a PathSegment) -> Document<'a> {
        match segment {
            PathSegment::Namespace { name } | PathSegment::Variant { name, .. } => str(name.as_str()),
            PathSegment::Type { name, bound_types, .. } | PathSegment::Callable { name, bound_types, .. } => {
                let name = str(name.as_str());

                if bound_types.is_empty() {
                    name
                } else {
                    name.append(self.type_arguments(bound_types))
                }
            }
        }
    }

    fn top_level_expression<'a>(&mut self, item: &'a TopLevelExpression) -> Document<'a> {
        match item {
            TopLevelExpression::Namespace(ns) => namespace(ns),
            TopLevelExpression::Import(import) => self.import(import),
            TopLevelExpression::FunctionDefinition(func) => self.function_definition(func),
            TopLevelExpression::StructDefinition(struct_def) => self.struct_definition(struct_def),
            TopLevelExpression::TraitDefinition(trait_def) => self.trait_definition(trait_def),
            TopLevelExpression::EnumDefinition(enum_def) => self.enum_definition(enum_def),
            TopLevelExpression::TraitImplementation(trait_impl) => self.trait_implementation(trait_impl),
            TopLevelExpression::Implementation(implementation) => self.implementation(implementation),
        }
    }

    fn import<'a>(&mut self, import: &'a Import) -> Document<'a> {
        let segments = import.path.path.iter().map(|seg| str(seg.as_str())).collect_vec();
        let root = join(segments, "::");

        let import_name_docs = import.names.iter().map(|seg| str(seg.as_str())).collect_vec();

        let nested_names = flex_break("", "")
            .append(join(import_name_docs, flex_break(",", ", ")))
            .group()
            .nest_if_broken(self.indent());

        let wrapped_names = str("(")
            .append(nested_names)
            .append(concat([flex_break(",", ""), str(")")]));

        str("import ").append(root).space().append(wrapped_names)
    }

    fn function_definition<'a>(&mut self, func: &'a FunctionDefinition) -> Document<'a> {
        let doc_comment = match &func.documentation {
            Some(doc) => self.doc_comment(doc),
            None => empty(),
        };

        let signature = doc_comment
            .append(visibility(func.visibility.as_ref()))
            .append("fn ")
            .append(if func.external { "external " } else { "" })
            .append(func.name.as_str())
            .append(self.type_parameters(&func.type_parameters))
            .append(self.parameters(&func.parameters));

        let signature = match &func.return_type {
            Some(ret_ty) => signature.append(" -> ").append(self.ty(ret_ty)),
            None => signature,
        }
        .group();

        let body = match &func.block {
            Some(block) => str(" ").append(self.block(block)),
            None => empty(),
        };

        signature.append(body)
    }

    fn struct_definition<'a>(&mut self, def: &'a StructDefinition) -> Document<'a> {
        let doc_comment = match &def.documentation {
            Some(doc) => self.doc_comment(doc),
            None => empty(),
        };

        let attributes = self.attributes(&def.attributes);

        let header = doc_comment
            .append(attributes)
            .append(visibility(def.visibility.as_ref()))
            .append("struct ")
            .append(if def.builtin { str("builtin ") } else { empty() })
            .append(def.name.as_str())
            .append(self.type_parameters(&def.type_parameters))
            .space();

        let mut prev_line = 0;
        let mut fields = Vec::with_capacity(def.fields.len());

        for (idx, field) in def.fields.iter().enumerate() {
            let (curr_line, _) = self.coordinates_of(field.location().start());

            if idx != 0 && curr_line.saturating_sub(prev_line) > 1 {
                fields.push(lines(2));
            } else if idx != 0 {
                fields.push(line());
            }

            (prev_line, _) = self.coordinates_of(field.location().end());

            let doc_comment = match &field.documentation {
                Some(doc) => self.doc_comment(doc),
                None => empty(),
            };

            let visibility = visibility(field.visibility.as_ref());
            let field_type = self.ty(&field.field_type);
            let default_value = match &field.default_value {
                Some(val) => str(" = ").append(self.expression(val)),
                None => empty(),
            };

            let field_doc = doc_comment
                .append(visibility)
                .append(field.name.as_str())
                .append(": ")
                .append(field_type)
                .append(default_value)
                .append(";");

            fields.push(field_doc);
        }

        if fields.is_empty() {
            return header.append("{}");
        }

        let body = str("{")
            .append(line().append(concat(fields)).nest(self.indent()).group())
            .append(line())
            .append("}");

        header.append(body)
    }

    fn trait_definition<'a>(&mut self, def: &'a TraitDefinition) -> Document<'a> {
        let doc_comment = match &def.documentation {
            Some(doc) => self.doc_comment(doc),
            None => empty(),
        };

        let attributes = self.attributes(&def.attributes);

        let header = doc_comment
            .append(attributes)
            .append(visibility(def.visibility.as_ref()))
            .append("trait ")
            .append(def.name.as_str())
            .append(self.type_parameters(&def.type_parameters))
            .space();

        if def.methods.is_empty() {
            return header.append("{}");
        }

        let methods = self.with_spacing(def.methods.iter(), |fmt, method| fmt.trait_method_definition(method));

        let body = str("{")
            .append(line().append(methods).nest(self.indent()).group())
            .append(line())
            .append("}");

        header.append(body)
    }

    fn trait_method_definition<'a>(&mut self, method: &'a TraitMethodDefinition) -> Document<'a> {
        let doc_comment = match &method.documentation {
            Some(doc) => self.doc_comment(doc),
            None => empty(),
        };

        let signature = doc_comment
            .append("fn ")
            .append(method.name.as_str())
            .append(self.type_parameters(&method.type_parameters))
            .append(self.parameters(&method.parameters));

        let signature = match &method.return_type {
            Some(ret_ty) => signature.append(" -> ").append(self.ty(ret_ty)),
            None => signature,
        }
        .group();

        let body = match &method.block {
            Some(block) => str(" ").append(self.block(block)),
            None => str(";"),
        };

        signature.append(body)
    }

    fn enum_definition<'a>(&mut self, def: &'a EnumDefinition) -> Document<'a> {
        let doc_comment = match &def.documentation {
            Some(doc) => self.doc_comment(doc),
            None => empty(),
        };

        let header = doc_comment
            .append(visibility(def.visibility.as_ref()))
            .append("enum ")
            .append(def.name.as_str())
            .append(self.type_parameters(&def.type_parameters))
            .space();

        if def.cases.is_empty() {
            return header.append("{}");
        }

        let variants = self.with_spacing(def.cases.iter(), |fmt, variant| fmt.enum_variant_definition(variant));

        let body = str("{")
            .append(line().append(variants).nest(self.indent()).group())
            .append(line())
            .append("}");

        header.append(body)
    }

    fn enum_variant_definition<'a>(&mut self, def: &'a EnumDefinitionCase) -> Document<'a> {
        let doc_comment = match &def.documentation {
            Some(doc) => self.doc_comment(doc),
            None => empty(),
        };

        let identifier = doc_comment.append(def.name.as_str());
        let fields = def.parameters.iter().map(|param| self.ty(param)).collect_vec();

        if fields.is_empty() {
            return identifier;
        }

        identifier
            .append(self.wrap_comma_separated_of("(", ")", fields))
            .append(",")
    }

    fn trait_implementation<'a>(&mut self, trait_impl: &'a TraitImplementation) -> Document<'a> {
        let header = str("use")
            .append(self.type_parameters(&trait_impl.type_parameters))
            .space()
            .append(self.ty(&trait_impl.name))
            .append(" in ")
            .append(self.ty(&trait_impl.target))
            .space();

        if trait_impl.methods.is_empty() {
            return header.append("{}");
        }

        let methods = self.with_spacing(trait_impl.methods.iter(), |fmt, method| {
            fmt.trait_method_implementation(method)
        });

        let body = str("{")
            .append(line().append(methods).nest(self.indent()).group())
            .append(line())
            .append("}");

        header.append(body)
    }

    fn trait_method_implementation<'a>(&mut self, method: &'a TraitMethodImplementation) -> Document<'a> {
        let signature = str("fn ")
            .append(if method.external { "external " } else { "" })
            .append(method.name.as_str())
            .append(self.type_parameters(&method.type_parameters))
            .append(self.parameters(&method.parameters));

        let signature = match &method.return_type {
            Some(ret_ty) => signature.append(" -> ").append(self.ty(ret_ty)),
            None => signature,
        }
        .group();

        let body = match &method.block {
            Some(block) => str(" ").append(self.block(block)),
            None => empty(),
        };

        signature.append(body)
    }

    fn implementation<'a>(&mut self, implementation: &'a Implementation) -> Document<'a> {
        let header = str("impl")
            .append(self.type_parameters(&implementation.type_parameters))
            .space()
            .append(self.ty(&implementation.name))
            .space();

        if implementation.methods.is_empty() {
            return header.append("{}");
        }

        let methods = self.with_spacing(implementation.methods.iter(), |fmt, method| {
            fmt.method_implementation(method)
        });

        let body = str("{")
            .append(line().append(methods).nest(self.indent()).group())
            .append(line())
            .append("}");

        header.append(body)
    }

    fn method_implementation<'a>(&mut self, method: &'a MethodDefinition) -> Document<'a> {
        let doc_comment = match &method.documentation {
            Some(doc) => self.doc_comment(doc),
            None => empty(),
        };

        let attributes = self.attributes(&method.attributes);

        let signature = doc_comment
            .append(attributes)
            .append(visibility(method.visibility.as_ref()))
            .append(str("fn "))
            .append(if method.external { "external " } else { "" })
            .append(method.name.as_str())
            .append(self.type_parameters(&method.type_parameters))
            .append(self.parameters(&method.parameters));

        let signature = match &method.return_type {
            Some(ret_ty) => signature.append(" -> ").append(self.ty(ret_ty)),
            None => signature,
        }
        .group();

        let body = match &method.block {
            Some(block) => str(" ").append(self.block(block)),
            None => empty(),
        };

        signature.append(body)
    }

    fn attributes<'a>(&mut self, attrs: &'a [Attribute]) -> Document<'a> {
        if attrs.is_empty() {
            return empty();
        }

        concat(attrs.iter().map(|attr| self.attribute(attr)).collect_vec())
    }

    fn attribute<'a>(&mut self, attr: &'a Attribute) -> Document<'a> {
        let name = str(attr.name.as_str());

        let mut arguments = Vec::with_capacity(attr.arguments.len());
        for arg in &attr.arguments {
            let name = str(arg.key.as_str());
            let value = self.literal(&arg.value);

            arguments.push(name.append(" = ").append(value));
        }

        if arguments.is_empty() {
            str("![").append(name).append("]").append(line())
        } else {
            str("![")
                .append(name)
                .append("(")
                .append(join(arguments, ", "))
                .append(")]")
                .append(line())
        }
    }

    fn parameters<'a>(&mut self, params: &'a [Parameter]) -> Document<'a> {
        if params.is_empty() {
            return str("()");
        }

        let parameters = params.iter().map(|param| self.parameter(param)).collect_vec();
        self.wrap_comma_separated_of("(", ")", parameters)
    }

    fn parameter<'a>(&mut self, param: &'a Parameter) -> Document<'a> {
        if param.name.as_str() == "self" {
            return str("self");
        }

        if param.vararg { str("...") } else { empty() }
            .append(param.name.as_str())
            .append(": ")
            .append(self.ty(&param.param_type))
            .group()
    }

    fn type_parameters<'a>(&mut self, params: &'a [TypeParameter]) -> Document<'a> {
        if params.is_empty() {
            return empty();
        }

        let type_params = params
            .iter()
            .map(|type_param| self.type_parameter(type_param))
            .collect_vec();

        self.wrap_comma_separated_of("<", ">", type_params)
    }

    fn type_parameter<'a>(&mut self, param: &'a TypeParameter) -> Document<'a> {
        let name = str(param.name.as_str());
        let constraints = param
            .constraints
            .iter()
            .map(|constraint| self.ty(constraint))
            .collect_vec();

        if constraints.is_empty() {
            name
        } else {
            name.append(": ").append(join(constraints, " + "))
        }
    }

    fn block<'a>(&mut self, block: &'a Block) -> Document<'a> {
        self.statements(&block.statements, &block.location)
    }

    fn statements<'a>(&mut self, body: &'a [Statement], loc: &Location) -> Document<'a> {
        if body.is_empty() && !self.any_comments_before(loc.end()) {
            return str("{}");
        }

        let body = self.with_spacing(body.iter(), |fmt, stmt| fmt.statement(stmt));
        let body = match printed_comments(self.pop_comments_before(loc.end()), true) {
            Some(comments) => body.append(line()).append(comments),
            None => body,
        };

        str("{")
            .append(line().append(body).nest(self.indent()).group())
            .append(line())
            .append("}")
    }

    fn statement<'a>(&mut self, stmt: &'a Statement) -> Document<'a> {
        let comments = self.pop_comments_before(stmt.location().start());
        let doc = match stmt {
            Statement::VariableDeclaration(stmt) => {
                let name = str(stmt.name.as_str());
                let declared_type = match &stmt.variable_type {
                    Some(ty) => str(": ").append(self.ty(ty)),
                    None => empty(),
                };

                let value = self.expression(&stmt.value);

                str("let ")
                    .append(name)
                    .append(declared_type)
                    .append(" = ")
                    .append(value.group())
                    .append(";")
            }
            Statement::Break(_) => str("break").append(';'),
            Statement::Continue(_) => str("continue").append(';'),
            Statement::Final(stmt) => self.expression(&stmt.value),
            Statement::Return(stmt) => match &stmt.value {
                Some(val) => str("return ").append(self.expression(val)).append(';'),
                None => str("return").append(';'),
            },
            Statement::InfiniteLoop(stmt) => str("loop ").append(self.block(&stmt.block)),
            Statement::IteratorLoop(stmt) => {
                let collection = self.expression(&stmt.collection);
                let body = self.block(&stmt.block);

                str("for ")
                    .append(stmt.pattern.as_str())
                    .space()
                    .append(collection)
                    .space()
                    .append(body)
            }
            Statement::PredicateLoop(stmt) => {
                let predicate = self.expression(&stmt.condition);
                let body = self.block(&stmt.block);

                str("while ").append(predicate).space().append(body)
            }
            Statement::Expression(expr) => {
                let needs_semicolon = !matches!(expr.as_ref(), Expression::If(_) | Expression::Switch(_));
                let expr = self.expression(expr);

                if needs_semicolon { expr.append(';') } else { expr }
            }
        };

        with_comments(doc, comments)
    }

    fn expression<'a>(&mut self, expr: &'a Expression) -> Document<'a> {
        let comments = self.pop_comments_before(expr.location().start());
        let doc = match expr {
            Expression::Array(expr) => self.array(expr),
            Expression::Assignment(expr) => self.assignment(expr),
            Expression::Call(expr) => self.call(expr),
            Expression::Cast(expr) => self.cast(expr),
            Expression::Construct(expr) => self.construct(expr),
            Expression::If(expr) => self.if_cond(expr),
            Expression::IntrinsicCall(expr) => self.intrinsic(expr, false),
            Expression::Is(expr) => self.is(expr),
            Expression::Literal(lit) => self.literal(lit),
            Expression::Member(expr) => self.member(expr),
            Expression::Range(expr) => self.range(expr),
            Expression::Scope(expr) => self.scope(expr),
            Expression::Switch(expr) => self.switch(expr),
            Expression::Variable(expr) => str(expr.name.as_str()),
            Expression::Variant(expr) => self.variant(expr),
        };

        with_comments(doc, comments)
    }

    fn array<'a>(&mut self, expr: &'a Array) -> Document<'a> {
        if expr.values.is_empty() {
            let comments = self.pop_comments_before(expr.location.end());

            return match printed_comments(comments, false) {
                Some(comments) => str("[")
                    .append(break_("", "").nest(self.indent()))
                    .append(comments.nest(self.indent()))
                    .append(break_("", ""))
                    .append("]")
                    .force_break(),
                None => str("[]"),
            };
        }

        let has_comments = self.any_comments_before(expr.location.end());
        let mut values = Vec::with_capacity(expr.values.len());

        for value in &expr.values {
            let leading_comment = self.pop_comments_on_same_line(value.location().start());
            let value_doc = self.expression(value);

            let commented = match leading_comment {
                Some(comment) => Document::String(comment.to_string()).append(line()).append(value_doc),
                None => value_doc,
            };

            values.push(commented);
        }

        let doc = self.wrap_comma_separated_of("[", "]", values);
        if has_comments { doc.force_break() } else { doc }
    }

    fn assignment<'a>(&mut self, expr: &'a Assignment) -> Document<'a> {
        let target = self.expression(&expr.target).group();
        let value = self.expression(&expr.value).group();

        target.append(" = ").append(value)
    }

    fn call<'a>(&mut self, expr: &'a Call) -> Document<'a> {
        let arguments = expr.arguments.iter().map(|arg| self.expression(arg)).collect_vec();
        let wrapped_arguments = self.wrap_comma_separated_of("(", ")", arguments);

        let name = self.path(&expr.name);

        match &expr.callee {
            Some(callee) => {
                let callee = self.expression(callee);

                callee.append(".").append(name).append(wrapped_arguments)
            }
            None => name.append(wrapped_arguments),
        }
    }

    fn cast<'a>(&mut self, expr: &'a Cast) -> Document<'a> {
        let source = self.expression(&expr.source);
        let target_type = self.ty(&expr.target_type);

        source.append(" as ").append(target_type)
    }

    fn construct<'a>(&mut self, expr: &'a Construct) -> Document<'a> {
        let name = self.path(&expr.path);

        let mut fields = Vec::with_capacity(expr.fields.len());
        for field in &expr.fields {
            let field_name = str(field.name.as_str());

            let field_doc = match &field.value {
                Some(value) => field_name.append(": ").append(self.expression(value)),
                None => field_name,
            };

            fields.push(field_doc);
        }

        let fields = self.wrap_comma_separated_of(cond("{", "{ "), cond("}", " }"), fields);

        name.space().append(fields)
    }

    fn if_cond<'a>(&mut self, expr: &'a IfCondition) -> Document<'a> {
        let mut conditions = Vec::new();

        for (idx, case) in expr.cases.iter().enumerate() {
            let body = self.block(&case.block);

            let condition = match &case.condition {
                Some(cond) => {
                    let keyword = if idx == 0 { "if " } else { " else if " };
                    let cond = self.expression(cond).group();

                    str(keyword).append(cond).space()
                }
                None => str(" else "),
            };

            conditions.push(condition.append(body));
        }

        if conditions.len() > 1 {
            concat(conditions).force_break()
        } else {
            concat(conditions)
        }
    }

    fn intrinsic<'a>(&mut self, expr: &'a IntrinsicCall, nested: bool) -> Document<'a> {
        let operator = match &expr.kind {
            IntrinsicKind::Add { .. } => "+",
            IntrinsicKind::Sub { .. } | IntrinsicKind::Negate { .. } => "-",
            IntrinsicKind::Mul { .. } => "*",
            IntrinsicKind::Div { .. } => "/",
            IntrinsicKind::And { .. } => "&&",
            IntrinsicKind::Or { .. } => "||",
            IntrinsicKind::BinaryAnd { .. } => "&",
            IntrinsicKind::BinaryOr { .. } => "|",
            IntrinsicKind::BinaryXor { .. } => "^",
            IntrinsicKind::Equal { .. } => "==",
            IntrinsicKind::NotEqual { .. } => "!=",
            IntrinsicKind::Less { .. } => "<",
            IntrinsicKind::LessEqual { .. } => "<=",
            IntrinsicKind::Greater { .. } => ">",
            IntrinsicKind::GreaterEqual { .. } => ">=",
            IntrinsicKind::Decrement { .. } => "--",
            IntrinsicKind::Increment { .. } => "++",
            IntrinsicKind::Not { .. } => "!",
        };

        match &expr.kind {
            IntrinsicKind::Add { lhs, rhs }
            | IntrinsicKind::Sub { lhs, rhs }
            | IntrinsicKind::Mul { lhs, rhs }
            | IntrinsicKind::Div { lhs, rhs }
            | IntrinsicKind::And { lhs, rhs }
            | IntrinsicKind::Or { lhs, rhs }
            | IntrinsicKind::BinaryAnd { lhs, rhs }
            | IntrinsicKind::BinaryOr { lhs, rhs }
            | IntrinsicKind::BinaryXor { lhs, rhs }
            | IntrinsicKind::Equal { lhs, rhs }
            | IntrinsicKind::NotEqual { lhs, rhs }
            | IntrinsicKind::Less { lhs, rhs }
            | IntrinsicKind::LessEqual { lhs, rhs }
            | IntrinsicKind::Greater { lhs, rhs }
            | IntrinsicKind::GreaterEqual { lhs, rhs } => {
                let lhs = match lhs.as_ref() {
                    Expression::IntrinsicCall(lhs) => self.intrinsic(lhs, true),
                    _ => self.expression(lhs),
                };

                let rhs = match rhs.as_ref() {
                    Expression::IntrinsicCall(rhs) => self.intrinsic(rhs, true),
                    _ => self.expression(rhs),
                };

                let block = lhs
                    .append(break_("", " "))
                    .append(str(operator).space().append(rhs))
                    .group();

                if nested {
                    block
                } else {
                    block.nest_if_broken(self.indent())
                }
            }
            IntrinsicKind::Decrement { target } | IntrinsicKind::Increment { target } => {
                self.expression(target).append(operator)
            }
            IntrinsicKind::Negate { target } | IntrinsicKind::Not { target } => {
                operator.as_doc().append(self.expression(target))
            }
        }
    }

    fn is<'a>(&mut self, expr: &'a Is) -> Document<'a> {
        let target = self.expression(&expr.target);
        let pattern = self.pattern(&expr.pattern);

        target.append(" is ").append(pattern)
    }

    fn literal<'a>(&mut self, lit: &'a Literal) -> Document<'a> {
        // Since most information abot the actual literal content isn't encoded in the
        // AST, it's easier for us to copy the source span, which the literal
        // occupies in the source text.
        //
        // For example, writing `0xDEAD_BEEF` would be converted into `3735928559` since
        // we don't encode the radix or underscores.
        let range = lit.location().0.clone();
        let span = self.source_file.content[range].to_string();

        span.as_doc()
    }

    fn member<'a>(&mut self, expr: &'a Member) -> Document<'a> {
        let callee = self.expression(&expr.callee);
        let name = str(expr.name.as_str());

        callee.append(".").append(name)
    }

    fn range<'a>(&mut self, expr: &'a lume_ast::Range) -> Document<'a> {
        let lower = self.expression(&expr.lower);
        let upper = self.expression(&expr.upper);

        let upper = if expr.inclusive { str("=").append(upper) } else { upper };

        lower
            .append(break_("", ""))
            .append(str("..").append(upper))
            .group()
            .nest_if_broken(self.indent())
    }

    fn scope<'a>(&mut self, expr: &'a Scope) -> Document<'a> {
        self.statements(&expr.body, &expr.location)
    }

    fn switch<'a>(&mut self, expr: &'a Switch) -> Document<'a> {
        let operand = self.expression(&expr.operand);

        let mut cases = Vec::with_capacity(expr.cases.len());
        for case in &expr.cases {
            let pattern = self.pattern(&case.pattern);
            let branch = self.expression(&case.branch);

            let case = pattern.append(" => ").append(branch).append(",");
            cases.push(case);
        }

        let header = str("switch ").append(operand).space();
        if cases.is_empty() {
            return header.append("{}");
        }

        let body = str("{")
            .append(line().append(join(cases, line())).nest(self.indent()).group())
            .append(line())
            .append("}");

        header.append(body)
    }

    fn variant<'a>(&mut self, expr: &'a Variant) -> Document<'a> {
        let name = self.path(&expr.name);

        if expr.arguments.is_empty() {
            return name;
        }

        let arguments = expr.arguments.iter().map(|arg| self.expression(arg)).collect_vec();
        let wrapped_arguments = self.wrap_comma_separated_of("(", ")", arguments);

        name.append(wrapped_arguments)
    }

    fn pattern<'a>(&mut self, pattern: &'a Pattern) -> Document<'a> {
        match pattern {
            Pattern::Literal(lit) => self.literal(lit),
            Pattern::Identifier(ident) => ident.as_str().as_doc(),
            Pattern::Variant(variant) => {
                let name = self.path(&variant.name);

                if variant.fields.is_empty() {
                    return name;
                }

                let fields = variant.fields.iter().map(|field| self.pattern(field)).collect_vec();
                let wrapped_fields = self.wrap_comma_separated_of("(", ")", fields);

                name.append(wrapped_fields)
            }
            Pattern::Wildcard(_) => str(".."),
        }
    }

    fn ty<'a>(&mut self, ty: &'a Type) -> Document<'a> {
        match ty {
            Type::Named(ty) => self.path(&ty.name),
            Type::SelfType(_) => str("self"),
            Type::Array(ty) => concat(vec![str("["), self.ty(&ty.element_type), str("]")]),
        }
    }

    fn type_arguments<'a>(&mut self, type_args: &'a [Type]) -> Document<'a> {
        if type_args.is_empty() {
            return empty();
        }

        let types = type_args.iter().map(|ty| self.ty(ty)).collect::<Vec<_>>();

        concat(vec![str("<"), join(types, ", "), str(">")])
    }

    fn wrap_comma_separated_of<'a, I>(
        &mut self,
        open: impl AsDocument<'a>,
        close: impl AsDocument<'a>,
        values: I,
    ) -> Document<'a>
    where
        I: IntoIterator<Item = Document<'a>>,
    {
        let open_doc = open.as_doc();
        let close_doc = close.as_doc();

        let values = values.into_iter().collect_vec();
        if values.is_empty() {
            return open_doc.append(close_doc);
        }

        let values = break_("", "")
            .append(join(values, break_(",", ", ")))
            .nest_if_broken(self.indent());

        open_doc.append(values).append(concat([break_(",", ""), close_doc]))
    }
}

fn namespace(namespace: &Namespace) -> Document<'_> {
    let segments = namespace.path.path.iter().map(|seg| str(seg.as_str())).collect_vec();
    let path = join(segments, "::");

    str("namespace ").append(path)
}

fn with_comments<'a>(doc: Document<'a>, comments: Comments<'_>) -> Document<'a> {
    match printed_comments(comments, true) {
        Some(comments) => comments.append(doc),
        None => doc,
    }
}

fn printed_comments<'a>(comments: Comments<'_>, last_newline: bool) -> Option<Document<'a>> {
    let trailing_newline = comments.trailing_newline;

    let mut comments = comments.lines.into_iter().peekable();
    let _ = comments.peek()?;

    let mut doc = Vec::new();
    while let Some((_span, c)) = comments.next() {
        // If the last line in the comment block is empty (i.e. has no actual text
        // content), we skip over it.
        if c.trim() == "//" && comments.peek().is_none() {
            continue;
        }

        doc.push(String::from(c).as_doc());

        match comments.peek() {
            Some(_) => {
                doc.push(line());
            }
            None => {
                if last_newline {
                    doc.push(line());
                }
            }
        }
    }

    if trailing_newline {
        doc.push(line());
    }

    Some(concat(doc))
}

fn visibility(vis: Option<&Visibility>) -> Document<'_> {
    match vis {
        Some(Visibility::Public { .. }) => str("pub "),
        Some(Visibility::Internal { .. }) => str("pub(internal) "),
        Some(Visibility::Private { .. }) => str("priv "),
        None => empty(),
    }
}

impl Document<'_> {
    pub fn print(&self, config: &Config) -> Result<String, std::fmt::Error> {
        let mut buffer = String::new();
        crate::print::format(&mut buffer, config.max_width.cast_signed(), im::vector![(
            0,
            crate::print::Mode::Unbroken,
            self
        )])?;

        if config.trailing_line {
            buffer.push('\n');
        }

        Ok(buffer)
    }
}
