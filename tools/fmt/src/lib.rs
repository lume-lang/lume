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
use lume_ast::support::WithDocumentation;
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
    pub root_node: lume_ast::SourceFile,

    /// Defines a list of all comments from the original source file, which
    /// were discarded during parsing.
    ///
    /// Each entry has a range, depicting the actual position of the comment in
    /// the original source file, and a slice of the comment (including
    /// backslashes).
    pub comments: Vec<(Range<usize>, &'src str)>,
}

fn parse_source<'src>(content: &'src str, dcx: DiagCtxHandle) -> lume_errors::Result<Source<'src>> {
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

    let parser = Parser::from_tokens(source_file.clone(), tokens.into_iter());
    let syntax_tree = parser.parse(lume_parser::Target::Item).syntax();
    let root_node = lume_ast::SourceFile::cast(syntax_tree).unwrap();

    dcx.ensure_untainted()?;

    Ok(Source {
        file: source_file,
        root_node,
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
            root_node,
            comments,
        } = source;

        self.source_file = source_file.clone();
        self.comments = comments
            .iter()
            .map(|(span, content)| (span.clone(), *content))
            .collect();

        self.with_spacing(root_node.item(), |fmt, item| fmt.top_level_expression(item))
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

    fn doc_comments<'a, I>(&self, doc: I) -> Document<'a>
    where
        I: IntoIterator<Item = String>,
    {
        concat(
            doc.into_iter()
                .map(|doc| self.doc_comment(doc.trim_start_matches("///").trim_start())),
        )
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
        T: lume_ast::AstNode,
        I: Iterator<Item = T>,
        F: FnMut(&mut Self, T) -> Document<'a>,
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

            // When getting the end of the item span, trim any whitespace from it.
            //
            // This prevents items such as:
            // ```lm
            // namespace std
            //
            // import std (Optional)
            // ```
            // where the `namespace std` item spans to the start of the `import` item.
            let item_snippet = &self.source_file.content[item.range()];
            let extra_whitespace_len = item.range().len() - item_snippet.trim_end().len();
            let span_end = item.location().end() - extra_whitespace_len;

            (prev_line, _) = self.coordinates_of(span_end);
            items.push(f(self, item).group());
        }

        items.as_doc()
    }

    fn path<'a>(&mut self, path: Path) -> Document<'a> {
        join(path.path_segment().map(|seg| self.path_segment(seg)), "::")
    }

    fn path_segment<'a>(&mut self, segment: PathSegment) -> Document<'a> {
        match segment {
            PathSegment::PathNamespace(_) | PathSegment::PathVariant(_) => {
                segment.as_text().trim().to_string().as_doc()
            }
            PathSegment::PathType(path) => {
                let name = match path.name() {
                    Some(name) => string(name.as_text()),
                    None => empty(),
                };

                match path.generic_args() {
                    Some(bound_types) => name.append(self.type_arguments(bound_types.args())),
                    None => name,
                }
            }
            PathSegment::PathCallable(path) => {
                let name = match path.name() {
                    Some(name) => string(name.as_text()),
                    None => empty(),
                };

                match path.generic_args() {
                    Some(bound_types) => name.append(self.type_arguments(bound_types.args())),
                    None => name,
                }
            }
        }
    }

    fn top_level_expression<'a>(&mut self, item: Item) -> Document<'a> {
        match item {
            Item::Namespace(ns) => namespace(ns),
            Item::Import(import) => self.import(import),
            Item::Fn(func) => self.function_definition(func),
            Item::Struct(struct_def) => self.struct_definition(struct_def),
            Item::Trait(trait_def) => self.trait_definition(trait_def),
            Item::Enum(enum_def) => self.enum_definition(enum_def),
            Item::TraitImpl(trait_impl) => self.trait_implementation(trait_impl),
            Item::Impl(implementation) => self.implementation(implementation),
        }
    }

    fn import<'a>(&mut self, import: Import) -> Document<'a> {
        let Some(import_path) = import.import_path() else {
            return import.as_text().as_doc();
        };

        let Some(import_list) = import.import_list() else {
            return import.as_text().as_doc();
        };

        let root = join(import_path.path().map(|seg| seg.as_text().as_doc()), "::");
        let import_name_docs = import_list.items().map(|seg| seg.as_text().as_doc());

        let nested_names = flex_break("", "")
            .append(join(import_name_docs, flex_break(",", ", ")))
            .group()
            .nest_if_broken(self.indent());

        let wrapped_names = str("(")
            .append(nested_names)
            .append(concat([flex_break(",", ""), str(")")]));

        str("import ").append(root).space().append(wrapped_names)
    }

    fn signature<'a>(&mut self, sig: Sig) -> Document<'a> {
        let Some(param_list) = sig.param_list() else {
            return sig.as_text().as_doc();
        };

        let signature = str("fn ")
            .append(if sig.extern_kw().is_some() { "external " } else { "" })
            .append(sig.name().as_doc())
            .append(self.type_parameters(sig.bound_types()))
            .append(self.parameters(param_list));

        match sig.return_type() {
            Some(ret_ty) => signature.append(" -> ").append(self.ty_opt(ret_ty.ty())),
            None => signature,
        }
        .group()
    }

    fn function_definition<'a>(&mut self, func: Fn) -> Document<'a> {
        let doc_comment = self.doc_comments(func.documentation());
        let attributes = self.attributes(func.attr());

        let Some(signature) = func.sig() else {
            return func.as_text().as_doc();
        };

        let signature = doc_comment
            .append(attributes)
            .append(visibility(func.visibility()))
            .append(self.signature(signature));

        let body = match func.block() {
            Some(block) => str(" ").append(self.block(block)),
            None => empty(),
        };

        signature.append(body)
    }

    fn struct_definition<'a>(&mut self, def: Struct) -> Document<'a> {
        let doc_comment = self.doc_comments(def.documentation());
        let attributes = self.attributes(def.attr());

        let header = doc_comment
            .append(attributes)
            .append(visibility(def.visibility()))
            .append("struct ")
            .append(def.name())
            .append(self.type_parameters(def.bound_types()))
            .space();

        let mut prev_line = 0;
        let mut fields = Vec::new();

        for (idx, field) in def.fields().enumerate() {
            let (curr_line, _) = self.coordinates_of(field.location().start());

            if idx != 0 && curr_line.saturating_sub(prev_line) > 1 {
                fields.push(lines(2));
            } else if idx != 0 {
                fields.push(line());
            }

            (prev_line, _) = self.coordinates_of(field.location().end());

            let doc_comment = self.doc_comments(field.documentation());
            let visibility = visibility(field.visibility());

            let field_type = self.ty_opt(field.field_type());
            let default_value = match field.default_value() {
                Some(val) => str(" = ").append(self.expression(val)),
                None => empty(),
            };

            let field_doc = doc_comment
                .append(visibility)
                .append(field.name())
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

    fn trait_definition<'a>(&mut self, def: Trait) -> Document<'a> {
        let doc_comment = self.doc_comments(def.documentation());
        let attributes = self.attributes(def.attr());

        let header = doc_comment
            .append(attributes)
            .append(visibility(def.visibility()))
            .append("trait ")
            .append(def.name())
            .append(self.type_parameters(def.bound_types()))
            .space();

        let methods = def.methods().collect_vec();
        if methods.is_empty() {
            return header.append("{}");
        }

        let methods = self.with_spacing(methods.into_iter(), |fmt, method| fmt.trait_method_definition(method));

        let body = str("{")
            .append(line().append(methods).nest(self.indent()).group())
            .append(line())
            .append("}");

        header.append(body)
    }

    fn trait_method_definition<'a>(&mut self, method: Method) -> Document<'a> {
        let doc_comment = self.doc_comments(method.documentation());

        let Some(signature) = method.sig() else {
            return method.as_text().as_doc();
        };

        let attributes = self.attributes(method.attr());
        let signature = doc_comment.append(attributes).append(self.signature(signature));

        let body = match method.block() {
            Some(block) => str(" ").append(self.block(block)),
            None => str(";"),
        };

        signature.append(body)
    }

    fn enum_definition<'a>(&mut self, def: Enum) -> Document<'a> {
        let doc_comment = self.doc_comments(def.documentation());

        let header = doc_comment
            .append(visibility(def.visibility()))
            .append("enum ")
            .append(def.name())
            .append(self.type_parameters(def.bound_types()))
            .space();

        let cases = def.cases().collect_vec();
        if cases.is_empty() {
            return header.append("{}");
        }

        let variants = self.with_spacing(cases.into_iter(), |fmt, variant| fmt.enum_variant_definition(variant));

        let body = str("{")
            .append(line().append(variants).nest(self.indent()).group())
            .append(line())
            .append("}");

        header.append(body)
    }

    fn enum_variant_definition<'a>(&mut self, def: Case) -> Document<'a> {
        let doc_comment = self.doc_comments(def.documentation());

        let identifier = doc_comment.append(def.name());
        let fields = match def.case_param_list() {
            Some(case_list) => case_list.ty().map(|param| self.ty(param)).collect_vec(),
            None => vec![],
        };

        if fields.is_empty() {
            return identifier;
        }

        identifier
            .append(self.wrap_comma_separated_of("(", ")", fields))
            .append(",")
    }

    fn trait_implementation<'a>(&mut self, trait_impl: TraitImpl) -> Document<'a> {
        let doc_comment = self.doc_comments(trait_impl.documentation());

        let header = doc_comment
            .append("use")
            .append(self.type_parameters(trait_impl.bound_types()))
            .space()
            .append(self.ty_opt(trait_impl.trait_type()))
            .append(": ")
            .append(self.ty_opt(trait_impl.target_type()))
            .space();

        let methods = trait_impl.methods().collect_vec();
        if methods.is_empty() {
            return header.append("{}");
        }

        let methods = self.with_spacing(methods.into_iter(), |fmt, method| {
            fmt.trait_method_implementation(method)
        });

        let body = str("{")
            .append(line().append(methods).nest(self.indent()).group())
            .append(line())
            .append("}");

        header.append(body)
    }

    fn trait_method_implementation<'a>(&mut self, method: Method) -> Document<'a> {
        let doc_comment = self.doc_comments(method.documentation());

        let Some(signature) = method.sig() else {
            return method.as_text().as_doc();
        };

        let signature = self.signature(signature);

        let body = match method.block() {
            Some(block) => str(" ").append(self.block(block)),
            None => empty(),
        };

        doc_comment.append(signature).append(body)
    }

    fn implementation<'a>(&mut self, implementation: Impl) -> Document<'a> {
        let doc_comment = self.doc_comments(implementation.documentation());

        let header = doc_comment
            .append("impl")
            .append(self.type_parameters(implementation.bound_types()))
            .space()
            .append(self.ty_opt(implementation.target()))
            .space();

        let methods = implementation.methods().collect_vec();
        if methods.is_empty() {
            return header.append("{}");
        }

        let methods = self.with_spacing(methods.into_iter(), |fmt, method| fmt.method_implementation(method));

        let body = str("{")
            .append(line().append(methods).nest(self.indent()).group())
            .append(line())
            .append("}");

        header.append(body)
    }

    fn method_implementation<'a>(&mut self, method: Method) -> Document<'a> {
        let doc_comment = self.doc_comments(method.documentation());
        let attributes = self.attributes(method.attr());

        let Some(signature) = method.sig() else {
            return method.as_text().as_doc();
        };

        let signature = doc_comment
            .append(attributes)
            .append(visibility(method.visibility()))
            .append(self.signature(signature));

        let body = match method.block() {
            Some(block) => str(" ").append(self.block(block)),
            None => empty(),
        };

        signature.append(body)
    }

    fn attributes<'a, I>(&mut self, attrs: I) -> Document<'a>
    where
        I: IntoIterator<Item = Attr>,
    {
        let attrs = attrs.into_iter().collect::<Vec<_>>();

        if attrs.is_empty() {
            return empty();
        }

        concat(attrs.into_iter().map(|attr| self.attribute(attr)).collect_vec())
    }

    fn attribute<'a>(&mut self, attr: Attr) -> Document<'a> {
        let name = attr.name().as_doc();

        let arg_list = match attr.arg_list() {
            Some(arguments) => arguments.args().collect_vec(),
            None => return str("![").append(name).append("]").append(line()),
        };

        let mut arguments = Vec::with_capacity(arg_list.len());
        for arg in arg_list {
            let name = arg.name().as_doc();

            let arg_doc = match arg.value() {
                Some(value) => name.append(" = ").append(self.literal(value)),
                None => arg.as_text().as_doc(),
            };

            arguments.push(arg_doc);
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

    fn parameters<'a>(&mut self, params: ParamList) -> Document<'a> {
        let params = params.param().collect_vec();

        if params.is_empty() {
            return str("()");
        }

        let parameters = params.into_iter().map(|param| self.parameter(param)).collect_vec();
        self.wrap_comma_separated_of("(", ")", parameters)
    }

    fn parameter<'a>(&mut self, param: Param) -> Document<'a> {
        if param.is_self() {
            return str("self");
        }

        let Some(ty) = param.ty() else {
            return param.as_text().as_doc();
        };

        if param.vararg().is_some() { str("...") } else { empty() }
            .append(param.name().as_doc())
            .append(": ")
            .append(self.ty(ty))
            .group()
    }

    fn type_parameters<'a>(&mut self, params: Option<BoundTypes>) -> Document<'a> {
        let params = match params {
            Some(list) => list.types().collect_vec(),
            None => return empty(),
        };

        if params.is_empty() {
            return empty();
        }

        let type_params = params
            .into_iter()
            .map(|type_param| self.type_parameter(type_param))
            .collect_vec();

        self.wrap_comma_separated_of("<", ">", type_params)
    }

    fn type_parameter<'a>(&mut self, param: BoundType) -> Document<'a> {
        let name = param.name().as_doc();

        match param.constraints() {
            Some(constraints) => {
                let constraints = constraints.ty().map(|constraint| self.ty(constraint));

                name.append(": ").append(join(constraints, " + "))
            }
            None => name,
        }
    }

    fn block<'a>(&mut self, block: Block) -> Document<'a> {
        self.statements(block.stmt(), block.location())
    }

    fn statements<'a, I>(&mut self, body: I, loc: Location) -> Document<'a>
    where
        I: IntoIterator<Item = Stmt>,
    {
        let body = body.into_iter().collect_vec();

        if body.is_empty() && !self.any_comments_before(loc.end()) {
            return str("{}");
        }

        let body = self.with_spacing(body.into_iter(), |fmt, stmt| fmt.statement(stmt));
        let body = match printed_comments(self.pop_comments_before(loc.end()), true) {
            Some(comments) => body.append(line()).append(comments),
            None => body,
        };

        str("{")
            .append(line().append(body).nest(self.indent()).group())
            .append(line())
            .append("}")
    }

    fn statement<'a>(&mut self, stmt: Stmt) -> Document<'a> {
        let comments = self.pop_comments_before(stmt.location().start());
        let doc = match stmt {
            Stmt::LetStmt(stmt) => {
                let Some(value) = stmt.expr() else {
                    return stmt.as_text().as_doc();
                };

                let name = stmt.name();
                let declared_type = match stmt.ty() {
                    Some(ty) => str(": ").append(self.ty(ty)),
                    None => empty(),
                };

                let value = self.expression(value);

                str("let ")
                    .append(name)
                    .append(declared_type)
                    .append(" = ")
                    .append(value.group())
                    .append(";")
            }
            Stmt::BreakStmt(_) => str("break").append(';'),
            Stmt::ContinueStmt(_) => str("continue").append(';'),
            Stmt::FinalStmt(stmt) => {
                let Some(value) = stmt.expr() else {
                    return stmt.as_text().as_doc();
                };

                self.expression(value)
            }
            Stmt::ReturnStmt(stmt) => match stmt.expr() {
                Some(val) => str("return ").append(self.expression(val)).append(';'),
                None => str("return").append(';'),
            },
            Stmt::LoopStmt(stmt) => match stmt.block() {
                Some(block) => str("loop ").append(self.block(block)),
                None => stmt.as_text().as_doc(),
            },
            Stmt::ForStmt(stmt) => {
                let Some(collection) = stmt.collection() else {
                    return stmt.as_text().as_doc();
                };

                let Some(body) = stmt.block() else {
                    return stmt.as_text().as_doc();
                };

                let collection = self.expression(collection);
                let body = self.block(body);

                str("for ")
                    .append(stmt.pattern())
                    .space()
                    .append("in")
                    .space()
                    .append(collection)
                    .space()
                    .append(body)
            }
            Stmt::WhileStmt(stmt) => {
                let Some(condition) = stmt.condition() else {
                    return stmt.as_text().as_doc();
                };

                let Some(body) = stmt.block() else {
                    return stmt.as_text().as_doc();
                };

                let predicate = self.expression(condition);
                let body = self.block(body);

                str("while ").append(predicate).space().append(body)
            }
            Stmt::ExprStmt(expr) => {
                let Some(expr) = expr.expr() else {
                    return expr.as_text().as_doc();
                };

                let needs_semicolon = !matches!(expr, Expr::IfExpr(_) | Expr::SwitchExpr(_));
                let expr = self.expression(expr);

                if needs_semicolon { expr.append(';') } else { expr }
            }
        };

        with_comments(doc, comments)
    }

    fn expression<'a>(&mut self, expr: Expr) -> Document<'a> {
        let comments = self.pop_comments_before(expr.location().start());
        let doc = match expr {
            Expr::ArrayExpr(expr) => self.array(expr),
            Expr::AssignmentExpr(expr) => self.assignment(expr),
            Expr::InstanceCallExpr(expr) => self.instance_call(expr),
            Expr::StaticCallExpr(expr) => self.static_call(expr),
            Expr::CastExpr(expr) => self.cast(expr),
            Expr::ConstructExpr(expr) => self.construct(expr),
            Expr::IfExpr(expr) => self.if_cond(expr),
            Expr::BinExpr(expr) => self.bin_expr(expr, false),
            Expr::PostfixExpr(expr) => self.postfix_expr(expr),
            Expr::UnaryExpr(expr) => self.unary_expr(expr),
            Expr::IsExpr(expr) => self.is(expr),
            Expr::LitExpr(lit) => match lit.literal() {
                Some(lit) => self.literal(lit),
                None => lit.as_text().as_doc(),
            },
            Expr::MemberExpr(expr) => self.member(expr),
            Expr::RangeExpr(expr) => self.range(expr),
            Expr::ScopeExpr(expr) => self.scope(expr),
            Expr::SwitchExpr(expr) => self.switch(expr),
            Expr::VariableExpr(expr) => expr.name().as_doc(),
            Expr::VariantExpr(expr) => self.variant(expr),
            Expr::ParenExpr(expr) => match expr.expr() {
                Some(expr) => concat(vec![str("("), self.expression(expr), str(")")]),
                None => expr.as_text().as_doc(),
            },
        };

        with_comments(doc, comments)
    }

    fn array<'a>(&mut self, expr: ArrayExpr) -> Document<'a> {
        let items = expr.items().collect_vec();

        if items.is_empty() {
            let comments = self.pop_comments_before(expr.location().end());

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

        let has_comments = self.any_comments_before(expr.location().end());
        let mut values = Vec::with_capacity(items.len());

        for value in items {
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

    fn assignment<'a>(&mut self, expr: AssignmentExpr) -> Document<'a> {
        let Some(target) = expr.lhs() else {
            return expr.as_text().as_doc();
        };

        let Some(value) = expr.rhs() else {
            return expr.as_text().as_doc();
        };

        let target = self.expression(target).group();
        let value = self.expression(value).group();

        target.append(" = ").append(value)
    }

    fn instance_call<'a>(&mut self, expr: InstanceCallExpr) -> Document<'a> {
        let Some(callee) = expr.callee() else {
            return expr.as_text().as_doc();
        };

        let Some(arg_list) = expr.arg_list() else {
            return expr.as_text().as_doc();
        };

        let callee = self.expression(callee);
        let name = expr.name().as_doc();

        let arguments = arg_list.expr().map(|arg| self.expression(arg)).collect_vec();
        let wrapped_arguments = self.wrap_comma_separated_of("(", ")", arguments);

        callee.append(".").append(name).append(wrapped_arguments)
    }

    fn static_call<'a>(&mut self, expr: StaticCallExpr) -> Document<'a> {
        let path = match expr.path() {
            Some(path) => self.path(path),
            None => return expr.as_text().as_doc(),
        };

        let Some(arg_list) = expr.arg_list() else {
            return expr.as_text().as_doc();
        };

        let arguments = arg_list.expr().map(|arg| self.expression(arg)).collect_vec();
        let wrapped_arguments = self.wrap_comma_separated_of("(", ")", arguments);

        path.append(wrapped_arguments)
    }

    fn cast<'a>(&mut self, expr: CastExpr) -> Document<'a> {
        let Some(source) = expr.expr() else {
            return expr.as_text().as_doc();
        };

        let Some(target_type) = expr.ty() else {
            return expr.as_text().as_doc();
        };

        let source = self.expression(source);
        let target_type = self.ty(target_type);

        source.append(" as ").append(target_type)
    }

    fn construct<'a>(&mut self, expr: ConstructExpr) -> Document<'a> {
        let Some(path) = expr.ty() else {
            return expr.as_text().as_doc();
        };

        let name = self.path(path);

        let mut fields = Vec::new();
        for field in expr.fields() {
            let field_name = field.name().as_doc();

            let field_doc = match field.value() {
                Some(value) => field_name.append(": ").append(self.expression(value)),
                None => field_name,
            };

            fields.push(field_doc);
        }

        let fields = self.wrap_comma_separated_of(cond("{", "{ "), cond("}", " }"), fields);

        name.space().append(fields)
    }

    fn if_cond<'a>(&mut self, expr: IfExpr) -> Document<'a> {
        let mut conditions = Vec::new();

        for (idx, case) in expr.cases().enumerate() {
            let Some(block) = case.block() else {
                return expr.as_text().as_doc();
            };

            let body = self.block(block);

            let condition = match case.condition() {
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

    fn bin_expr<'a>(&mut self, expr: BinExpr, nested: bool) -> Document<'a> {
        let lhs = match expr.lhs() {
            Some(expr) => self.expression(expr),
            None => return expr.as_text().as_doc(),
        };

        let rhs = match expr.rhs() {
            Some(expr) => self.expression(expr),
            None => return expr.as_text().as_doc(),
        };

        let operator = match expr {
            // Arithmetic intrinsics
            _ if expr.add().is_some() => "+",
            _ if expr.sub().is_some() => "-",
            _ if expr.mul().is_some() => "*",
            _ if expr.div().is_some() => "/",
            _ if expr.and().is_some() => "&&",
            _ if expr.or().is_some() => "||",

            // Logical intrinsics
            _ if expr.binary_and().is_some() => "&",
            _ if expr.binary_or().is_some() => "|",
            _ if expr.binary_xor().is_some() => "^",

            // Comparison intrinsics
            _ if expr.equal().is_some() => "==",
            _ if expr.nequal().is_some() => "!=",
            _ if expr.less().is_some() => "<",
            _ if expr.lequal().is_some() => "<=",
            _ if expr.greater().is_some() => ">",
            _ if expr.gequal().is_some() => ">=",

            _ => return expr.as_text().as_doc(),
            // IntrinsicKind::Decrement { .. } => "--",
            // IntrinsicKind::Increment { .. } => "++",
            // IntrinsicKind::Not { .. } => "!",
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

        /*
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
                    Expression::IntrinsicCall(lhs) => self.bin_expr(lhs, true),
                    _ => self.expression(lhs),
                };

                let rhs = match rhs.as_ref() {
                    Expression::IntrinsicCall(rhs) => self.bin_expr(rhs, true),
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
         */
    }

    fn postfix_expr<'a>(&mut self, expr: PostfixExpr) -> Document<'a> {
        let value = match expr.expr() {
            Some(expr) => self.expression(expr),
            None => return expr.as_text().as_doc(),
        };

        let operator = match expr {
            _ if expr.increment().is_some() => "--",
            _ if expr.decrement().is_some() => "++",

            _ => return expr.as_text().as_doc(),
        };

        value.append(operator)
    }

    fn unary_expr<'a>(&mut self, expr: UnaryExpr) -> Document<'a> {
        let value = match expr.expr() {
            Some(expr) => self.expression(expr),
            None => return expr.as_text().as_doc(),
        };

        let operator = match expr {
            _ if expr.sub().is_some() => "-",
            _ if expr.not().is_some() => "!",

            _ => return expr.as_text().as_doc(),
        };

        operator.as_doc().append(value)
    }

    fn is<'a>(&mut self, expr: IsExpr) -> Document<'a> {
        let target = match expr.expr() {
            Some(expr) => self.expression(expr),
            None => return expr.as_text().as_doc(),
        };

        let pattern = match expr.pat() {
            Some(pat) => self.pattern(pat),
            None => return expr.as_text().as_doc(),
        };

        target.append(" is ").append(pattern)
    }

    fn literal<'a>(&mut self, lit: Literal) -> Document<'a> {
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

    fn member<'a>(&mut self, expr: MemberExpr) -> Document<'a> {
        let callee = match expr.expr() {
            Some(expr) => self.expression(expr),
            None => return expr.as_text().as_doc(),
        };

        callee.append(".").append(expr.name())
    }

    fn range<'a>(&mut self, expr: lume_ast::RangeExpr) -> Document<'a> {
        let lower = match expr.lower() {
            Some(expr) => self.expression(expr),
            None => return expr.as_text().as_doc(),
        };

        let upper = match expr.upper() {
            Some(expr) => self.expression(expr),
            None => return expr.as_text().as_doc(),
        };

        let upper = if expr.assign().is_some() {
            str("=").append(upper)
        } else {
            upper
        };

        lower
            .append(break_("", ""))
            .append(str("..").append(upper))
            .group()
            .nest_if_broken(self.indent())
    }

    fn scope<'a>(&mut self, expr: ScopeExpr) -> Document<'a> {
        match expr.block() {
            Some(block) => self.statements(block.stmt(), expr.location()),
            None => string(expr.as_text()),
        }
    }

    fn switch<'a>(&mut self, expr: SwitchExpr) -> Document<'a> {
        let operand = match expr.expr() {
            Some(expr) => self.expression(expr),
            None => return expr.as_text().as_doc(),
        };

        let mut cases = Vec::new();
        for case in expr.arms() {
            let pattern = match case.pat() {
                Some(pat) => self.pattern(pat),
                None => return expr.as_text().as_doc(),
            };

            let branch = match case.expr() {
                Some(expr) => self.expression(expr),
                None => return expr.as_text().as_doc(),
            };

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

    fn variant<'a>(&mut self, expr: VariantExpr) -> Document<'a> {
        let Some(path) = expr.path() else {
            return expr.as_text().as_doc();
        };

        let Some(arg_list) = expr.arg_list() else {
            return expr.as_text().as_doc();
        };

        let name = self.path(path);
        let fields = arg_list.expr().collect_vec();

        if fields.is_empty() {
            return name;
        }

        let arguments = fields.into_iter().map(|arg| self.expression(arg)).collect_vec();
        let wrapped_arguments = self.wrap_comma_separated_of("(", ")", arguments);

        name.append(wrapped_arguments)
    }

    fn pattern<'a>(&mut self, pattern: Pat) -> Document<'a> {
        match &pattern {
            Pat::PatLiteral(lit) => match lit.literal() {
                Some(lit) => self.literal(lit),
                None => string(pattern.as_text()),
            },
            Pat::PatIdent(ident) => ident.name().as_doc(),
            Pat::PatVariant(variant) => {
                let Some(path) = variant.path() else {
                    return pattern.as_text().as_doc();
                };

                let name = self.path(path);
                let fields = variant.pat().collect_vec();

                if fields.is_empty() {
                    return name;
                }

                let fields = fields.into_iter().map(|field| self.pattern(field)).collect_vec();
                let wrapped_fields = self.wrap_comma_separated_of("(", ")", fields);

                name.append(wrapped_fields)
            }
            Pat::PatWildcard(_) => str(".."),
        }
    }

    fn ty<'a>(&mut self, ty: Type) -> Document<'a> {
        match ty {
            Type::NamedType(ty) => match ty.path() {
                Some(path) => self.path(path),
                None => string(ty.as_text()),
            },
            Type::SelfType(_) => str("Self"),
            Type::ArrayType(ty) => match ty.elemental() {
                Some(elemental) => concat(vec![str("["), self.ty(elemental), str("]")]),
                None => string(ty.as_text()),
            },
        }
    }

    fn ty_opt<'a>(&mut self, ty: Option<Type>) -> Document<'a> {
        ty.map_or(empty(), |ty| self.ty(ty))
    }

    fn type_arguments<'a, I>(&mut self, type_args: I) -> Document<'a>
    where
        I: IntoIterator<Item = Type>,
    {
        let type_args = type_args.into_iter().collect_vec();

        if type_args.is_empty() {
            return empty();
        }

        let types = type_args.into_iter().map(|ty| self.ty(ty)).collect::<Vec<_>>();

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

fn namespace<'ast>(namespace: Namespace) -> Document<'ast> {
    let Some(import_path) = namespace.import_path() else {
        return string(namespace.as_text());
    };

    let path = join(import_path.path().map(|seg| string(seg.as_text())), "::");

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

fn visibility<'a>(vis: Option<Visibility>) -> Document<'a> {
    match vis {
        Some(v) if v.internal_kw().is_some() => str("pub(internal) "),
        Some(v) if v.pub_kw().is_some() => str("pub "),
        Some(v) if v.priv_kw().is_some() => str("priv "),
        Some(v) => string(v.as_text()),
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
