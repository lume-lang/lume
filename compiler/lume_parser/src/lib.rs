pub(crate) mod attr;
pub(crate) mod expr;
pub(crate) mod generic;
pub(crate) mod item;
pub(crate) mod path;
pub(crate) mod pattern;
pub(crate) mod recover;
pub(crate) mod stmt;
pub(crate) mod ty;

#[cfg(test)]
mod tests;

use std::sync::Arc;

use lume_errors::Result;
use lume_lexer::{Token, TokenKind};
use lume_span::SourceFile;
use lume_syntax::*;

/// Declares what the parser should parse.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Target {
    /// Parse top-level items, such as function definitions, structs, etc.
    #[default]
    Item,

    /// Parse statements, as if the parser started within a block.
    Statement,
}

pub struct Parser {
    /// Defines the source code which is being parsed.
    source: Arc<SourceFile>,

    /// Defines all the tokens from the parsed source.
    tokens: Vec<(SyntaxKind, TextSpan)>,

    builder: SyntaxTreeBuilder,
}

impl Parser {
    /// Creates a new parser instance from a source file.
    ///
    /// # Errors
    ///
    /// Returns `Err` if some part of the input source contains invalid or
    /// unsupported tokens.
    pub fn from_source(source: Arc<SourceFile>) -> Result<Self> {
        let mut lexer = lume_lexer::Lexer::new(source.clone());
        let tokens = lexer.lex()?;

        Ok(Self::from_tokens(source, tokens.into_iter()))
    }

    /// Creates a new parser instance from a set of pre-lexed tokens.
    pub fn from_tokens<'src, I>(source: Arc<SourceFile>, tokens: I) -> Self
    where
        I: DoubleEndedIterator<Item = Token<'src>>,
    {
        Self {
            source,
            tokens: tokens.into_iter().rev().map(as_parser_token).collect(),
            builder: SyntaxTreeBuilder::new(),
        }
    }

    /// Parses the module within the parser state.
    ///
    /// This function iterates through the tokens of the module source code,
    /// parsing each top-level expression and collecting them into a vector.
    pub fn parse(mut self, target: Target) -> SyntaxTree {
        let entrypoint = match target {
            Target::Item => Self::parse_item,
            Target::Statement => Self::parse_statement,
        };

        self.start_node(SyntaxKind::SOURCE_FILE);

        loop {
            if self.eof() {
                break;
            }

            entrypoint(&mut self);
        }

        self.finish_node();
        self.builder.finish()
    }

    /// Peeks the token at the current index, plus some offset.
    #[inline]
    fn token_at(&self, offset: usize) -> SyntaxKind {
        let non_trivia = self.non_trivia_pos(offset);

        self.tokens
            .get(non_trivia)
            .map_or(SyntaxKind::EOF, |(kind, _range)| *kind)
    }

    /// Gets the span of the current token.
    fn span(&self) -> TextSpan {
        let non_trivia = self.non_trivia_pos(0);

        self.tokens
            .get(non_trivia)
            .map_or(TextSpan(0, 0), |(_kind, range)| *range)
    }

    /// Gets the span at the current index, plus some offset.
    #[inline]
    fn span_at(&self, offset: usize) -> TextSpan {
        let non_trivia = self.non_trivia_pos(offset);

        self.tokens
            .get(non_trivia)
            .map_or(TextSpan(0, 0), |(_kind, range)| *range)
    }

    /// Peeks the current token, which is not yet parsed.
    ///
    /// If parser has already reached the end of input, returns
    /// [`SyntaxKind::EOF`].
    fn token(&self) -> SyntaxKind {
        self.token_at(0)
    }

    /// Gets a slice of the source code at the given span.
    fn content_at(&self, span: TextSpan) -> &str {
        self.source.content.get(span.0..span.1).unwrap_or("")
    }

    /// Determines whether the parser has reached the end-of-file.
    fn eof(&self) -> bool {
        self.token() == SyntaxKind::EOF
    }

    /// Reports an error at the current token location.
    fn error<M: Into<String>>(&mut self, message: M) {
        self.builder.errors.push(SyntaxError::new(message, self.span()));
    }

    /// Reports an error at the current token location and skips to the next
    /// token.
    fn error_and_skip<M: Into<String>>(&mut self, message: M) {
        self.error(message);
        self.skip();
    }

    /// Reports an error at the current token location.
    fn error_and_recover<M: Into<String>>(&mut self, message: M, set: &[SyntaxKind]) -> bool {
        self.error(message);
        self.recover_with_set(set)
    }

    /// Advances the cursor position forward by a single token.
    fn skip(&mut self) {
        self.consume_trivia();

        let _ = self.tokens.pop();
    }

    /// Peeks the next token from the lexer and returns [`true`] if it
    /// matches the expected kind.
    ///
    /// If the token does not match the expected kind, returns [`false`].
    fn peek(&self, kind: SyntaxKind) -> bool {
        self.token() == kind
    }

    /// Peeks the next token from the lexer, plus some offset, and returns
    /// [`true`] if it matches the expected kind.
    ///
    /// If the token does not match the expected kind, returns [`false`].
    fn peek_at(&self, offset: usize, kind: SyntaxKind) -> bool {
        self.token_at(offset) == kind
    }

    /// Peeks the next token from the lexer and returns [`true`] if it
    /// matches any of the expected kinds.
    ///
    /// If the token does not match any of the given syntax kinds, returns
    /// [`false`].
    fn peek_any(&self, kind: &[SyntaxKind]) -> bool {
        if kind.iter().any(|&k| self.peek(k)) {
            return true;
        }

        false
    }

    /// Gets the zero-based index of the next non-trivia token in
    /// the token queue.
    fn non_trivia_pos(&self, from: usize) -> usize {
        self.tokens
            .iter()
            .enumerate()
            .rev()
            .skip(from)
            .find_map(|(idx, (tok, _range))| if tok.is_trivia() { None } else { Some(idx) })
            .unwrap_or(0)
    }

    fn consume_trivia(&mut self) {
        while let Some(trivia) = self.tokens.pop_if(|(kind, _range)| kind.is_trivia()) {
            self.node_token(trivia.0, trivia.1);
        }
    }

    /// Checks the next token from the lexer and returns [`true`] if it
    /// matches the expected kind. Additionally, it advances the cursor
    /// position by a single token.
    ///
    /// If the token does not match the expected kind, returns [`false`] without
    /// advancing the cursor.
    fn check(&mut self, kind: SyntaxKind) -> bool {
        self.consume_trivia();

        if self.eof() {
            return false;
        }

        if let Some((kind, span)) = self.tokens.pop_if(|(tok, _span)| *tok == kind) {
            self.node_token(kind, span);
            return true;
        }

        false
    }

    /// Consume the next token from the lexer and returns [`true`] if it
    /// matches any of the expected kinds. Additionally, it advances the cursor
    /// position by a single token.
    ///
    /// If the token does not match any of the given syntax kinds, returns
    /// [`false`] without advancing the cursor.
    fn check_any(&mut self, kind: &[SyntaxKind]) -> bool {
        if kind.iter().any(|&k| self.check(k)) {
            return true;
        }

        false
    }

    /// Consume the next token from the lexer and returns [`true`] if it
    /// matches the expected kind. Additionally, it advances the cursor
    /// position by a single token.
    ///
    /// If the token does not match the expected kind, an error is raised.
    fn consume(&mut self, kind: SyntaxKind) -> bool {
        if self.check(kind) {
            return true;
        }

        self.error(format!("expected kind {kind:?}"));
        false
    }

    /// Consumes the current token, no matter it's kind.
    fn consume_any(&mut self) -> SyntaxKind {
        self.consume_trivia();

        if let Some((kind, span)) = self.tokens.pop() {
            self.node_token(kind, span);
            return kind;
        }

        SyntaxKind::EOF
    }

    /// Parses a sequence of delimited statements, where the closure `f`
    /// is invoked between each delimiter, until no more delimiters are found.
    ///
    /// This is useful for any sequence of identifiers, such as namespace paths.
    fn consume_delim(&mut self, delim: SyntaxKind, mut f: impl FnMut(&mut Parser)) {
        loop {
            f(self);

            if !self.check(delim) {
                break;
            }
        }
    }

    /// Parses a a sequence of zero-or-more items, where the closure
    /// `f` is invoked, as long as the given `open` token is present.
    ///
    /// This is useful for when you need zero-or-more items without consuming
    /// any tokens, such as attributes.
    fn consume_any_seq(&mut self, open: SyntaxKind, mut f: impl FnMut(&mut Parser)) -> bool {
        let mut any = false;

        while self.peek(open) {
            any = true;
            f(self);
        }

        any
    }

    /// Parses a sequence of statements, where the closure `f`
    /// is invoked, until the given "close" token is found.
    ///
    /// This is useful for any sequence of expressions or statements, such as
    /// blocks. Upon returning, the `close` token will have been consumed.
    fn consume_seq_to_end(&mut self, close: SyntaxKind, mut f: impl FnMut(&mut Parser)) {
        while !self.check(close) && !self.eof() {
            f(self);
        }
    }

    /// Parses a sequence of delimited statements, where the closure `f`
    /// is invoked between each delimiter, until the given "close" token is
    /// found.
    ///
    /// This is useful for any sequence of expressions or statements, such as
    /// arrays, parameters, etc. Upon returning, the `close` token will have
    /// been consumed.
    fn consume_delim_seq_to_end(
        &mut self,
        close: SyntaxKind,
        delim: SyntaxKind,
        mut f: impl FnMut(&mut Parser),
    ) -> bool {
        if self.check(close) {
            return true;
        }

        while !self.check(close) {
            f(self);

            if !self.check(delim) {
                if !self.check(close) {
                    self.error(format!("expected {delim:?}"));
                    return false;
                }

                break;
            }
        }

        true
    }

    /// Parses an enclosed sequence of delimited statements, where the closure
    /// `f` is invoked between each delimiter, until the given "close" token
    /// is found.
    ///
    /// This is useful for any sequence of expressions or statements, such as
    /// arrays, parameters, etc. Upon returning, the `close` token will have
    /// been consumed.
    fn consume_enclosed_delim_seq(
        &mut self,
        open: SyntaxKind,
        close: SyntaxKind,
        delim: SyntaxKind,
        f: impl FnMut(&mut Parser),
    ) -> bool {
        if self.consume(open) {
            self.consume_delim_seq_to_end(close, delim, f)
        } else {
            false
        }
    }

    /// Parses an enclosed sequence of comma-delimited statements, where the
    /// closure `f` is invoked between each delimiter, until the given
    /// "close" token is found.
    ///
    /// This is useful for any sequence of expressions or statements, such as
    /// arrays, parameters, etc. Upon returning, both the `open` and `close`
    /// tokens will have been consumed.
    fn consume_comma_seq(&mut self, open: SyntaxKind, close: SyntaxKind, f: impl FnMut(&mut Parser)) -> bool {
        self.consume_enclosed_delim_seq(open, close, Token![,], f)
    }

    /// Parses a parenthesis-enclosed sequence of comma-delimited statements,
    /// where the closure `f` is invoked between each delimiter, until the
    /// given "close" token is found.
    ///
    /// This is useful for any sequence of expressions or statements, such as
    /// arrays, parameters, etc. Upon returning, both the `open` and `close`
    /// tokens will have been consumed.
    fn consume_paren_seq(&mut self, f: impl FnMut(&mut Parser)) {
        self.consume_comma_seq(SyntaxKind::LEFT_PAREN, SyntaxKind::RIGHT_PAREN, f);
    }

    /// Parses a curly-braced enclosed sequence of statements, where the closure
    /// `f` is invoked, until the given "close" token is found.
    ///
    /// This is useful for any sequence of statements within a block.
    /// Upon returning, both the `open` and `close` tokens will have been
    /// consumed.
    fn consume_curly_seq(&mut self, f: impl FnMut(&mut Parser)) {
        if self.consume(SyntaxKind::LEFT_BRACE) {
            self.consume_seq_to_end(SyntaxKind::RIGHT_BRACE, f);
        }
    }

    /// Creates a new checkout for the current position.
    fn checkpoint(&mut self) -> Checkpoint {
        self.consume_trivia();
        self.builder.inner.checkpoint()
    }

    /// Adds a new token node to the tree.
    fn node_token(&mut self, kind: SyntaxKind, span: TextSpan) {
        let text = self.source.content.get(span.0..span.1).unwrap_or("");

        self.builder.token(kind, text);
    }

    /// Starts a new node.
    fn start_node(&mut self, kind: SyntaxKind) {
        self.with_trivia(|parser| {
            parser.builder.start_node(kind);
        });
    }

    /// Starts a new node at the given checkpoint.
    fn start_node_at(&mut self, kind: SyntaxKind, c: Checkpoint) {
        self.with_trivia(|parser| {
            parser.builder.start_node_at(kind, c);
        });
    }

    /// Finish the current node.
    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    /// Starts the given checkout and immedietly finishes it.
    fn complete_node(&mut self, kind: SyntaxKind, c: Checkpoint) -> SyntaxKind {
        self.start_node_at(kind, c);
        self.finish_node();

        kind
    }

    fn with_trivia<F: FnOnce(&mut Self)>(&mut self, f: F) {
        // Depending on whether we're at the first token or not, we might not
        // be able to consume any trivia tokens, since that would create multiple root
        // nodes (which is invalid).
        //
        // If this is the first token, consume the trivia *after* starting the node.
        let is_at_root = self.builder.stack().is_empty();

        if is_at_root {
            f(self);
            self.consume_trivia();
        } else {
            self.consume_trivia();
            f(self);
        }
    }

    /// Parses the next token as an identifier.
    fn parse_ident(&mut self) {
        self.start_node(SyntaxKind::NAME);

        match self.token() {
            // Actual identifiers are obviously allowed, so they pass through.
            SyntaxKind::IDENT => {
                self.node_token(SyntaxKind::IDENT, self.span());
                self.skip();
            }

            // Keywords are also allowed, so reserved keywords can be used as identifiers.
            ident if ident.is_keyword() => {
                self.node_token(SyntaxKind::IDENT, self.span());
                self.skip();
            }

            _ => {
                self.error_and_skip("expected identifier");
            }
        }

        self.finish_node();
    }

    /// Parses the next token as a callable name.
    fn parse_callable_ident(&mut self) {
        self.start_node(SyntaxKind::NAME);

        match self.token() {
            // Actual identifiers are obviously allowed, so they pass through.
            SyntaxKind::IDENT => {
                self.node_token(SyntaxKind::IDENT, self.span());
                self.skip();
            }

            // Keywords are also allowed, so reserved keywords can be used as identifiers.
            ident if ident.is_keyword() => {
                self.node_token(SyntaxKind::IDENT, self.span());
                self.skip();
            }

            _ => {
                self.error_and_skip("expected identifier");
            }
        }

        let _ = self.check(Token![?]);

        self.finish_node();
    }

    /// Reads the current documentation comment into the parser's state, if any
    /// is present.
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn parse_doc_comment(&mut self) {
        if !self.peek(SyntaxKind::DOC_COMMENT) {
            return;
        }

        while self.check(SyntaxKind::DOC_COMMENT) {}
    }
}

fn as_parser_token(token: Token<'_>) -> (SyntaxKind, TextSpan) {
    let kind = match token.kind {
        TokenKind::As => SyntaxKind::AS_KW,
        TokenKind::Add => SyntaxKind::ADD,
        TokenKind::AddAssign => SyntaxKind::ADDASSIGN,
        TokenKind::And => SyntaxKind::AND,
        TokenKind::Arrow => SyntaxKind::ARROW,
        TokenKind::ArrowBig => SyntaxKind::BIG_ARROW,
        TokenKind::Assign => SyntaxKind::ASSIGN,
        TokenKind::DocComment(_) => SyntaxKind::DOC_COMMENT,
        TokenKind::BinaryAnd => SyntaxKind::BINARY_AND,
        TokenKind::BinaryOr => SyntaxKind::BINARY_OR,
        TokenKind::BinaryXor => SyntaxKind::BINARY_XOR,
        TokenKind::Break => SyntaxKind::BREAK_KW,
        TokenKind::Colon => SyntaxKind::COLON,
        TokenKind::Comma => SyntaxKind::COMMA,
        TokenKind::Comment(_) => SyntaxKind::LINE_COMMENT,
        TokenKind::Continue => SyntaxKind::CONTINUE_KW,
        TokenKind::Decrement => SyntaxKind::DECREMENT,
        TokenKind::Div => SyntaxKind::DIV,
        TokenKind::DivAssign => SyntaxKind::DIVASSIGN,
        TokenKind::Dot => SyntaxKind::DOT,
        TokenKind::DotDot => SyntaxKind::DOT_DOT,
        TokenKind::DotDotDot => SyntaxKind::DOT_DOT_DOT,
        TokenKind::Else => SyntaxKind::ELSE_KW,
        TokenKind::Enum => SyntaxKind::ENUM_KW,
        TokenKind::Eof => SyntaxKind::EOF,
        TokenKind::Equal => SyntaxKind::EQUAL,
        TokenKind::Exclamation => SyntaxKind::NOT,
        TokenKind::External => SyntaxKind::EXTERN_KW,
        TokenKind::False => SyntaxKind::FALSE_KW,
        TokenKind::Fn => SyntaxKind::FN_KW,
        TokenKind::Float(_) => SyntaxKind::FLOAT_LIT,
        TokenKind::For => SyntaxKind::FOR_KW,
        TokenKind::Greater => SyntaxKind::GREATER,
        TokenKind::GreaterEqual => SyntaxKind::GEQUAL,
        TokenKind::Identifier(_) => SyntaxKind::IDENT,
        TokenKind::If => SyntaxKind::IF_KW,
        TokenKind::Impl => SyntaxKind::IMPL_KW,
        TokenKind::Import => SyntaxKind::IMPORT_KW,
        TokenKind::In => SyntaxKind::IN_KW,
        TokenKind::Is => SyntaxKind::IS_KW,
        TokenKind::Increment => SyntaxKind::INCREMENT,
        TokenKind::Integer(_) => SyntaxKind::INTEGER_LIT,
        TokenKind::Internal => SyntaxKind::INTERNAL_KW,
        TokenKind::LeftBracket => SyntaxKind::LEFT_BRACKET,
        TokenKind::LeftCurly => SyntaxKind::LEFT_BRACE,
        TokenKind::LeftParen => SyntaxKind::LEFT_PAREN,
        TokenKind::Less => SyntaxKind::LESS,
        TokenKind::LessEqual => SyntaxKind::LEQUAL,
        TokenKind::Let => SyntaxKind::LET_KW,
        TokenKind::Loop => SyntaxKind::LOOP_KW,
        TokenKind::Mul => SyntaxKind::MUL,
        TokenKind::MulAssign => SyntaxKind::MULASSIGN,
        TokenKind::Namespace => SyntaxKind::NAMESPACE_KW,
        TokenKind::NotEqual => SyntaxKind::NEQUAL,
        TokenKind::PathSeparator => SyntaxKind::PATH_SEP,
        TokenKind::Priv => SyntaxKind::PRIV_KW,
        TokenKind::Pub => SyntaxKind::PUB_KW,
        TokenKind::Question => SyntaxKind::QUESTION,
        TokenKind::Or => SyntaxKind::OR,
        TokenKind::Return => SyntaxKind::RETURN_KW,
        TokenKind::RightBracket => SyntaxKind::RIGHT_BRACKET,
        TokenKind::RightCurly => SyntaxKind::RIGHT_BRACE,
        TokenKind::RightParen => SyntaxKind::RIGHT_PAREN,
        TokenKind::Semicolon => SyntaxKind::SEMICOLON,
        TokenKind::SelfRef => SyntaxKind::SELF_EXPR,
        TokenKind::SelfType => SyntaxKind::SELF_TYPE,
        TokenKind::String(_) => SyntaxKind::STRING_LIT,
        TokenKind::Struct => SyntaxKind::STRUCT_KW,
        TokenKind::Sub => SyntaxKind::SUB,
        TokenKind::SubAssign => SyntaxKind::SUBASSIGN,
        TokenKind::Switch => SyntaxKind::SWITCH_KW,
        TokenKind::Trait => SyntaxKind::TRAIT_KW,
        TokenKind::True => SyntaxKind::TRUE_KW,
        TokenKind::Unsafe => SyntaxKind::UNSAFE_KW,
        TokenKind::Use => SyntaxKind::USE_KW,
        TokenKind::While => SyntaxKind::WHILE_KW,
        TokenKind::Whitespace(_) => SyntaxKind::WHITESPACE,
    };

    (kind, TextSpan(token.index.start, token.index.end))
}
