#![feature(string_remove_matches)]

use std::sync::Arc;

use crate::errors::*;
use error_snippet::{Error, Result};
use lume_ast::*;
use lume_errors::DiagCtxHandle;
use lume_lexer::IDENTIFIER_SEPARATOR;
use lume_lexer::{Lexer, Token, TokenKind};
use lume_span::{SourceFile, SourceFileId, SourceMap};

mod errors;
pub mod expr;
pub mod generic;
pub mod item;
pub mod path;
pub mod stmt;
pub mod ty;

#[cfg(test)]
mod tests;

pub struct Parser {
    /// Defines the source code which is being parsed.
    source: Arc<SourceFile>,

    /// Handle to the diagnostics context which will handle parsing errors.
    dcx: DiagCtxHandle,

    /// Defines the lexer which tokenizes the module source code.
    lexer: Lexer,

    /// Defines the index of the current token being processed, given in a zero-based index.
    index: usize,

    /// Defines the current position within the module source code, given in a zero-based index.
    position: usize,

    /// Defines all the tokens from the parsed source.
    tokens: Vec<Token>,

    /// Defines the documentation comment for the current item, if any.
    doc_token: Option<String>,

    /// Defines whether the parser should attempt to recover from errors.
    attempt_recovery: bool,
}

#[macro_export]
macro_rules! err {
    (
        $self:expr,
        $kind:ident $(,)?
        $(
            $field: ident,
            $value: expr
        ),*
    ) => {
        $kind {
            source: $self.source.clone(),
            range: match $self.tokens.get($self.index) {
                Some(token) => token.index.clone(),
                None => (0..$self.index)
            },
            $( $field: $value ),*
        }
        .into()
    };
}

impl Parser {
    /// Creates a new [`Parser`] instance with the given source file as
    /// it's input to parse.
    pub fn new(source: Arc<SourceFile>, dcx: DiagCtxHandle) -> Self {
        let lexer = Lexer::new(source.clone());

        Parser {
            source,
            lexer,
            dcx,
            index: 0,
            position: 0,
            tokens: Vec::new(),
            doc_token: None,
            attempt_recovery: true,
        }
    }

    /// Creates a new [`Parser`] for the given source code.
    ///
    /// This function doesn't perform any parsing itself - it simply readies
    /// a parser to interate over tokens from it's inner lexer.
    pub fn new_with_str(str: &str) -> Parser {
        let source = SourceFile::internal(str);
        let dcx = DiagCtxHandle::shim();

        Parser::new(Arc::new(source), dcx)
    }

    /// Creates a new [`Parser`] for the given source code.
    ///
    /// This function doesn't perform any parsing itself - it simply readies
    /// a parser to interate over tokens from it's inner lexer.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `file` is not found within `state`.
    pub fn new_with_src(sources: &SourceMap, file: SourceFileId) -> Result<Parser> {
        let source_file = sources.get_or_err(file)?;
        let dcx = DiagCtxHandle::shim();

        Ok(Parser::new(source_file, dcx))
    }

    /// Parses the given source.
    ///
    /// This function iterates through the tokens of the module source code,
    /// parsing each top-level expression and collecting them into a vector.
    ///
    /// # Errors
    ///
    /// Returns `Err` if some part of the input is unexpected or if the
    /// parser unexpectedly reaches end-of-file.
    pub fn parse_str(str: &str) -> Result<Vec<TopLevelExpression>> {
        let mut parser = Parser::new_with_str(str);
        parser.disable_recovery();

        parser.parse()
    }

    /// Parses the given source text.
    ///
    /// This function iterates through the tokens of the module source code,
    /// parsing each top-level expression and collecting them into a vector.
    ///
    /// # Errors
    ///
    /// Returns `Err` if some part of the input is unexpected or if the
    /// parser unexpectedly reaches end-of-file.
    pub fn parse_src(sources: &SourceMap, file: SourceFileId, dcx: DiagCtxHandle) -> Result<Vec<TopLevelExpression>> {
        let source_file = sources.get_or_err(file)?;
        let mut parser = Parser::new(source_file, dcx);
        parser.disable_recovery();

        parser.parse()
    }

    /// Disables the parser from attempting to recover from errors.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn disable_recovery(&mut self) {
        self.attempt_recovery = false;
    }

    /// Prepares the parser to being parsing.
    ///
    /// This function iterates through the tokens of the module source code
    /// from the lexer and moves them into the parser.
    ///
    /// # Errors
    ///
    /// Returns `Err` if some part of the input source is unsupported
    /// or unexpected.
    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    pub fn prepare(&mut self) -> Result<()> {
        // If we've already tokenized something, we shouldn't add any more.
        if !self.tokens.is_empty() {
            tracing::debug!("all tokens pre-lexed ({})", self.tokens.len());
            return Ok(());
        }

        // Pre-tokenize the input source text.
        loop {
            let token = self.lexer.next_token()?;

            match token.kind {
                TokenKind::Comment => continue,
                TokenKind::Eof => break,
                _ => {}
            }

            self.tokens.push(token);
        }

        tracing::debug!("lexed {} tokens", self.tokens.len());

        Ok(())
    }

    /// Parses the module within the parser state.
    ///
    /// This function iterates through the tokens of the module source code,
    /// parsing each top-level expression and collecting them into a vector.
    ///
    /// # Errors
    ///
    /// Returns `Err` if some part of the input is unexpected or if the
    /// parser unexpectedly reaches end-of-file.
    #[tracing::instrument(
        level = "INFO",
        name = "lume_parser::parser::parse",
        parent = None,
        skip(self),
        fields(file = %self.source.name)
        err
    )]
    pub fn parse(&mut self) -> Result<Vec<TopLevelExpression>> {
        self.prepare()?;

        let mut expressions = Vec::new();

        loop {
            if self.eof() {
                break;
            }

            self.read_doc_comment();

            expressions.push(self.parse_top_level_expression()?);
        }

        Ok(expressions)
    }

    /// Determines whether the parser has reached the end-of-file.
    #[inline]
    fn eof(&self) -> bool {
        self.index + 1 > self.tokens.len()
    }

    /// Parses a single token from the lexer at the given index.
    ///
    /// Returns the parsed token or a parsing error.
    fn token_at(&self, index: usize) -> Token {
        if let Some(token) = self.tokens.get(index) {
            token.clone()
        } else {
            let index = match self.tokens.last() {
                Some(t) => t.index.clone(),
                None => 0..1,
            };

            Token {
                kind: TokenKind::Eof,
                index,
                ty: None,
                value: None,
            }
        }
    }

    /// Parses a single token from the lexer.
    ///
    /// Returns the parsed token or a parsing error.
    fn token(&self) -> Token {
        self.token_at(self.index)
    }

    /// Parses the previous token from the lexer.
    ///
    /// Returns the parsed token or a parsing error.
    fn previous_token(&self) -> Token {
        self.token_at(self.index - 1)
    }

    /// Parses the last token from the lexer.
    ///
    /// Returns the parsed token or a parsing error.
    fn last_token(&self) -> Token {
        self.token_at(self.tokens.len() - 1)
    }

    /// Peeks the token from the lexer at some offset and returns it if it matches the expected kind.
    ///
    /// Returns a boolean indicating whether the token matches the expected kind.
    fn peek_offset(&self, kind: TokenKind, offset: isize) -> bool {
        let token = self.token_at(self.index.saturating_add_signed(offset));

        token.kind == kind
    }

    /// Peeks the next token from the lexer and returns it if it matches the expected kind.
    ///
    /// Returns a boolean indicating whether the token matches the expected kind.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn peek_next(&self, kind: TokenKind) -> bool {
        self.peek_offset(kind, 1)
    }

    /// Peeks the next token from the lexer and returns it if it matches the expected kind.
    ///
    /// Returns a boolean indicating whether the token matches the expected kind.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn peek(&self, kind: TokenKind) -> bool {
        self.peek_offset(kind, 0)
    }

    /// Advances the cursor position backwards by a single token.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn rewind(&mut self) {
        self.index -= 1;
        self.position = self.token().start();
    }

    /// Advances the cursor position forward by a single token.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn skip(&mut self) {
        self.index += 1;
        self.position = self.token().start();
    }

    /// Moves the current cursor position to the given index.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn move_to(&mut self, index: usize) {
        self.index = index;
        self.position = self.token().start();
    }

    /// Moves the current cursor position to the given position.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn move_to_pos(&mut self, pos: usize) {
        while self.position > pos {
            self.rewind();
        }
    }

    /// Consumes the next token from the lexer and returns it if it matches the expected kind.
    ///
    /// If the token does not match the expected kind, an error is returned.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        let current = self.token();

        if current.kind == kind {
            Ok(current)
        } else {
            Err(err!(self, UnexpectedToken, expected, kind, actual, current.kind))
        }
    }

    /// Consumes the next token from the lexer and returns it if it matches the expected kind. Additionally,
    /// it advances the cursor position by a single token.
    ///
    /// If the token does not match the expected kind, an error is returned.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn consume(&mut self, kind: TokenKind) -> Result<Token> {
        match self.expect(kind) {
            Ok(token) => {
                self.skip();

                Ok(token)
            }
            Err(err) => Err(err),
        }
    }

    /// Consumes the next token from the lexer and returns it, not matter what token it is.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn consume_any(&mut self) -> Token {
        let token = self.token();

        self.skip();

        token
    }

    /// Consumes the next token from the lexer and returns it if it matches the expected kind. Additionally,
    /// it advances the cursor position by a single token.
    ///
    /// If the token does not match the expected kind, `None` is returned.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn consume_if(&mut self, kind: TokenKind) -> Option<Token> {
        if self.eof() {
            return None;
        }

        let current = self.token();

        if current.kind == kind {
            self.skip();

            Some(current)
        } else {
            None
        }
    }

    /// Invokes the given closure `f`, while preserving the start- and end-index
    /// of the consumed span. The span is returned as a `Location`, along with the
    /// result of the closure.
    fn consume_with_loc<T>(&mut self, mut f: impl FnMut(&mut Parser) -> Result<T>) -> Result<(T, Location)> {
        // Get the start-index of the current token, whatever it is.
        let start = self.token().start();

        let result = f(self)?;

        let end = self.token_at(self.index - 1).end();

        Ok((result, (start..end).into()))
    }

    /// Parses a sequence of statements, where the closure `f`
    /// is invoked, until the given "close" token is found.
    ///
    /// This is useful for any sequence of expressions or statements, such as blocks.
    /// Upon returning, the `close` token will have been consumed.
    #[tracing::instrument(level = "TRACE", skip(self, f), err)]
    fn consume_seq_to_end<T>(
        &mut self,
        close: TokenKind,
        mut f: impl FnMut(&mut Parser) -> Result<T>,
    ) -> Result<Vec<T>> {
        let mut v = Vec::new();

        while self.consume_if(close).is_none() {
            v.push(f(self)?);
        }

        Ok(v)
    }

    /// Parses a sequence of delimited statements, where the closure `f`
    /// is invoked between each delimiter, until no more delimiters are found.
    ///
    /// This is useful for any sequence of identifiers, such as namespace paths.
    #[tracing::instrument(level = "TRACE", skip(self, f), err)]
    fn consume_delim<T>(&mut self, delim: TokenKind, mut f: impl FnMut(&mut Parser) -> Result<T>) -> Result<Vec<T>> {
        let mut v = Vec::new();

        loop {
            v.push(f(self)?);

            if self.consume_if(delim).is_none() {
                break;
            }
        }

        Ok(v)
    }

    /// Parses a sequence of delimited statements, where the closure `f`
    /// is invoked between each delimiter, until the given "close" token is found.
    ///
    /// This is useful for any sequence of expressions or statements, such as arrays,
    /// parameters, etc. Upon returning, the `close` token will have been consumed.
    #[tracing::instrument(level = "TRACE", skip(self, f), err)]
    fn consume_delim_seq_to_end<T>(
        &mut self,
        close: TokenKind,
        delim: TokenKind,
        mut f: impl FnMut(&mut Parser) -> Result<T>,
    ) -> Result<Vec<T>> {
        let mut v = Vec::new();

        if self.check(close) {
            return Ok(v);
        }

        while !self.check(close) {
            v.push(f(self)?);

            if !self.check(delim) {
                if !self.check(close) {
                    return Err(MissingDelimiterInSequence {
                        source: self.source.clone(),
                        range: self.token().index,
                        delimiter: delim,
                    }
                    .into());
                }

                break;
            }
        }

        Ok(v)
    }

    /// Parses an enclosed sequence of delimited statements, where the closure `f`
    /// is invoked between each delimiter, until the given "close" token is found.
    ///
    /// This is useful for any sequence of expressions or statements, such as arrays,
    /// parameters, etc. Upon returning, the `close` token will have been consumed.
    #[tracing::instrument(level = "TRACE", skip(self, f), err)]
    fn consume_enclosed_delim_seq<T>(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        delim: TokenKind,
        f: impl FnMut(&mut Parser) -> Result<T>,
    ) -> Result<Vec<T>> {
        self.consume(open)?;
        self.consume_delim_seq_to_end(close, delim, f)
    }

    /// Parses an enclosed sequence of comma-delimited statements, where the closure `f`
    /// is invoked between each delimiter, until the given "close" token is found.
    ///
    /// This is useful for any sequence of expressions or statements, such as arrays,
    /// parameters, etc. Upon returning, both the `open` and `close` tokens will have been consumed.
    #[tracing::instrument(level = "TRACE", skip(self, f), err)]
    fn consume_comma_seq<T>(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        f: impl FnMut(&mut Parser) -> Result<T>,
    ) -> Result<Vec<T>> {
        self.consume_enclosed_delim_seq(open, close, TokenKind::Comma, f)
    }

    /// Parses a parenthesis-enclosed sequence of comma-delimited statements, where the closure `f`
    /// is invoked between each delimiter, until the given "close" token is found.
    ///
    /// This is useful for any sequence of expressions or statements, such as arrays,
    /// parameters, etc. Upon returning, both the `open` and `close` tokens will have been consumed.
    #[tracing::instrument(level = "TRACE", skip(self, f), err)]
    fn consume_paren_seq<T>(&mut self, f: impl FnMut(&mut Parser) -> Result<T>) -> Result<Vec<T>> {
        self.consume_comma_seq(TokenKind::LeftParen, TokenKind::RightParen, f)
    }

    /// Parses a curly-braced enclosed sequence of statements, where the closure `f`
    /// is invoked, until the given "close" token is found.
    ///
    /// This is useful for any sequence of statements within a block.
    /// Upon returning, both the `open` and `close` tokens will have been consumed.
    #[tracing::instrument(level = "TRACE", skip(self, f), err)]
    fn consume_curly_seq<T>(&mut self, f: impl FnMut(&mut Parser) -> Result<T>) -> Result<Vec<T>> {
        self.consume(TokenKind::LeftCurly)?;
        self.consume_seq_to_end(TokenKind::RightCurly, f)
    }

    /// Checks whether the current token is of the given type. If so, the token is consumed.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn check(&mut self, kind: TokenKind) -> bool {
        self.consume_if(kind).is_some()
    }

    /// Checks whether the current token is an `External` token.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn check_external(&mut self) -> bool {
        self.check(TokenKind::External)
    }

    /// Reads the current documentation comment into the parser's state, if any is present.
    fn read_doc_comment(&mut self) {
        if let Some(doc_comment) = self.consume_if(TokenKind::DocComment) {
            tracing::trace!("found doc comment ({:?})", doc_comment.index);

            self.doc_token = Some(doc_comment.value.unwrap_or_default());
        } else {
            tracing::trace!("no doc comment found");
        }
    }

    /// Asserts that the current token is an `fn` token.
    fn expect_fn(&mut self) -> Result<Token> {
        self.consume(TokenKind::Fn)
    }

    /// Asserts that the current token is an `impl` token.
    fn expect_impl(&mut self) -> Result<Token> {
        self.consume(TokenKind::Impl)
    }

    /// Asserts that the current token is a `;` token.
    fn expect_semi(&mut self) -> Result<Token> {
        self.consume(TokenKind::Semicolon)
    }

    /// Parses the next token as an identifier.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn parse_identifier(&mut self) -> Result<Identifier> {
        let identifier = match self.consume_any() {
            // Actual identifiers are obviously allowed, so they pass through.
            ident if ident.kind == TokenKind::Identifier => ident,

            // Keywords are also allowed, so reserved keywords can be used as identifiers.
            ident if ident.kind.is_keyword() => ident,

            ident => {
                return Err(ExpectedIdentifier {
                    source: self.source.clone(),
                    range: ident.index.clone(),
                    actual: ident.kind,
                }
                .into());
            }
        };

        let location = identifier.index.clone();
        let name = match identifier {
            v if v.kind == TokenKind::Identifier => v.value.unwrap(),
            v => v.into(),
        };

        Ok(Identifier {
            name,
            location: location.into(),
        })
    }

    /// Parses the next token as an identifier, optionally with an suffixed question mark.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn parse_callable_name(&mut self) -> Result<Identifier> {
        let mut identifier = self.parse_identifier()?;

        if self.check(TokenKind::Question) {
            identifier.name.push('?');
            identifier.location.0.end += 1;
        }

        Ok(identifier)
    }

    /// Parses the next token as an identifier. If the parsing fails, return `Err(err)`.
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn parse_ident_or_err(&mut self, err: Error) -> Result<Identifier> {
        match self.parse_identifier() {
            Ok(name) => Ok(name),
            Err(_) => Err(err),
        }
    }

    /// Parses the next token as an identifier, optionally with an suffixed question mark.
    ///
    /// If the parsing fails, return `Err(err)`.
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn parse_callable_name_or_err(&mut self, err: Error) -> Result<Identifier> {
        match self.parse_callable_name() {
            Ok(name) => Ok(name),
            Err(_) => Err(err),
        }
    }

    /// Parses the next token(s) as a namespace path.
    ///
    /// Identifier paths are much like regular identifiers, but can be joined together
    /// with periods, to form longer chains of them. They can be as short as a single
    /// link, such as `std`, but they can also be longer, such as `std::fmt::error`.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn parse_import_path(&mut self) -> Result<ImportPath> {
        let segments = self.consume_delim(IDENTIFIER_SEPARATOR, Parser::parse_identifier)?;

        let start = segments.first().unwrap().location.0.start;
        let end = segments.last().unwrap().location.0.end;

        let path = ImportPath {
            path: segments,
            location: (start..end).into(),
        };

        Ok(path)
    }

    /// Returns a block for functions or methods.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn parse_block(&mut self) -> Result<Block> {
        let (statements, location) = self.consume_with_loc(|p| {
            let mut stmts = Vec::new();

            p.consume(TokenKind::LeftCurly)?;

            while p.consume_if(TokenKind::RightCurly).is_none() {
                match p.parse_statement() {
                    Ok(stmt) => stmts.push(stmt),
                    Err(err) => {
                        if !p.attempt_recovery {
                            return Err(err);
                        }

                        p.dcx.emit(err);

                        p.recover_statement();
                    }
                }
            }

            Ok(stmts)
        })?;

        Ok(Block { statements, location })
    }

    /// Returns an empty block for external functions.
    ///
    /// Also functions as an extra layer to report errors, if a function body is declared.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn parse_external_block(&mut self) -> Result<Block> {
        if self.peek(TokenKind::LeftCurly) {
            return Err(err!(self, ExternalFunctionBody));
        }

        if self.eof() {
            let last_token_end = self.last_token().end();

            return Ok(Block::from_location(last_token_end..last_token_end));
        }

        Ok(Block::from_location(self.token().index))
    }

    /// Returns an empty block for external functions and an actual block for non-external functions.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn parse_opt_external_block(&mut self, external: bool) -> Result<Block> {
        if external {
            self.parse_external_block()
        } else {
            self.parse_block()
        }
    }
}
