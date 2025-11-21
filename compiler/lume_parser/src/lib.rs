use std::sync::Arc;

use error_snippet::{Error, Result};
use lume_ast::*;
use lume_errors::DiagCtxHandle;
use lume_lexer::{Token, TokenKind, TokenType};
use lume_span::SourceFile;

use crate::errors::*;

pub mod attr;
mod errors;
pub mod expr;
pub mod generic;
pub mod item;
pub mod path;
pub mod pattern;
pub mod stmt;
pub mod ty;

#[cfg(test)]
mod tests;

pub struct Parser<'src> {
    /// Defines the source code which is being parsed.
    source: Arc<SourceFile>,

    /// Handle to the diagnostics context which will handle parsing errors.
    dcx: DiagCtxHandle,

    /// Defines the index of the current token being processed, given in a
    /// zero-based index.
    index: usize,

    /// Defines the current position within the module source code, given in a
    /// zero-based index.
    position: usize,

    /// Defines all the tokens from the parsed source.
    tokens: Vec<Token<'src>>,

    /// Defines the documentation comment for the current item, if any.
    doc_token: Option<String>,

    /// Defines the attributes for the current item, if any.
    attributes: Option<Vec<Attribute>>,

    /// Defines whether the parser should attempt to recover from errors.
    attempt_recovery: bool,
}

impl<'src> Parser<'src> {
    /// Creates a new [`Parser`] instance with the given source file as
    /// it's input to parse.
    pub fn new(source: Arc<SourceFile>, mut tokens: Vec<Token<'src>>, dcx: DiagCtxHandle) -> Result<Self> {
        // Filter away any comment tokens, if present.
        tokens.retain(|t| t.as_type() != TokenType::Comment);

        Ok(Parser {
            source,
            dcx,
            index: 0,
            position: 0,
            tokens,
            doc_token: None,
            attributes: None,
            attempt_recovery: true,
        })
    }

    /// Disables the parser from attempting to recover from errors.
    #[libftrace::traced(level = Trace)]
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
    #[libftrace::traced(level = Debug, err)]
    pub fn prepare(&mut self) -> Result<()> {
        // If we've already tokenized something, we shouldn't add any more.
        if !self.tokens.is_empty() {
            libftrace::debug!("all tokens pre-lexed ({})", self.tokens.len());
            return Ok(());
        }

        libftrace::debug!("lexed {} tokens", self.tokens.len());

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
    #[libftrace::traced(level = Info, fields(file = self.source.name), err)]
    pub fn parse(&mut self) -> Result<Vec<TopLevelExpression>> {
        let mut expressions = Vec::new();

        loop {
            if self.eof() {
                break;
            }

            self.read_doc_comment()?;
            self.attributes = Some(self.parse_attributes()?);

            expressions.push(self.parse_top_level_expression()?);
        }

        self.dcx.ensure_untainted()?;

        Ok(expressions)
    }

    /// Determines whether the parser has reached the end-of-file.
    #[inline]
    fn eof(&self) -> bool {
        self.index + 1 > self.tokens.len() || self.token().kind == TokenKind::Eof
    }

    /// Parses a single token from the lexer at the given index.
    ///
    /// Returns the parsed token or a parsing error.
    fn token_at(&self, index: usize) -> Token<'src> {
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
            }
        }
    }

    /// Parses a single token from the lexer.
    ///
    /// Returns the parsed token or a parsing error.
    fn token(&self) -> Token<'src> {
        self.token_at(self.index)
    }

    /// Parses the previous token from the lexer.
    ///
    /// Returns the parsed token or a parsing error.
    fn previous_token(&self) -> Token<'src> {
        self.token_at(self.index - 1)
    }

    /// Parses the next token from the lexer.
    ///
    /// Returns the parsed token or a parsing error.
    fn next_token(&self) -> Token<'src> {
        self.token_at(self.index + 1)
    }

    /// Peeks the token from the lexer at some offset and returns it if it
    /// matches the expected kind.
    ///
    /// Returns a boolean indicating whether the token matches the expected
    /// kind.
    fn peek_offset(&self, kind: TokenType, offset: isize) -> bool {
        let token = self.token_at(self.index.saturating_add_signed(offset));

        token.kind.as_type() == kind
    }

    /// Peeks the next token from the lexer and returns it if it matches the
    /// expected kind.
    ///
    /// Returns a boolean indicating whether the token matches the expected
    /// kind.
    #[libftrace::traced(level = Trace)]
    fn peek_next(&self, kind: TokenType) -> bool {
        self.peek_offset(kind, 1)
    }

    /// Peeks the next token from the lexer and returns it if it matches the
    /// expected kind.
    ///
    /// Returns a boolean indicating whether the token matches the expected
    /// kind.
    #[libftrace::traced(level = Trace)]
    fn peek(&self, kind: TokenType) -> bool {
        self.peek_offset(kind, 0)
    }

    /// Advances the cursor position backwards by a single token.
    #[libftrace::traced(level = Trace)]
    fn rewind(&mut self) {
        self.index -= 1;
        self.position = self.token().start();
    }

    /// Advances the cursor position forward by a single token.
    #[libftrace::traced(level = Trace)]
    fn skip(&mut self) {
        self.index += 1;
        self.position = self.token().start();
    }

    /// Moves the current cursor position to the given index.
    #[libftrace::traced(level = Trace)]
    fn move_to(&mut self, index: usize) {
        self.index = index;
        self.position = self.token().start();
    }

    /// Moves the current cursor position to the given position.
    #[libftrace::traced(level = Trace)]
    fn move_to_pos(&mut self, pos: usize) {
        while self.position > pos {
            self.rewind();
        }
    }

    /// Consumes the next token from the lexer and returns it if it matches the
    /// expected kind.
    ///
    /// If the token does not match the expected kind, an error is returned.
    #[libftrace::traced(level = Trace)]
    fn expect(&mut self, kind: TokenType) -> Result<Token<'src>> {
        let current = self.token();

        if current.kind.as_type() == kind {
            Ok(current)
        } else {
            Err(UnexpectedToken {
                source: self.source.clone(),
                range: current.index,
                expected: kind,
                actual: current.kind.as_type(),
            }
            .into())
        }
    }

    /// Consumes the next token from the lexer and returns it if it matches the
    /// expected kind. Additionally, it advances the cursor position by a
    /// single token.
    ///
    /// If the token does not match the expected kind, an error is returned.
    #[libftrace::traced(level = Trace)]
    fn consume(&mut self, kind: TokenType) -> Result<Token<'src>> {
        match self.expect(kind) {
            Ok(token) => {
                self.skip();

                Ok(token)
            }
            Err(err) => Err(err),
        }
    }

    /// Consumes the next token from the lexer and returns it, not matter what
    /// token it is.
    #[libftrace::traced(level = Trace)]
    fn consume_any(&mut self) -> Token<'src> {
        let token = self.token();

        self.skip();

        token
    }

    /// Consumes the next token from the lexer and returns it if it matches the
    /// expected kind. Additionally, it advances the cursor position by a
    /// single token.
    ///
    /// If the token does not match the expected kind, `None` is returned.
    #[libftrace::traced(level = Trace)]
    fn consume_if(&mut self, kind: TokenType) -> Option<Token<'src>> {
        if self.eof() {
            return None;
        }

        let current = self.token();

        if current.kind.as_type() == kind {
            self.skip();

            Some(current)
        } else {
            None
        }
    }

    /// Invokes the given closure `f`, while preserving the start- and end-index
    /// of the consumed span. The span is returned as a `Location`, along with
    /// the result of the closure.
    fn consume_with_loc<T>(&mut self, mut f: impl FnMut(&mut Parser<'src>) -> Result<T>) -> Result<(T, Location)> {
        // Get the start-index of the current token, whatever it is.
        let start = self.token().start();

        let result = f(self)?;

        let end = self.token_at(self.index - 1).end();

        Ok((result, (start..end).into()))
    }

    /// Parses a sequence of statements, where the closure `f`
    /// is invoked, until the given "close" token is found.
    ///
    /// This is useful for any sequence of expressions or statements, such as
    /// blocks. Upon returning, the `close` token will have been consumed.
    #[libftrace::traced(level = Trace, err)]
    fn consume_seq_to_end<T>(
        &mut self,
        close: TokenType,
        mut f: impl FnMut(&mut Parser<'src>) -> Result<T>,
    ) -> Result<Vec<T>> {
        let mut v = Vec::new();

        while !self.check(close) {
            v.push(f(self)?);
        }

        Ok(v)
    }

    /// Parses a sequence of delimited statements, where the closure `f`
    /// is invoked between each delimiter, until no more delimiters are found.
    ///
    /// This is useful for any sequence of identifiers, such as namespace paths.
    #[libftrace::traced(level = Trace, err)]
    fn consume_delim<T>(
        &mut self,
        delim: TokenType,
        mut f: impl FnMut(&mut Parser<'src>) -> Result<T>,
    ) -> Result<Vec<T>> {
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
    /// is invoked between each delimiter, until the given "close" token is
    /// found.
    ///
    /// This is useful for any sequence of expressions or statements, such as
    /// arrays, parameters, etc. Upon returning, the `close` token will have
    /// been consumed.
    #[libftrace::traced(level = Trace, err)]
    fn consume_delim_seq_to_end<T>(
        &mut self,
        close: TokenType,
        delim: TokenType,
        mut f: impl FnMut(&mut Parser<'src>) -> Result<T>,
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

    /// Parses an enclosed sequence of delimited statements, where the closure
    /// `f` is invoked between each delimiter, until the given "close" token
    /// is found.
    ///
    /// This is useful for any sequence of expressions or statements, such as
    /// arrays, parameters, etc. Upon returning, the `close` token will have
    /// been consumed.
    #[libftrace::traced(level = Trace, err)]
    fn consume_enclosed_delim_seq<T>(
        &mut self,
        open: TokenType,
        close: TokenType,
        delim: TokenType,
        f: impl FnMut(&mut Parser<'src>) -> Result<T>,
    ) -> Result<Vec<T>> {
        self.consume(open)?;
        self.consume_delim_seq_to_end(close, delim, f)
    }

    /// Parses an enclosed sequence of comma-delimited statements, where the
    /// closure `f` is invoked between each delimiter, until the given
    /// "close" token is found.
    ///
    /// This is useful for any sequence of expressions or statements, such as
    /// arrays, parameters, etc. Upon returning, both the `open` and `close`
    /// tokens will have been consumed.
    #[libftrace::traced(level = Trace, err)]
    fn consume_comma_seq<T>(
        &mut self,
        open: TokenType,
        close: TokenType,
        f: impl FnMut(&mut Parser<'src>) -> Result<T>,
    ) -> Result<Vec<T>> {
        self.consume_enclosed_delim_seq(open, close, TokenType::Comma, f)
    }

    /// Parses a parenthesis-enclosed sequence of comma-delimited statements,
    /// where the closure `f` is invoked between each delimiter, until the
    /// given "close" token is found.
    ///
    /// This is useful for any sequence of expressions or statements, such as
    /// arrays, parameters, etc. Upon returning, both the `open` and `close`
    /// tokens will have been consumed.
    #[libftrace::traced(level = Trace, err)]
    fn consume_paren_seq<T>(&mut self, f: impl FnMut(&mut Parser<'src>) -> Result<T>) -> Result<Vec<T>> {
        self.consume_comma_seq(TokenType::LeftParen, TokenType::RightParen, f)
    }

    /// Parses a curly-braced enclosed sequence of statements, where the closure
    /// `f` is invoked, until the given "close" token is found.
    ///
    /// This is useful for any sequence of statements within a block.
    /// Upon returning, both the `open` and `close` tokens will have been
    /// consumed.
    #[libftrace::traced(level = Trace, err)]
    fn consume_curly_seq<T>(&mut self, f: impl FnMut(&mut Parser<'src>) -> Result<T>) -> Result<Vec<T>> {
        self.consume(TokenType::LeftCurly)?;
        self.consume_seq_to_end(TokenType::RightCurly, f)
    }

    /// Parses a a sequence of zero-or-more items, where the closure
    /// `f` is invoked, as long as the given `open` token is present.
    ///
    /// This is useful for when you need zero-or-more items without consuming
    /// any tokens, such as attributes.
    #[libftrace::traced(level = Trace, err)]
    fn consume_any_seq<T>(
        &mut self,
        open: TokenType,
        mut f: impl FnMut(&mut Parser<'src>) -> Result<T>,
    ) -> Result<Vec<T>> {
        let mut v = Vec::new();

        if self.peek(open) {
            v.push(f(self)?);
        }

        Ok(v)
    }

    /// Checks whether the current token is of the given type. If so, the token
    /// is consumed.
    #[libftrace::traced(level = Trace)]
    fn check(&mut self, kind: TokenType) -> bool {
        self.consume_if(kind).is_some()
    }

    /// Checks whether the current token is an `External` token.
    #[libftrace::traced(level = Trace)]
    fn check_external(&mut self) -> bool {
        self.check(TokenType::External)
    }

    /// Reads the current documentation comment into the parser's state, if any
    /// is present.
    #[libftrace::traced(level = Debug, err)]
    fn read_doc_comment(&mut self) -> Result<()> {
        let mut doc_comments = Vec::new();

        while let Some(doc_token) = self.consume_if(TokenType::DocComment)
            && let TokenKind::DocComment(value) = doc_token.kind
        {
            doc_comments.push(value);
        }

        if doc_comments.is_empty() {
            libftrace::trace!("no doc comment found");
            return Ok(());
        }

        self.doc_token = Some(
            doc_comments
                .into_iter()
                .map(|c| c.trim_start_matches("///").trim_start().to_string())
                .collect::<Vec<String>>()
                .join("\n"),
        );

        Ok(())
    }

    /// Asserts that the current token is an `fn` token.
    fn expect_fn(&mut self) -> Result<Token<'src>> {
        self.consume(TokenType::Fn)
    }

    /// Asserts that the current token is an `impl` token.
    fn expect_impl(&mut self) -> Result<Token<'src>> {
        self.consume(TokenType::Impl)
    }

    /// Asserts that the current token is a `;` token.
    fn expect_semi(&mut self) -> Result<Token<'src>> {
        self.consume(TokenType::Semicolon)
    }

    /// Parses the next token as an identifier.
    #[libftrace::traced(level = Trace)]
    fn parse_identifier(&mut self) -> Result<Identifier> {
        let identifier = match self.consume_any() {
            // Actual identifiers are obviously allowed, so they pass through.
            ident if ident.kind.as_type() == TokenType::Identifier => ident,

            // Keywords are also allowed, so reserved keywords can be used as identifiers.
            ident if ident.kind.is_keyword() => ident,

            ident => {
                return Err(ExpectedIdentifier {
                    source: self.source.clone(),
                    range: ident.index.clone(),
                    actual: ident.kind.as_type(),
                }
                .into());
            }
        };

        let location = identifier.index.clone();
        let slice = self.source.content.get(location.start..location.end).unwrap();

        Ok(Identifier {
            name: slice.to_string(),
            location: location.into(),
        })
    }

    /// Parses the next token as an identifier, optionally with an suffixed
    /// question mark.
    #[libftrace::traced(level = Trace)]
    fn parse_callable_name(&mut self) -> Result<Identifier> {
        let mut identifier = self.parse_identifier()?;

        if self.check(TokenType::Question) {
            identifier.name.push('?');
            identifier.location.0.end += 1;
        }

        Ok(identifier)
    }

    /// Parses the next token as an identifier. If the parsing fails, return
    /// `Err(err)`.
    #[libftrace::traced(level = Trace, err)]
    fn parse_ident_or_err(&mut self, err: Error) -> Result<Identifier> {
        match self.parse_identifier() {
            Ok(name) => Ok(name),
            Err(_) => Err(err),
        }
    }

    /// Parses the next token as an identifier, optionally with an suffixed
    /// question mark.
    ///
    /// If the parsing fails, return `Err(err)`.
    #[libftrace::traced(level = Trace, err)]
    fn parse_callable_name_or_err(&mut self, err: Error) -> Result<Identifier> {
        match self.parse_callable_name() {
            Ok(name) => Ok(name),
            Err(_) => Err(err),
        }
    }

    /// Parses the next token(s) as a namespace path.
    ///
    /// Identifier paths are much like regular identifiers, but can be joined
    /// together with periods, to form longer chains of them. They can be as
    /// short as a single link, such as `std`, but they can also be longer,
    /// such as `std::fmt::error`.
    #[libftrace::traced(level = Trace)]
    fn parse_import_path(&mut self) -> Result<ImportPath> {
        let segments = self.consume_delim(TokenType::PathSeparator, Parser::parse_identifier)?;

        let start = segments.first().unwrap().location.0.start;
        let end = segments.last().unwrap().location.0.end;

        let path = ImportPath {
            path: segments,
            location: (start..end).into(),
        };

        Ok(path)
    }

    /// Returns a block for functions or methods.
    #[libftrace::traced(level = Trace)]
    fn parse_block(&mut self) -> Result<Block> {
        let (statements, location) = self.consume_with_loc(|p| {
            let mut stmts = Vec::new();

            p.consume(TokenType::LeftCurly)?;

            while p.consume_if(TokenType::RightCurly).is_none() {
                match p.parse_statement() {
                    Ok(stmt) => stmts.push(stmt),
                    Err(err) => {
                        if !p.attempt_recovery {
                            return Err(err);
                        }

                        p.dcx.emit_and_push(err);

                        // If we couldn't recover, raise the error from before.
                        if !p.recover_statement() {
                            p.dcx.ensure_untainted()?;
                        }
                    }
                }
            }

            Ok(stmts)
        })?;

        Ok(Block { statements, location })
    }

    /// Returns an empty block for external functions and an actual block for
    /// non-external functions.
    #[libftrace::traced(level = Trace)]
    fn parse_opt_external_block(&mut self, external: bool) -> Result<Option<Block>> {
        if external {
            if self.peek(TokenType::LeftCurly) {
                return Err(ExternalFunctionBody {
                    source: self.source.clone(),
                    range: self.token().index,
                }
                .into());
            }

            Ok(None)
        } else {
            self.parse_block().map(Some)
        }
    }
}
