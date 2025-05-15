#![feature(string_remove_matches)]

use std::sync::Arc;

use crate::errors::*;
use error_snippet::{Error, Result};
use lume_ast::*;
use lume_errors::DiagCtxHandle;
use lume_lexer::IDENTIFIER_SEPARATOR;
use lume_lexer::{Lexer, Token, TokenKind};
use lume_span::{SourceFile, SourceFileId};

mod errors;
pub mod expr;
pub mod generic;
pub mod item;
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
    pub fn new_with_src(state: &lume_state::State, file: SourceFileId) -> Result<Parser> {
        let source = state.source_of(file)?;
        let dcx = DiagCtxHandle::shim();

        Ok(Parser::new(source, dcx))
    }

    /// Parses the given source.
    ///
    /// This function iterates through the tokens of the module source code,
    /// parsing each top-level expression and collecting them into a vector.
    pub fn parse_str(str: &str) -> Result<Vec<TopLevelExpression>> {
        let mut parser = Parser::new_with_str(str);

        parser.parse()
    }

    /// Parses the given source text.
    ///
    /// This function iterates through the tokens of the module source code,
    /// parsing each top-level expression and collecting them into a vector.
    pub fn parse_src(state: &mut lume_state::State, file: SourceFileId) -> Result<Vec<TopLevelExpression>> {
        let mut parser = Parser::new_with_src(state, file)?;
        parser.dcx = state.dcx_mut().handle();

        parser.parse()
    }

    /// Prepares the parser to being parsing.
    ///
    /// This function iterates through the tokens of the module source code
    /// from the lexer and moves them into the parser.
    pub fn prepare(&mut self) -> Result<()> {
        // If we've already tokenized something, we shouldn't add any more.
        if !self.tokens.is_empty() {
            return Ok(());
        }

        // Pre-tokenize the input source text.
        loop {
            let token = self.lexer.next_token()?;

            match token.kind {
                TokenKind::Comment => continue,
                TokenKind::Eof => break,
                _ => {}
            };

            self.tokens.push(token);
        }

        Ok(())
    }

    /// Parses the module within the parser state.
    ///
    /// This function iterates through the tokens of the module source code,
    /// parsing each top-level expression and collecting them into a vector.
    pub fn parse(&mut self) -> Result<Vec<TopLevelExpression>> {
        self.prepare()?;

        let mut expressions = Vec::new();

        loop {
            if self.eof() {
                break;
            }

            self.read_doc_comment()?;

            expressions.push(self.parse_top_level_expression()?);
        }

        Ok(expressions)
    }

    /// Determines whether the parser has reached the end-of-file.
    fn eof(&self) -> bool {
        self.index + 1 > self.tokens.len()
    }

    /// Parses a single token from the lexer at the given index.
    ///
    /// Returns the parsed token or a parsing error.
    fn token_at(&self, index: usize) -> Result<Token> {
        match self.tokens.get(index) {
            Some(token) => Ok(token.clone()),
            None => {
                let index = match self.tokens.last() {
                    Some(t) => t.index.clone(),
                    None => 0..1,
                };

                Ok(Token {
                    kind: TokenKind::Eof,
                    index,
                    ty: None,
                    value: None,
                })
            }
        }
    }

    /// Parses a single token from the lexer.
    ///
    /// Returns the parsed token or a parsing error.
    fn token(&self) -> Result<Token> {
        self.token_at(self.index)
    }

    /// Parses the previous token from the lexer.
    ///
    /// Returns the parsed token or a parsing error.
    fn previous_token(&self) -> Result<Token> {
        self.token_at(self.index - 1)
    }

    /// Parses the last token from the lexer.
    ///
    /// Returns the parsed token or a parsing error.
    fn last_token(&self) -> Result<Token> {
        self.token_at(self.tokens.len() - 1)
    }

    /// Peeks the token from the lexer at some offset and returns it if it matches the expected kind.
    ///
    /// Returns a boolean indicating whether the token matches the expected kind.
    fn peek_offset(&self, kind: TokenKind, offset: isize) -> Result<bool> {
        let token = match self.token_at(self.index.saturating_add_signed(offset)) {
            Ok(token) => token,
            Err(_) => return Ok(false),
        };

        if token.kind == kind {
            return Ok(true);
        }

        Ok(false)
    }

    /// Peeks the next token from the lexer and returns it if it matches the expected kind.
    ///
    /// Returns a boolean indicating whether the token matches the expected kind.
    fn peek_next(&self, kind: TokenKind) -> Result<bool> {
        self.peek_offset(kind, 1)
    }

    /// Peeks the next token from the lexer and returns it if it matches the expected kind.
    ///
    /// Returns a boolean indicating whether the token matches the expected kind.
    fn peek(&self, kind: TokenKind) -> Result<bool> {
        self.peek_offset(kind, 0)
    }

    /// Advances the cursor position by a single token.
    fn skip(&mut self) -> Result<()> {
        self.position += self.token()?.len();
        self.index += 1;

        Ok(())
    }

    /// Consumes the next token from the lexer and returns it if it matches the expected kind.
    ///
    /// If the token does not match the expected kind, an error is returned.
    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        let current = self.token()?;

        match current.kind == kind {
            true => Ok(current),
            false => Err(err!(self, UnexpectedToken, expected, kind, actual, current.kind)),
        }
    }

    /// Consumes the next token from the lexer and returns it if it matches the expected kind. Additionally,
    /// it advances the cursor position by a single token.
    ///
    /// If the token does not match the expected kind, an error is returned.
    fn consume(&mut self, kind: TokenKind) -> Result<Token> {
        match self.expect(kind) {
            Ok(token) => {
                self.skip()?;

                Ok(token)
            }
            Err(err) => Err(err),
        }
    }

    /// Consumes the next token from the lexer and returns it, not matter what token it is.
    fn consume_any(&mut self) -> Result<Token> {
        let token = self.token()?;

        self.skip()?;

        Ok(token)
    }

    /// Consumes the next token from the lexer and returns it if it matches the expected kind. Additionally,
    /// it advances the cursor position by a single token.
    ///
    /// If the token does not match the expected kind, `None` is returned.
    fn consume_if(&mut self, kind: TokenKind) -> Result<Option<Token>> {
        if self.eof() {
            return Ok(None);
        }

        let current = self.token()?;

        match current.kind == kind {
            true => {
                self.skip()?;

                Ok(Some(current))
            }
            false => Ok(None),
        }
    }

    /// Invokes the given closure `f`, while preserving the start- and end-index
    /// of the consumed span. The span is returned as a `Location`, along with the
    /// result of the closure.
    fn consume_with_loc<T>(&mut self, mut f: impl FnMut(&mut Parser) -> Result<T>) -> Result<(T, Location)> {
        // Get the start-index of the current token, whatever it is.
        let start = self.token()?.start();

        let result = f(self)?;

        let end = self.token_at(self.index - 1)?.end();

        Ok((result, (start..end).into()))
    }

    /// Parses a sequence of statements, where the closure `f`
    /// is invoked, until the given "close" token is found.
    ///
    /// This is useful for any sequence of expressions or statements, such as blocks.
    /// Upon returning, the `close` token will have been consumed.
    fn consume_seq_to_end<T>(
        &mut self,
        close: TokenKind,
        mut f: impl FnMut(&mut Parser) -> Result<T>,
    ) -> Result<Vec<T>> {
        let mut v = Vec::new();

        while self.consume_if(close)?.is_none() {
            v.push(f(self)?);
        }

        Ok(v)
    }

    /// Parses a sequence of delimited statements, where the closure `f`
    /// is invoked between each delimiter, until no more delimiters are found.
    ///
    /// This is useful for any sequence of identifiers, such as namespace paths.
    fn consume_delim<T>(&mut self, delim: TokenKind, mut f: impl FnMut(&mut Parser) -> Result<T>) -> Result<Vec<T>> {
        let mut v = Vec::new();

        loop {
            v.push(f(self)?);

            if self.consume_if(delim)?.is_none() {
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
    fn consume_delim_seq_to_end<T>(
        &mut self,
        close: TokenKind,
        delim: TokenKind,
        mut f: impl FnMut(&mut Parser) -> Result<T>,
    ) -> Result<Vec<T>> {
        let mut v = Vec::new();

        while self.consume_if(close)?.is_none() {
            if !v.is_empty() && !self.peek(close)? {
                self.consume(delim)?;
            }

            v.push(f(self)?);
        }

        Ok(v)
    }

    /// Parses an enclosed sequence of delimited statements, where the closure `f`
    /// is invoked between each delimiter, until the given "close" token is found.
    ///
    /// This is useful for any sequence of expressions or statements, such as arrays,
    /// parameters, etc. Upon returning, the `close` token will have been consumed.
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
    fn consume_paren_seq<T>(&mut self, f: impl FnMut(&mut Parser) -> Result<T>) -> Result<Vec<T>> {
        self.consume_comma_seq(TokenKind::LeftParen, TokenKind::RightParen, f)
    }

    /// Parses a curly-braced enclosed sequence of statements, where the closure `f`
    /// is invoked, until the given "close" token is found.
    ///
    /// This is useful for any sequence of statements within a block.
    /// Upon returning, both the `open` and `close` tokens will have been consumed.
    fn consume_curly_seq<T>(&mut self, f: impl FnMut(&mut Parser) -> Result<T>) -> Result<Vec<T>> {
        self.consume(TokenKind::LeftCurly)?;
        self.consume_seq_to_end(TokenKind::RightCurly, f)
    }

    /// Checks whether the current token is of the given type. If so, the token is consumed.
    fn check(&mut self, kind: TokenKind) -> bool {
        match self.consume_if(kind) {
            Ok(t) => t.is_some(),
            Err(_) => false,
        }
    }

    /// Checks whether the current token is an `External` token.
    fn check_external(&mut self) -> bool {
        self.check(TokenKind::External)
    }

    /// Reads the current documentation comment into the parser's state, if any is present.
    fn read_doc_comment(&mut self) -> Result<()> {
        if let Some(doc_comment) = self.consume_if(TokenKind::DocComment)? {
            self.doc_token = Some(doc_comment.value.unwrap_or_default());
        }

        Ok(())
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
    fn parse_identifier(&mut self) -> Result<Identifier> {
        let identifier = match self.consume_any() {
            // Actual identifiers are obviously allowed, so they pass through.
            Ok(ident) if ident.kind == TokenKind::Identifier => ident,

            // Keywords are also allowed, so reserved keywords can be used as identifiers.
            Ok(ident) if ident.kind.is_keyword() => ident,

            Ok(ident) => {
                return Err(ExpectedIdentifier {
                    source: self.source.clone(),
                    range: ident.index.clone(),
                    actual: ident.kind,
                }
                .into());
            }
            Err(err) => return Err(err),
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

    /// Parses the next token as an identifier. If the parsing fails, return `Err(err)`.
    fn parse_ident_or_err(&mut self, err: Error) -> Result<Identifier> {
        match self.parse_identifier() {
            Ok(name) => Ok(name),
            Err(_) => Err(err),
        }
    }

    /// Parses the next token(s) as a namespace path.
    ///
    /// Identifier paths are much like regular identifiers, but can be joined together
    /// with periods, to form longer chains of them. They can be as short as a single
    /// link, such as `std`, but they can also be longer, such as `std::fmt::error`.
    fn parse_namespace_path(&mut self) -> Result<NamespacePath> {
        let segments = self.consume_delim(IDENTIFIER_SEPARATOR, |p| p.parse_identifier())?;

        let start = segments.first().unwrap().location.0.start;
        let end = segments.last().unwrap().location.0.end;

        let path = NamespacePath {
            path: segments,
            location: (start..end).into(),
        };

        Ok(path)
    }

    /// Parses the next token as a symbol path.
    fn parse_path(&mut self) -> Result<Path> {
        let segments = self.consume_delim(IDENTIFIER_SEPARATOR, |p| p.parse_identifier())?;
        let (name, root) = segments.split_last().unwrap();

        let root = if root.is_empty() {
            NamespacePath::empty()
        } else {
            let root_loc_start = root.first().unwrap().location.start();
            let root_loc_end = root.last().unwrap().location.end();

            NamespacePath {
                path: root.to_vec(),
                location: (root_loc_start..root_loc_end).into(),
            }
        };

        Ok(Path {
            name: name.to_owned(),
            root,
            location: name.location.clone(),
        })
    }

    /// Returns a block for functions or methods.
    fn parse_block(&mut self) -> Result<Block> {
        let (statements, location) = self.consume_with_loc(|p| {
            let mut stmts = Vec::new();

            p.consume(TokenKind::LeftCurly)?;

            while p.consume_if(TokenKind::RightCurly)?.is_none() {
                match p.parse_statement() {
                    Ok(stmt) => stmts.push(stmt),
                    Err(err) => {
                        p.dcx.emit(err);

                        p.recover_statement()?;
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
    fn parse_external_block(&mut self) -> Result<Block> {
        if self.peek(TokenKind::LeftCurly)? {
            return Err(err!(self, ExternalFunctionBody));
        }

        if self.eof() {
            let last_token_end = self.last_token()?.end();

            return Ok(Block::from_location(last_token_end..last_token_end));
        }

        Ok(Block::from_location(self.token()?.index))
    }

    /// Returns a block, if the next token is a left curly bracket (`{`), as a `Some(block)`.
    ///
    /// Otherwise, returns `None`.
    fn parse_opt_block(&mut self) -> Result<Option<Block>> {
        if self.peek(TokenKind::LeftCurly)? {
            return Ok(Some(self.parse_block()?));
        }

        Ok(None)
    }

    /// Returns an empty block for external functions and an actual block for non-external functions.
    fn parse_opt_external_block(&mut self, external: bool) -> Result<Block> {
        if external {
            self.parse_external_block()
        } else {
            self.parse_block()
        }
    }
}
