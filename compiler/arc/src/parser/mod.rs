use std::{ops::Range, sync::Arc};

use self::errors::*;
use error_snippet::Result;
use lexer::{Lexer, Token, TokenKind};
use lume_errors::DiagCtxHandle;
use lume_span::SourceFile;

mod errors;
pub(crate) mod lexer;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    value: T,
    span: std::ops::Range<usize>,
}

impl<T> Spanned<T> {
    /// Creates a new spanned value.
    pub fn new(value: T, span: std::ops::Range<usize>) -> Spanned<T> {
        Self { value, span }
    }

    /// Gets a reference to the value held by the span.
    pub fn value(&self) -> &T {
        &self.value
    }

    /// Moves the span into the held value.
    pub fn into_value(self) -> T {
        self.value
    }

    /// Gets a reference to the value held by the span.
    pub fn span(&self) -> &std::ops::Range<usize> {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Location {
    pub source: Arc<SourceFile>,
    pub range: Range<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Block {
    pub ty: Spanned<String>,
    pub arguments: Vec<Value>,
    pub properties: Vec<Property>,
    pub location: Location,
}

impl Block {
    pub fn find_prop<'a>(&'a self, name: &str) -> Option<&'a Property> {
        self.properties.iter().find(|prop| prop.name.value == name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Property {
    pub name: Spanned<String>,
    pub value: Value,
    pub location: Location,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Value {
    String(String, Location),
    Integer(i64, Location),
    Block(Box<Block>),
}

impl Value {
    pub fn location(&self) -> &Location {
        match self {
            Self::String(_, loc) | Self::Integer(_, loc) => loc,
            Self::Block(block) => &block.location,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(_, _) => write!(f, "String"),
            Self::Integer(_, _) => write!(f, "Integer"),
            Self::Block(_) => write!(f, "Block"),
        }
    }
}

pub struct Parser {
    /// Defines the source code which is being parsed.
    source: Arc<SourceFile>,

    /// Handle to the diagnostics context which will handle parsing errors.
    dcx: DiagCtxHandle,

    /// Defines the lexer which tokenizes the module source code.
    lexer: Lexer,

    /// Defines the index of the current token being processed, given in a zero-based index.
    index: usize,

    /// Defines the current tokens being processed.
    tokens: Vec<Token>,
}

impl Parser {
    /// Creates a new [`Parser`] instance with the given source file as
    /// it's input to parse.
    pub fn new(source: Arc<SourceFile>, dcx: DiagCtxHandle) -> Self {
        let lexer = Lexer::new(source.clone());

        Parser {
            source,
            dcx,
            lexer,
            index: 0,
            tokens: vec![],
        }
    }

    /// Creates a new [`Parser`] for the given source code.
    ///
    /// This function doesn't perform any parsing itself - it simply readies
    /// a parser to interate over tokens from it's inner lexer.
    #[allow(dead_code)]
    pub fn new_with_str(str: &str) -> Parser {
        let source = SourceFile::internal(str);
        let dcx = DiagCtxHandle::shim();

        Parser::new(Arc::new(source), dcx)
    }

    /// Checks whether the parser has reached the end of the input.
    fn is_eof(&self) -> bool {
        self.index >= self.tokens.len()
    }

    /// Gets the [`Token`] at the given cursor index.
    fn token_at(&self, pos: usize) -> Token {
        if let Some(token) = self.tokens.get(pos) {
            token.clone()
        } else {
            Token {
                kind: TokenKind::Eof,
                index: self.position(),
            }
        }
    }

    /// Gets the [`Token`] at the current cursor index.
    fn token(&self) -> Token {
        self.token_at(self.index)
    }

    /// Gets the zero-based index of the current cursor position.
    fn position(&self) -> Range<usize> {
        self.token().index.clone()
    }

    /// Advances the cursor position by a single token.
    fn skip(&mut self) {
        self.index += 1;
    }

    /// Consumes the next [`Token`] from the [`Lexer`] and returns it, if it matches the expected kind.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the token does not match the expected kind.
    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        let current = self.token();

        if current.kind == kind {
            Ok(current)
        } else {
            Err(UnexpectedToken {
                source: self.source.clone(),
                range: current.index.clone(),
                expected: kind,
                actual: current.kind.clone(),
            }
            .into())
        }
    }

    /// Consumes the next [`Token`] from the [`Lexer`] and returns it if it matches the expected kind. Additionally,
    /// it advances the cursor position by a single token.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the token does not match the expected kind.
    fn consume(&mut self, kind: TokenKind) -> Result<Token> {
        let index = self.index;

        match self.expect(kind) {
            Ok(_) => {
                self.skip();

                Ok(self.token_at(index))
            }
            Err(err) => Err(err),
        }
    }

    /// Consumes the next [`Token`] from the [`Lexer`] and returns it. Additionally,
    /// it advances the cursor position by a single token.
    fn consume_any(&mut self) -> Token {
        let index = self.index;

        self.skip();
        self.token_at(index)
    }

    /// Parses the entire input source text until the [`Parser`] reaches
    /// end-of-file or until an unrecoverable parsing error occured.
    ///
    /// # Errors
    ///
    /// Returns `Err` if a token from the input source is unexpected or a value
    /// within the parsed document is unexpected or otherwise invalid, given the context.
    pub fn parse(&mut self) -> Result<Vec<Block>> {
        // Pre-tokenize the input source text.
        if self.tokens.is_empty() {
            self.tokens = self.lexer.all()?;
        }

        let mut blocks = Vec::new();

        while !self.is_eof() {
            let block = match self.parse_block() {
                Ok(block) => block,
                Err(err) => {
                    self.recover_block();
                    self.dcx.emit(err);

                    continue;
                }
            };

            blocks.push(block);
        }

        Ok(blocks)
    }

    /// Parses a single `Block` statement within an `Arcfile` into a [`Block`].
    fn parse_block(&mut self) -> Result<Block> {
        let start = self.position().start;

        let ty = self.parse_identifier()?;
        let arguments = self.parse_block_arguments()?;

        self.consume(TokenKind::CurlyLeft)?;

        let properties = self.parse_block_properties()?;

        let end = self.position().end;
        self.consume(TokenKind::CurlyRight)?;

        Ok(Block {
            ty,
            arguments,
            properties,
            location: Location {
                source: self.source.clone(),
                range: start..end,
            },
        })
    }

    /// Attempts to recover from a parsing error, whilst attempting to parse a block.
    ///
    /// The recovery for blocks is the move the cursor to the next matching brace. So, given the
    /// following Arcfile block:
    ///
    /// ```arc
    /// Package sample { }
    ///         ^ error occurs here...
    ///                  ^ ...so we move the cursor to here
    /// ```
    fn recover_block(&mut self) {
        let mut brace_depth = 0;

        loop {
            match self.token().kind {
                TokenKind::CurlyLeft => {
                    self.skip();
                    brace_depth += 1;
                }
                TokenKind::CurlyRight => {
                    self.skip();
                    brace_depth -= 1;

                    if brace_depth == 0 {
                        break;
                    }
                }
                _ => self.skip(),
            }
        }
    }

    /// Parses a zero-or-more block arguments within an `Arcfile`.
    fn parse_block_arguments(&mut self) -> Result<Vec<Value>> {
        let mut args = Vec::new();

        loop {
            if !self.token().kind.is_value() || self.token().kind == TokenKind::CurlyLeft {
                break;
            }

            args.push(self.parse_value()?);
        }

        Ok(args)
    }

    /// Parses a zero-or-more block properties within an `Arcfile`.
    fn parse_block_properties(&mut self) -> Result<Vec<Property>> {
        let mut props = Vec::new();

        loop {
            if self.token().kind == TokenKind::CurlyRight {
                break;
            }

            let start = self.position().start;

            let name = self.parse_identifier()?.clone();
            self.consume(TokenKind::Equal)?;

            let end = self.position().end;
            let value = self.parse_value()?;

            props.push(Property {
                name,
                value,
                location: Location {
                    source: self.source.clone(),
                    range: start..end,
                },
            });
        }

        Ok(props)
    }

    /// Parses an identifier ([`TokenKind::Identifier`]) at the current cursor position.
    fn parse_identifier(&mut self) -> Result<Spanned<String>> {
        if let TokenKind::Identifier(ident) = &self.consume_any().kind {
            return Ok(Spanned {
                span: self.token_at(self.index - 1).index,
                value: ident.clone(),
            });
        }

        let token = self.token();

        Err(ExpectedIdentifier {
            source: self.source.clone(),
            range: token.index.clone(),
            actual: token.kind.clone(),
        }
        .into())
    }

    /// Parses a value ([`Value`]) at the current cursor position.
    fn parse_value(&mut self) -> Result<Value> {
        let location = Location {
            source: self.source.clone(),
            range: self.position(),
        };

        Ok(match &self.consume_any().kind {
            TokenKind::String(str) => Value::String(str.clone(), location),
            TokenKind::Integer(int) => Value::Integer(*int, location),
            TokenKind::Identifier(_) => Value::Block(Box::new(self.parse_block()?)),
            kind => {
                return Err(ExpectedValue {
                    source: self.source.clone(),
                    range: self.token().index.clone(),
                    actual: kind.clone(),
                }
                .into());
            }
        })
    }
}
