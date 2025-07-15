use std::ops::Range;
use std::sync::Arc;

use crate::errors::*;
use error_snippet::Result;
use lume_ast::Identifier;
use lume_span::SourceFile;

mod errors;

pub const IDENTIFIER_SEPARATOR: TokenKind = TokenKind::PathSeparator;

const SYMBOLS: &[char] = &[
    '+', '-', '*', '/', '=', '!', '?', '<', '>', '&', '|', '^', '{', '}', '(', ')', '[', ']', ',', '.', ':', ';',
];

pub const OPERATOR_PRECEDENCE: &[(TokenKind, u8)] = &[
    (TokenKind::Assign, 1),
    (TokenKind::AddAssign, 1),
    (TokenKind::SubAssign, 1),
    (TokenKind::MulAssign, 1),
    (TokenKind::DivAssign, 1),
    (TokenKind::As, 2),
    (TokenKind::Equal, 3),
    (TokenKind::NotEqual, 3),
    (TokenKind::Greater, 4),
    (TokenKind::Less, 4),
    (TokenKind::BinaryAnd, 5),
    (TokenKind::BinaryXor, 6),
    (TokenKind::BinaryOr, 7),
    (TokenKind::And, 8),
    (TokenKind::Or, 9),
    (TokenKind::GreaterEqual, 10),
    (TokenKind::LessEqual, 10),
    (TokenKind::Add, 11),
    (TokenKind::Sub, 11),
    (TokenKind::Mul, 12),
    (TokenKind::Div, 12),
    (TokenKind::Increment, 13),
    (TokenKind::Decrement, 13),
    (TokenKind::Dot, 14),
    (TokenKind::DotDot, 14),
    (IDENTIFIER_SEPARATOR, 15),
];

/// Defines the precedence for unary operators, such as `-` or `!`.
///
/// They cannot be defined within [`OPERATOR_PRECEDENCE`], as unary operators
/// share the same operators as other operators, but have different meanings.
///
/// For example, `-` can mean both "minus some amount", but when it's within it's own
/// expression before a number expression, it'd mean "the negative of the following expression".
pub const UNARY_PRECEDENCE: u8 = 3;

/// Defines all the operators which are notated as postfix, as opposed to infix.
pub const POSTFIX_OPERATORS: &[TokenKind] = &[TokenKind::Increment, TokenKind::Decrement];

/// Defines all the operators which are used in binary contexts.
pub const BINARY_OPERATORS: &[TokenKind] = &[TokenKind::BinaryAnd, TokenKind::BinaryOr, TokenKind::BinaryXor];

/// Defines all the operators which are used in boolean contexts.
pub const BOOLEAN_OPERATORS: &[TokenKind] = &[TokenKind::And, TokenKind::Or];

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    As,
    Add,
    AddAssign,
    And,
    Arrow,
    Assign,
    DocComment,
    BinaryAnd,
    BinaryOr,
    BinaryXor,
    Break,
    Builtin,
    Colon,
    Comma,
    Comment,
    Continue,
    Decrement,
    Div,
    DivAssign,
    Dot,
    DotDot,
    DotDotDot,
    Else,
    Enum,
    Eof,
    Equal,
    Exclamation,
    External,
    False,
    Fn,
    Float,
    For,
    Greater,
    GreaterEqual,
    Identifier,
    If,
    Impl,
    Import,
    In,
    Increment,
    Integer(u32),
    LeftBracket,
    LeftCurly,
    LeftParen,
    Less,
    LessEqual,
    Let,
    Loop,
    Mul,
    MulAssign,
    Namespace,
    NotEqual,
    PathSeparator,
    Priv,
    Pub,
    Question,
    Or,
    Return,
    RightBracket,
    RightCurly,
    RightParen,
    Semicolon,
    SelfRef,
    String,
    Struct,
    Sub,
    SubAssign,
    Trait,
    True,
    Use,
    While,
}

impl TokenKind {
    #[inline]
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            TokenKind::As
                | TokenKind::Break
                | TokenKind::Builtin
                | TokenKind::Continue
                | TokenKind::Else
                | TokenKind::External
                | TokenKind::False
                | TokenKind::Fn
                | TokenKind::For
                | TokenKind::If
                | TokenKind::Impl
                | TokenKind::Import
                | TokenKind::In
                | TokenKind::Loop
                | TokenKind::Namespace
                | TokenKind::Priv
                | TokenKind::Pub
                | TokenKind::Return
                | TokenKind::SelfRef
                | TokenKind::Struct
                | TokenKind::Trait
                | TokenKind::True
                | TokenKind::Use
                | TokenKind::While
        )
    }

    #[inline]
    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            TokenKind::Integer(_) | TokenKind::Float | TokenKind::String | TokenKind::False | TokenKind::True
        )
    }

    #[inline]
    pub fn is_unary(&self) -> bool {
        matches!(self, TokenKind::Exclamation | TokenKind::Sub)
    }

    #[inline]
    pub fn is_comment(&self) -> bool {
        matches!(self, TokenKind::Comment | TokenKind::DocComment)
    }

    #[inline]
    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::Add
                | TokenKind::AddAssign
                | TokenKind::And
                | TokenKind::BinaryAnd
                | TokenKind::BinaryOr
                | TokenKind::BinaryXor
                | TokenKind::Decrement
                | TokenKind::Div
                | TokenKind::DivAssign
                | TokenKind::Equal
                | TokenKind::Greater
                | TokenKind::GreaterEqual
                | TokenKind::Increment
                | TokenKind::Less
                | TokenKind::LessEqual
                | TokenKind::Mul
                | TokenKind::MulAssign
                | TokenKind::NotEqual
                | TokenKind::Or
                | TokenKind::Sub
                | TokenKind::SubAssign
        )
    }

    #[inline]
    pub fn has_value(&self) -> bool {
        self.is_keyword() || self.is_operator() || self.is_comment()
    }
}

impl From<TokenKind> for &'static str {
    #[inline]
    fn from(val: TokenKind) -> &'static str {
        match val {
            TokenKind::As => "as",
            TokenKind::Add => "+",
            TokenKind::AddAssign => "+=",
            TokenKind::And => "&&",
            TokenKind::Arrow => "->",
            TokenKind::Assign => "=",
            TokenKind::DocComment => "doc comment",
            TokenKind::BinaryAnd => "&",
            TokenKind::BinaryOr => "|",
            TokenKind::BinaryXor => "^",
            TokenKind::Break => "break",
            TokenKind::Builtin => "builtin",
            TokenKind::Colon => ":",
            TokenKind::Comma => ",",
            TokenKind::Comment => "comment",
            TokenKind::Continue => "continue",
            TokenKind::Decrement => "--",
            TokenKind::Div => "/",
            TokenKind::DivAssign => "/=",
            TokenKind::Dot => ".",
            TokenKind::DotDot => "..",
            TokenKind::DotDotDot => "...",
            TokenKind::Else => "else",
            TokenKind::Enum => "enum",
            TokenKind::Eof => "EOF",
            TokenKind::Equal => "==",
            TokenKind::Exclamation => "!",
            TokenKind::External => "external",
            TokenKind::False => "false",
            TokenKind::Fn => "fn",
            TokenKind::Float => "float",
            TokenKind::For => "for",
            TokenKind::Greater => ">",
            TokenKind::GreaterEqual => ">=",
            TokenKind::If => "if",
            TokenKind::Identifier => "identifier",
            TokenKind::Impl => "impl",
            TokenKind::Import => "import",
            TokenKind::In => "in",
            TokenKind::Increment => "++",
            TokenKind::LeftBracket => "[",
            TokenKind::LeftCurly => "{",
            TokenKind::LeftParen => "(",
            TokenKind::Less => "<",
            TokenKind::LessEqual => "<=",
            TokenKind::Let => "let",
            TokenKind::Loop => "loop",
            TokenKind::Mul => "*",
            TokenKind::MulAssign => "*=",
            TokenKind::Namespace => "namespace",
            TokenKind::NotEqual => "!=",
            TokenKind::Integer(_) => "integer",
            TokenKind::PathSeparator => "::",
            TokenKind::Priv => "priv",
            TokenKind::Pub => "pub",
            TokenKind::Question => "?",
            TokenKind::Or => "||",
            TokenKind::Return => "return",
            TokenKind::RightBracket => "]",
            TokenKind::RightCurly => "}",
            TokenKind::RightParen => ")",
            TokenKind::SelfRef => "self",
            TokenKind::Semicolon => ";",
            TokenKind::String => "string",
            TokenKind::Struct => "struct",
            TokenKind::Sub => "-",
            TokenKind::SubAssign => "-=",
            TokenKind::Trait => "trait",
            TokenKind::True => "true",
            TokenKind::Use => "use",
            TokenKind::While => "while",
        }
    }
}

impl From<TokenKind> for String {
    fn from(val: TokenKind) -> String {
        <TokenKind as Into<&'static str>>::into(val).to_string()
    }
}

impl std::fmt::Debug for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let desc: &'static str = (*self).into();

        write!(f, "{desc}")
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let desc: &'static str = (*self).into();

        write!(f, "{desc}")
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    /// Defines the kind of the token.
    pub kind: TokenKind,

    /// Defines the start and end position of the token.
    pub index: Range<usize>,

    /// Defines the value of the token.
    pub value: Option<String>,

    /// Defines the "kind" of the token. This is only used for integer- and floating-point literals.
    pub ty: Option<String>,
}

impl Token {
    #[inline]
    pub fn new(kind: TokenKind, value: String) -> Self {
        Token {
            kind,
            index: 0..0,
            value: Some(value),
            ty: None,
        }
    }

    #[inline]
    pub fn empty(kind: TokenKind) -> Self {
        Token {
            kind,
            index: 0..0,
            value: None,
            ty: None,
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.index.end - self.index.start
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn start(&self) -> usize {
        self.index.start
    }

    #[inline]
    pub fn end(&self) -> usize {
        self.index.end
    }

    /// Gets the precedence of the token kind.
    ///
    /// Returns the precedence of the token kind, or 0 if the token kind is not an operator.
    #[inline]
    pub fn precedence(&self) -> u8 {
        OPERATOR_PRECEDENCE
            .iter()
            .find(|(k, _)| k == &self.kind)
            .map_or(0, |(_, p)| *p)
    }

    /// Determines whether the token is a postfix operator.
    #[inline]
    pub fn is_postfix(&self) -> bool {
        POSTFIX_OPERATORS.iter().any(|k| k == &self.kind)
    }

    /// Determines whether the token is a binary operator.
    #[inline]
    pub fn is_binary(&self) -> bool {
        BINARY_OPERATORS.iter().any(|k| k == &self.kind)
    }

    /// Determines whether the token is a boolean operator.
    #[inline]
    pub fn is_boolean(&self) -> bool {
        BOOLEAN_OPERATORS.iter().any(|k| k == &self.kind)
    }
}

impl From<Token> for &'static str {
    fn from(val: Token) -> &'static str {
        val.kind.into()
    }
}

impl From<Token> for String {
    fn from(val: Token) -> Self {
        val.kind.into()
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)?;

        if !self.kind.has_value()
            && let Some(value) = &self.value
        {
            write!(f, " ({value})")?;
        }

        Ok(())
    }
}

impl From<Token> for Identifier {
    fn from(value: Token) -> Identifier {
        Identifier {
            name: value.value.unwrap(),
            location: value.index.into(),
        }
    }
}

#[derive(Debug)]
pub struct Lexer {
    /// Declares the source to lex tokens from.
    pub source: Arc<SourceFile>,

    /// Defines the current position in the source.
    position: usize,

    /// Defines the total length of the source code.
    length: usize,
}

impl Lexer {
    /// Creates a new lexer instance.
    pub fn new(source: Arc<SourceFile>) -> Self {
        Lexer {
            length: source.content.len(),
            source,
            position: 0,
        }
    }

    #[inline]
    pub fn is_eof(&self) -> bool {
        self.position >= self.source.content.len()
    }

    /// Tries to get the character at the current cursor position.
    ///
    /// Returns `None` if the cursor is at the end of the source.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn current_char(&self) -> Option<char> {
        self.source.content.as_bytes().get(self.position).map(|c| *c as char)
    }

    /// Tries to get the character at the current cursor position.
    ///
    /// Returns `\0` if the cursor is at the end of the source.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn current_char_or_eof(&self) -> char {
        self.current_char().unwrap_or('\0')
    }

    /// Tries to get the character which is at the current cursor position, offset by `offset`.
    ///
    /// Returns `\0` if the cursor is at the end of the source.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn at_offset(&self, offset: usize) -> char {
        self.source
            .content
            .as_bytes()
            .get(self.position + offset)
            .map_or('\0', |c| *c as char)
    }

    /// Advances the cursor position of the lexer to the next line.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn next(&mut self) {
        self.position += 1;
    }

    /// Gets the character at the current cursor position and advances the cursor position to the next position.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn consume(&mut self) -> char {
        let c = self.current_char_or_eof();

        self.next();
        c
    }

    /// Consumes characters while the predicate returns `true`.
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn eat_while(&mut self, predicate: impl Fn(char) -> bool) {
        loop {
            match self.current_char() {
                Some(c) if predicate(c) => {
                    tracing::trace!("char matched: {c:?}");
                    self.next();
                }
                Some(c) => {
                    tracing::trace!("char not matched: {c:?}");
                    break;
                }
                _ => {
                    tracing::trace!("reached EOF");
                    break;
                }
            }
        }
    }

    /// Gets characters while the given character is found.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn take_until(&mut self, c: char) -> &str {
        let start = self.position;

        let sliced = &self.source.content[start..];
        let found_idx = match sliced.find(c).map(|i| i + start) {
            Some(idx) => idx,
            None => self.source.content.len(),
        };

        self.position = found_idx;
        &self.source.content[start..found_idx]
    }

    /// Gets characters while the predicate returns `true`.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn take_while(&mut self, predicate: impl Fn(char) -> bool) -> String {
        let start = self.position;

        self.eat_while(predicate);

        self.source.content[start..self.position].to_string()
    }

    /// Skips characters while the predicate returns `true`.
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn skip_while(&mut self, predicate: impl Fn(char) -> bool) {
        loop {
            let c = self.current_char();
            if c.is_none() || !predicate(c.unwrap()) {
                tracing::trace!("char not matched: {c:?}");
                break;
            }

            tracing::trace!("char matched: {c:?}");
            self.next();
        }
    }

    /// Gets the token at the current cursor position. The cursor is advanced to the start of the next token.
    ///
    /// # Errors
    ///
    /// This method will return `Err` if the expected token was formatted incorrectly,
    /// or if the lexer unexpectedly encountered end-of-file.
    #[tracing::instrument(
        level = "INFO",
        name = "lume_lexer::lexer::next_token",
        parent = None,
        fields(start_idx, end_idx, token),
        skip(self),
        err
    )]
    pub fn next_token(&mut self) -> Result<Token> {
        let start_idx = self.position;
        tracing::Span::current().record("start_idx", start_idx);

        let first_char = match self.current_char_or_eof() {
            '\0' => return Ok(Token::empty(TokenKind::Eof)),
            c => c,
        };

        tracing::trace!("first character: {first_char:?}");

        let mut token = match first_char {
            // Identifiers, such as keywords and standalone words.
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),

            // Symbols, such as operators and punctuation.
            c if SYMBOLS.contains(&c) => self.symbol()?,

            // Numbers
            '0'..='9' => self.number(),

            // String literals
            '"' => self.string()?,

            // Whitespace
            ' ' | '\t' | '\n' | '\r' => {
                self.eat_while(char::is_whitespace);

                return self.next_token();
            }
            _ => {
                return Err(UnexpectedCharacter {
                    source: self.source.clone(),
                    range: self.position..self.position + 1,
                    char: first_char,
                }
                .into());
            }
        };

        let end_idx = self.position;
        tracing::Span::current().record("end_idx", end_idx);

        token.index = start_idx..end_idx;

        tracing::Span::current().record("token", format_args!("{}", token.kind));

        Ok(token)
    }

    /// Parses a comment token at the current cursor position.
    #[tracing::instrument(level = "DEBUG", skip(self), ret)]
    fn comment(&mut self) -> Token {
        let kind = self.eat_comment_prefix();
        let content = self.take_until('\n').trim().to_string();

        Token::new(kind, content)
    }

    #[tracing::instrument(level = "TRACE", skip(self), ret)]
    fn eat_comment_prefix(&mut self) -> TokenKind {
        // Skip over all the whitespace characters, before attempting to eat the comment prefix.
        self.eat_while(char::is_whitespace);

        // Eat the comment prefix characters.
        match self.take_while(|c| c == '/').len() {
            3 => TokenKind::DocComment,
            _ => TokenKind::Comment,
        }
    }

    /// Parses a block of comment tokens at the current cursor position.
    #[tracing::instrument(level = "DEBUG", skip(self), ret)]
    fn comment_block(&mut self) -> Token {
        let mut kind = TokenKind::Comment;
        let mut comments = Vec::new();

        loop {
            let position = self.position;
            let token = self.comment();

            // If this is the first iteration, update the kind of token.
            if comments.is_empty() {
                kind = token.kind;
            }

            // Otherwise, if the comment type doesn't match, stop iterating.
            if !comments.is_empty() && token.kind != kind {
                self.position = position;
                break;
            }

            comments.push(token.value.unwrap());

            // Consume the newline character.
            self.next();

            if matches!(self.current_char(), Some('\n') | None) {
                break;
            }
        }

        self.position -= 1;

        Token::new(kind, comments.join("\n"))
    }

    /// Parses an identifier token at the current cursor position.
    #[tracing::instrument(level = "DEBUG", skip(self), ret)]
    fn identifier(&mut self) -> Token {
        let content = self.take_while(|c| c.is_ascii_alphanumeric() || c == '_');

        match content.as_str() {
            "as" => Token::empty(TokenKind::As),
            "break" => Token::empty(TokenKind::Break),
            "builtin" => Token::empty(TokenKind::Builtin),
            "continue" => Token::empty(TokenKind::Continue),
            "else" => Token::empty(TokenKind::Else),
            "enum" => Token::empty(TokenKind::Enum),
            "external" => Token::empty(TokenKind::External),
            "false" => Token::empty(TokenKind::False),
            "fn" => Token::empty(TokenKind::Fn),
            "for" => Token::empty(TokenKind::For),
            "if" => Token::empty(TokenKind::If),
            "impl" => Token::empty(TokenKind::Impl),
            "import" => Token::empty(TokenKind::Import),
            "in" => Token::empty(TokenKind::In),
            "let" => Token::empty(TokenKind::Let),
            "loop" => Token::empty(TokenKind::Loop),
            "namespace" => Token::empty(TokenKind::Namespace),
            "priv" => Token::empty(TokenKind::Priv),
            "pub" => Token::empty(TokenKind::Pub),
            "return" => Token::empty(TokenKind::Return),
            "self" => Token::empty(TokenKind::SelfRef),
            "struct" => Token::empty(TokenKind::Struct),
            "trait" => Token::empty(TokenKind::Trait),
            "true" => Token::empty(TokenKind::True),
            "use" => Token::empty(TokenKind::Use),
            "while" => Token::empty(TokenKind::While),
            _ => Token::new(TokenKind::Identifier, content),
        }
    }

    /// Parses a symbol token at the current cursor position.
    #[tracing::instrument(level = "DEBUG", skip(self), err, ret)]
    fn symbol(&mut self) -> Result<Token> {
        let max_len = (self.position + 3).min(self.length);
        let slice = &self.source.content[self.position..max_len];

        let chars: Vec<char> = slice.chars().collect();
        let (kind, len) = match self.symbol_value(&chars) {
            Ok((kind, len)) => (kind, len),
            Err(err) => return Err(err),
        };

        if kind == TokenKind::Comment {
            return Ok(self.comment_block());
        }

        let symbol: String = chars[..len].iter().collect();

        for _ in 0..len {
            self.next();
        }

        Ok(Token::new(kind, symbol))
    }

    #[tracing::instrument(level = "TRACE", skip(self), err, ret)]
    fn symbol_value(&mut self, chars: &[char]) -> Result<(TokenKind, usize)> {
        if chars.len() >= 3
            && let ('.', '.', '.') = (chars[0], chars[1], chars[2])
        {
            return Ok((TokenKind::DotDotDot, 3));
        }

        if chars.len() >= 2 {
            match (chars[0], chars[1]) {
                ('+', '=') => return Ok((TokenKind::AddAssign, 2)),
                ('&', '&') => return Ok((TokenKind::And, 2)),
                ('-', '>') => return Ok((TokenKind::Arrow, 2)),
                ('/', '/') => return Ok((TokenKind::Comment, 2)),
                ('=', '=') => return Ok((TokenKind::Equal, 2)),
                ('-', '-') => return Ok((TokenKind::Decrement, 2)),
                ('.', '.') => return Ok((TokenKind::DotDot, 2)),
                ('/', '=') => return Ok((TokenKind::DivAssign, 2)),
                ('>', '=') => return Ok((TokenKind::GreaterEqual, 2)),
                ('+', '+') => return Ok((TokenKind::Increment, 2)),
                ('<', '=') => return Ok((TokenKind::LessEqual, 2)),
                ('*', '=') => return Ok((TokenKind::MulAssign, 2)),
                ('!', '=') => return Ok((TokenKind::NotEqual, 2)),
                ('|', '|') => return Ok((TokenKind::Or, 2)),
                (':', ':') => return Ok((TokenKind::PathSeparator, 2)),
                ('-', '=') => return Ok((TokenKind::SubAssign, 2)),
                _ => {}
            }
        }

        if !chars.is_empty() {
            match chars[0] {
                '+' => return Ok((TokenKind::Add, 1)),
                '=' => return Ok((TokenKind::Assign, 1)),
                '&' => return Ok((TokenKind::BinaryAnd, 1)),
                '|' => return Ok((TokenKind::BinaryOr, 1)),
                '^' => return Ok((TokenKind::BinaryXor, 1)),
                ':' => return Ok((TokenKind::Colon, 1)),
                ',' => return Ok((TokenKind::Comma, 1)),
                '/' => return Ok((TokenKind::Div, 1)),
                '.' => return Ok((TokenKind::Dot, 1)),
                '!' => return Ok((TokenKind::Exclamation, 1)),
                '>' => return Ok((TokenKind::Greater, 1)),
                '[' => return Ok((TokenKind::LeftBracket, 1)),
                '{' => return Ok((TokenKind::LeftCurly, 1)),
                '(' => return Ok((TokenKind::LeftParen, 1)),
                '<' => return Ok((TokenKind::Less, 1)),
                '*' => return Ok((TokenKind::Mul, 1)),
                '?' => return Ok((TokenKind::Question, 1)),
                ']' => return Ok((TokenKind::RightBracket, 1)),
                '}' => return Ok((TokenKind::RightCurly, 1)),
                ')' => return Ok((TokenKind::RightParen, 1)),
                ';' => return Ok((TokenKind::Semicolon, 1)),
                '-' => return Ok((TokenKind::Sub, 1)),
                _ => {}
            }
        }

        Err(UnexpectedCharacter {
            source: self.source.clone(),
            range: self.position..self.position + 1,
            char: chars[0],
        }
        .into())
    }

    /// Parses a number token at the current cursor position.
    #[tracing::instrument(level = "DEBUG", skip(self), ret)]
    fn number(&mut self) -> Token {
        let start_index = self.position;

        // Read the radix prefix, if any.
        let radix = self.parse_radix_prefix();

        let mut token_kind = TokenKind::Integer(radix);

        // Attempt to parse any following groups, such as decimal point or float exponent.
        match self.current_char_or_eof() {
            '.' if self.at_offset(1).is_ascii_digit() => {
                self.next();

                // Check for digits following the decimal point
                if self.current_char_or_eof().is_ascii_digit() {
                    self.consume_digits(radix);

                    if matches!(self.current_char_or_eof(), 'e' | 'E') {
                        self.next();
                        self.consume_float_exponent();
                    }
                }

                token_kind = TokenKind::Float;
            }
            'e' | 'E' => {
                self.next();
                self.consume_float_exponent();

                token_kind = TokenKind::Float;
            }
            _ => {}
        }

        let end_index = self.position;
        let number_str = self.source.content[start_index..end_index].to_string();

        let kind = match self.current_char_or_eof() {
            '_' => {
                self.next();

                let kind = self.take_while(|c| c.is_ascii_alphanumeric());

                Some(kind)
            }
            _ => None,
        };

        let mut token = Token::new(token_kind, number_str);
        token.ty = kind;
        token
    }

    /// Parses a radix prefix from the input stream.
    ///
    /// Radix prefixes are used to specify the base of a number literal, such as:
    ///   - Binary: `0b1010`
    ///   - Octal: `0o755`
    ///   - Hexadecimal: `0x1A`
    ///
    /// This method consumes characters from the token stream until it encounters a non-digit character.
    /// If no radix prefix is found, the method reads all base-10 digits.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn parse_radix_prefix(&mut self) -> u32 {
        // Default to base-10.
        let mut radix = 10;

        if self.current_char_or_eof() == '0' {
            self.next();

            match self.current_char_or_eof() {
                // Binary
                'b' | 'B' => {
                    radix = 2;
                    self.next();
                    self.consume_digits(radix);
                }

                // Octal
                'o' | 'O' => {
                    radix = 8;
                    self.next();
                    self.consume_digits(radix);
                }

                // Hexadecimal
                'x' | 'X' => {
                    radix = 16;
                    self.next();
                    self.consume_digits(radix);
                }

                // Decimal
                '0'..='9' => self.consume_digits(radix),

                // Otherwise, it's just a zero.
                _ => {}
            }
        } else {
            self.consume_digits(radix);
        }

        radix
    }

    #[tracing::instrument(level = "TRACE", skip(self))]
    fn consume_digits(&mut self, radix: u32) {
        let mut last_numeric = self.position;

        while let Some(c) = self.current_char() {
            if !c.is_digit(radix) && c != '_' {
                break;
            }

            self.next();

            if c.is_digit(radix) {
                last_numeric = self.position;
            }
        }

        self.position = last_numeric;
    }

    #[tracing::instrument(level = "TRACE", skip(self))]
    fn consume_float_exponent(&mut self) {
        if matches!(self.current_char_or_eof(), '-' | '+') {
            self.next();
        }

        self.skip_while(|c| c.is_ascii_digit());
    }

    /// Parses a string token at the current cursor position.
    #[tracing::instrument(level = "DEBUG", skip(self), err, ret)]
    fn string(&mut self) -> Result<Token> {
        let mut content = String::new();

        // Consume the opening quote
        self.consume();

        loop {
            let c = self.consume();
            match c {
                '"' => break,
                '\0' => {
                    return Err(MissingEndingQuote {
                        source: self.source.clone(),
                        range: self.position..self.position + 1,
                    }
                    .into());
                }
                _ => {}
            }

            content.push(c);
        }

        Ok(Token::new(TokenKind::String, content))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lexer(source: &str) -> Lexer {
        let source = SourceFile::internal(source);

        Lexer::new(Arc::new(source))
    }

    fn lex_all(source: &str) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        let mut lexer = lexer(source);

        loop {
            let token = lexer.next_token()?;
            if token.kind == TokenKind::Eof {
                break;
            }

            tokens.push(token);
        }

        Ok(tokens)
    }

    fn token(kind: TokenKind, value: Option<String>, position: usize, end: usize) -> Token {
        Token {
            kind,
            value,
            index: position..end,
            ty: None,
        }
    }

    fn token_ty(kind: TokenKind, value: Option<String>, position: usize, end: usize, ty: &'static str) -> Token {
        Token {
            kind,
            value,
            index: position..end,
            ty: Some(ty.to_string()),
        }
    }

    macro_rules! assert_token {
        (
            $input: expr,
            $kind: expr,
            $value: expr,
            $position: expr,
            $end: expr
        ) => {
            let mut lexer = lexer($input);
            let token = lexer.next_token().unwrap();

            assert_eq!(token.kind, $kind);
            assert_eq!(token.index, ($position as usize)..($end as usize));

            if token.kind.has_value() && $value.is_some() {
                let val: String = $value.unwrap().to_string();
                assert_eq!(token.value, Some(val.to_string()));
            }
        };
    }

    macro_rules! assert_tokens {
        (
            $input: expr,
            $(
                $token: expr
            ),+
        ) => {
            let expected = vec![$($token),+];

            let mut lexer = lexer($input);

            for exp in expected {
                let token = lexer.next_token().unwrap();

                assert_eq!(token.kind, exp.kind);

                if token.kind.has_value() && exp.value.is_some() {
                    let val: String = exp.value.unwrap().to_string();
                    assert_eq!(token.value, Some(val.to_string()));
                }

                assert_eq!(token.index, exp.index);
            }
        };
    }

    #[test]
    fn test_keywords_map() {
        assert_token!("as", TokenKind::As, None::<String>, 0, 2);
        assert_token!("break", TokenKind::Break, None::<String>, 0, 5);
        assert_token!("builtin", TokenKind::Builtin, None::<String>, 0, 7);
        assert_token!("continue", TokenKind::Continue, None::<String>, 0, 8);
        assert_token!("external", TokenKind::External, None::<String>, 0, 8);
        assert_token!("for", TokenKind::For, None::<String>, 0, 3);
        assert_token!("fn", TokenKind::Fn, None::<String>, 0, 2);
        assert_token!("if", TokenKind::If, None::<String>, 0, 2);
        assert_token!("import", TokenKind::Import, None::<String>, 0, 6);
        assert_token!("impl", TokenKind::Impl, None::<String>, 0, 4);
        assert_token!("in", TokenKind::In, None::<String>, 0, 2);
        assert_token!("loop", TokenKind::Loop, None::<String>, 0, 4);
        assert_token!("namespace", TokenKind::Namespace, None::<String>, 0, 9);
        assert_token!("self", TokenKind::SelfRef, None::<String>, 0, 4);
        assert_token!("struct", TokenKind::Struct, None::<String>, 0, 6);
        assert_token!("return", TokenKind::Return, None::<String>, 0, 6);
        assert_token!("true", TokenKind::True, None::<String>, 0, 4);
        assert_token!("false", TokenKind::False, None::<String>, 0, 5);
        assert_token!("priv", TokenKind::Priv, None::<String>, 0, 4);
        assert_token!("pub", TokenKind::Pub, None::<String>, 0, 3);
        assert_token!("use", TokenKind::Use, None::<String>, 0, 3);
        assert_token!("while", TokenKind::While, None::<String>, 0, 5);
    }

    #[test]
    fn test_symbols_map() {
        assert_token!("...", TokenKind::DotDotDot, Some("..."), 0, 3);

        assert_token!("+=", TokenKind::AddAssign, Some("+="), 0, 2);
        assert_token!("&&", TokenKind::And, Some("&&"), 0, 2);
        assert_token!("->", TokenKind::Arrow, Some("->"), 0, 2);
        assert_token!("==", TokenKind::Equal, Some("=="), 0, 2);
        assert_token!("--", TokenKind::Decrement, Some("--"), 0, 2);
        assert_token!("..", TokenKind::DotDot, Some(".."), 0, 2);
        assert_token!("/=", TokenKind::DivAssign, Some("/="), 0, 2);
        assert_token!(">=", TokenKind::GreaterEqual, Some(">="), 0, 2);
        assert_token!("++", TokenKind::Increment, Some("++"), 0, 2);
        assert_token!("<=", TokenKind::LessEqual, Some("<="), 0, 2);
        assert_token!("*=", TokenKind::MulAssign, Some("*="), 0, 2);
        assert_token!("!=", TokenKind::NotEqual, Some("!="), 0, 2);
        assert_token!("||", TokenKind::Or, Some("||"), 0, 2);
        assert_token!("::", TokenKind::PathSeparator, Some("::"), 0, 2);
        assert_token!("-=", TokenKind::SubAssign, Some("-="), 0, 2);

        assert_token!("&", TokenKind::BinaryAnd, Some("&"), 0, 1);
        assert_token!("|", TokenKind::BinaryOr, Some("|"), 0, 1);
        assert_token!("^", TokenKind::BinaryXor, Some("^"), 0, 1);
        assert_token!("=", TokenKind::Assign, Some("="), 0, 1);
        assert_token!(":", TokenKind::Colon, Some(":"), 0, 1);
        assert_token!(",", TokenKind::Comma, Some(","), 0, 1);
        assert_token!("/", TokenKind::Div, Some("/"), 0, 1);
        assert_token!(".", TokenKind::Dot, Some("."), 0, 1);
        assert_token!("!", TokenKind::Exclamation, Some("!"), 0, 1);
        assert_token!(">", TokenKind::Greater, Some(">"), 0, 1);
        assert_token!("[", TokenKind::LeftBracket, Some("["), 0, 1);
        assert_token!("{", TokenKind::LeftCurly, Some("{"), 0, 1);
        assert_token!("(", TokenKind::LeftParen, Some("("), 0, 1);
        assert_token!("<", TokenKind::Less, Some("<"), 0, 1);
        assert_token!("*", TokenKind::Mul, Some("*"), 0, 1);
        assert_token!("]", TokenKind::RightBracket, Some("]"), 0, 1);
        assert_token!("}", TokenKind::RightCurly, Some("}"), 0, 1);
        assert_token!(")", TokenKind::RightParen, Some(")"), 0, 1);
        assert_token!(";", TokenKind::Semicolon, Some(";"), 0, 1);
        assert_token!("-", TokenKind::Sub, Some("-"), 0, 1);
    }

    #[test]
    fn test_parenthesis() {
        assert_tokens!(
            "()",
            token(TokenKind::LeftParen, None, 0, 1),
            token(TokenKind::RightParen, None, 1, 2)
        );
    }

    #[test]
    fn test_brackets() {
        assert_tokens!(
            "[]",
            token(TokenKind::LeftBracket, None, 0, 1),
            token(TokenKind::RightBracket, None, 1, 2)
        );
    }

    #[test]
    fn test_curly_braces() {
        assert_tokens!(
            "{}",
            token(TokenKind::LeftCurly, None, 0, 1),
            token(TokenKind::RightCurly, None, 1, 2)
        );
    }

    #[test]
    fn test_comment() {
        assert_token!("//", TokenKind::Comment, Some(""), 0, 2);
        assert_token!("///", TokenKind::DocComment, Some(""), 0, 3);
        assert_token!("// testing", TokenKind::Comment, Some("testing"), 0, 10);
        assert_token!("/// testing", TokenKind::DocComment, Some("testing"), 0, 11);

        assert_tokens!(
            "// comment 1
            // comment 2",
            token(TokenKind::Comment, Some("comment 1\ncomment 2".into()), 0, 37)
        );

        assert_tokens!(
            "// comment 1
            //
            // comment 2",
            token(TokenKind::Comment, Some("comment 1\n\ncomment 2".into()), 0, 52)
        );

        assert_tokens!(
            "/// comment 1
            /// comment 2",
            token(TokenKind::DocComment, Some("comment 1\ncomment 2".into()), 0, 39)
        );

        assert_tokens!(
            "/// comment 1
            ///
            /// comment 2",
            token(TokenKind::DocComment, Some("comment 1\n\ncomment 2".into()), 0, 55)
        );

        assert_tokens!(
            "/// comment 1
            // comment 2",
            token(TokenKind::DocComment, Some("comment 1".into()), 0, 13),
            token(TokenKind::Comment, Some("comment 2".into()), 26, 38)
        );

        assert_tokens!(
            "// comment 1  ",
            token(TokenKind::Comment, Some("comment 1".into()), 0, 14)
        );

        assert_tokens!(
            "/// comment 1  ",
            token(TokenKind::DocComment, Some("comment 1".into()), 0, 15)
        );
    }

    #[test]
    fn test_identifier() {
        assert_token!("foo", TokenKind::Identifier, Some("foo"), 0, 3);
        assert_token!("FOO", TokenKind::Identifier, Some("FOO"), 0, 3);
        assert_token!("_LUME", TokenKind::Identifier, Some("_LUME"), 0, 5);
        assert_token!("__LUME__", TokenKind::Identifier, Some("__LUME__"), 0, 8);
        assert_token!("_", TokenKind::Identifier, Some("_"), 0, 1);
        assert_token!("_1", TokenKind::Identifier, Some("_1"), 0, 2);
        assert_token!("_test1", TokenKind::Identifier, Some("_test1"), 0, 6);
    }

    #[test]
    fn test_number() {
        assert_token!("1", TokenKind::Integer(10), Some("1"), 0, 1);
        assert_token!("10", TokenKind::Integer(10), Some("10"), 0, 2);
        assert_token!("0b01", TokenKind::Integer(2), Some("0b01"), 0, 4);
        assert_token!("0B01", TokenKind::Integer(2), Some("0B01"), 0, 4);
        assert_token!("0x01", TokenKind::Integer(16), Some("0x01"), 0, 4);
        assert_token!("0X01", TokenKind::Integer(16), Some("0X01"), 0, 4);
        assert_token!("0o01", TokenKind::Integer(8), Some("0o01"), 0, 4);
        assert_token!("0O01", TokenKind::Integer(8), Some("0O01"), 0, 4);
        assert_token!("0", TokenKind::Integer(10), Some("0"), 0, 1);
        assert_token!("0000", TokenKind::Integer(10), Some("0000"), 0, 4);
        assert_token!("10.0", TokenKind::Float, Some("10.0"), 0, 4);
        assert_token!("10.123", TokenKind::Float, Some("10.123"), 0, 6);
        assert_token!("0_0", TokenKind::Integer(10), Some("0_0"), 0, 3);
        assert_token!("00_00", TokenKind::Integer(10), Some("00_00"), 0, 5);
        assert_token!("1_000_000", TokenKind::Integer(10), Some("1_000_000"), 0, 9);

        assert_tokens!(
            "-1",
            token(TokenKind::Sub, None, 0, 1),
            token(TokenKind::Integer(10), Some("1".into()), 1, 2)
        );

        assert_tokens!("10e5", token(TokenKind::Float, Some("10e5".into()), 0, 4));
        assert_tokens!("10E5", token(TokenKind::Float, Some("10E5".into()), 0, 4));
        assert_tokens!("1.0e5", token(TokenKind::Float, Some("1.0e5".into()), 0, 5));
        assert_tokens!("1.0E5", token(TokenKind::Float, Some("1.0E5".into()), 0, 5));

        assert_tokens!("1_u32", token_ty(TokenKind::Integer(10), Some("1".into()), 0, 5, "u32"));
        assert_tokens!("1_f32", token_ty(TokenKind::Integer(10), Some("1".into()), 0, 5, "f32"));
        assert_tokens!("1.1_f64", token_ty(TokenKind::Float, Some("1.1".into()), 0, 7, "f64"));

        assert_tokens!(
            "0x0_u64",
            token_ty(TokenKind::Integer(16), Some("0x0".into()), 0, 7, "u64")
        );

        assert_tokens!(
            "1_000_u32",
            token_ty(TokenKind::Integer(10), Some("1_000".into()), 0, 9, "u32")
        );

        assert_tokens!(
            "0xDEAD_BEEF_u32",
            token_ty(TokenKind::Integer(16), Some("0xDEAD_BEEF".into()), 0, 15, "u32")
        );
    }

    #[test]
    fn test_string() {
        assert_token!("\"\"", TokenKind::String, Some(""), 0, 2);
        assert_token!("\"    \"", TokenKind::String, Some("    "), 0, 6);
        assert_token!("\"hello world\"", TokenKind::String, Some("hello world"), 0, 13);
        assert_token!("\"hello\nworld\"", TokenKind::String, Some("hello\nworld"), 0, 13);

        lex_all("\"hello world")
            .inspect_err(|err| {
                assert_eq!(&err.message(), "expected ending quote");
            })
            .unwrap_err();
    }
}
