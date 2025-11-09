#![allow(clippy::unnecessary_wraps, reason = "callbacks to Logos")]

use std::fmt::Display;
use std::marker::PhantomData;
use std::ops::{Deref, Range};
use std::sync::Arc;

use error_snippet::Result;
use logos::Logos;
use lume_span::SourceFile;

use crate::errors::*;

mod errors;

pub const OPERATOR_PRECEDENCE: &[(TokenKind, u8)] = &[
    (TokenKind::Assign, 1),
    (TokenKind::AddAssign, 1),
    (TokenKind::SubAssign, 1),
    (TokenKind::MulAssign, 1),
    (TokenKind::DivAssign, 1),
    (TokenKind::As, 2),
    (TokenKind::Is, 2),
    (TokenKind::BinaryAnd, 3),
    (TokenKind::BinaryXor, 3),
    (TokenKind::BinaryOr, 3),
    (TokenKind::And, 3),
    (TokenKind::Or, 3),
    (TokenKind::Equal, 4),
    (TokenKind::NotEqual, 4),
    (TokenKind::Greater, 5),
    (TokenKind::Less, 5),
    (TokenKind::GreaterEqual, 6),
    (TokenKind::LessEqual, 6),
    (TokenKind::Add, 7),
    (TokenKind::Sub, 7),
    (TokenKind::Mul, 8),
    (TokenKind::Div, 8),
    (TokenKind::Increment, 9),
    (TokenKind::Decrement, 9),
    (TokenKind::Dot, 10),
    (TokenKind::DotDot, 10),
    (TokenKind::PathSeparator, 11),
];

/// Defines the precedence for unary operators, such as `-` or `!`.
///
/// They cannot be defined within [`OPERATOR_PRECEDENCE`], as unary operators
/// share the same operators as other operators, but have different meanings.
///
/// For example, `-` can mean both "minus some amount", but when it's within
/// it's own expression before a number expression, it'd mean "the negative of
/// the following expression".
pub const UNARY_PRECEDENCE: u8 = 3;

/// Defines all the operators which are notated as postfix, as opposed to infix.
pub const POSTFIX_OPERATORS: &[TokenKind] = &[TokenKind::Increment, TokenKind::Decrement];

/// Defines all the operators which are used in binary contexts.
pub const BINARY_OPERATORS: &[TokenKind] = &[TokenKind::BinaryAnd, TokenKind::BinaryOr, TokenKind::BinaryXor];

/// Defines all the operators which are used in boolean contexts.
pub const BOOLEAN_OPERATORS: &[TokenKind] = &[TokenKind::And, TokenKind::Or];

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum LexerError {
    /// Invalid or unexpected character
    UnexpectedCharacter(char),

    /// String literal without any ending quote
    MissingEndingQuote,

    #[default]
    Other,
}

impl LexerError {
    /// Creates a new [`LexerError`] when the lexer found invalid tokens.
    fn from_lexer<'src>(lex: &mut logos::Lexer<'src, TokenKind<'src>>) -> <TokenKind<'src> as Logos<'src>>::Error {
        let c = lex.slice().chars().next().unwrap();

        LexerError::UnexpectedCharacter(c)
    }
}

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
#[logos(skip(r"\s+"))]
#[logos(error(LexerError, callback = LexerError::from_lexer))]
#[logos(subpattern int_binary = r"0[bB][0-1][_0-1]*")]
#[logos(subpattern int_octal = r"0[oO][0-8][_0-8]*")]
#[logos(subpattern int_dec = r"(0[dD])?[0-9][_0-9]*")]
#[logos(subpattern int_hex = r"0[xX][0-9a-fA-F][_0-9a-fA-F]*")]
#[logos(subpattern integer = r"(?&int_binary)|(?&int_octal)|(?&int_dec)|(?&int_hex)")]
#[logos(subpattern float_dec = r"\.[_0-9]+")]
#[logos(subpattern float_exp = r"[eE][-+]?[_0-9]+")]
#[logos(subpattern float = r"[0-9][_0-9]*(((?&float_dec)(?&float_exp)?)|(?&float_exp))?")]
pub enum TokenKind<'source> {
    #[token("as")]
    As,

    #[token("+")]
    Add,

    #[token("+=")]
    AddAssign,

    #[token("&&")]
    And,

    #[token("->")]
    Arrow,

    #[token("=>")]
    ArrowBig,

    #[token("=")]
    Assign,

    #[regex("///", lex_comment)]
    DocComment(&'source str),

    #[token("&")]
    BinaryAnd,

    #[token("|")]
    BinaryOr,

    #[token("^")]
    BinaryXor,

    #[token("break")]
    Break,

    #[token("builtin")]
    Builtin,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[regex("//", lex_comment)]
    Comment(&'source str),

    #[token("continue")]
    Continue,

    #[token("--")]
    Decrement,

    #[token("/")]
    Div,

    #[token("/=")]
    DivAssign,

    #[token(".")]
    Dot,

    #[token("..")]
    DotDot,

    #[token("...")]
    DotDotDot,

    #[token("else")]
    Else,

    #[token("enum")]
    Enum,

    Eof,

    #[token("==")]
    Equal,

    #[token("!")]
    Exclamation,

    #[token("external")]
    External,

    #[token("false")]
    False,

    #[token("fn")]
    Fn,

    #[regex("(?&float)", |_| Option::<FloatKind>::None, priority = 1)]
    #[regex("(?&float)f32", |_| Some(FloatKind::F32), priority = 4)]
    #[regex("(?&float)f64", |_| Some(FloatKind::F64), priority = 4)]
    Float(Option<FloatKind>),

    #[token("for")]
    For,

    #[token(">")]
    Greater,

    #[token(">=")]
    GreaterEqual,

    #[regex("[_a-zA-Z][_a-zA-Z0-9]*", |lex| lex.slice(), priority = 0)]
    Identifier(&'source str),

    #[token("if")]
    If,

    #[token("impl")]
    Impl,

    #[token("import")]
    Import,

    #[token("in")]
    In,

    #[token("is")]
    Is,

    #[token("++")]
    Increment,

    #[regex("(?&integer)", |lex| lex_integer(lex, None), priority = 2)]
    #[regex("(?&integer)i8", |lex| lex_integer(lex, Some(IntegerKind::I8)))]
    #[regex("(?&integer)i16", |lex| lex_integer(lex, Some(IntegerKind::I16)))]
    #[regex("(?&integer)i32", |lex| lex_integer(lex, Some(IntegerKind::I32)))]
    #[regex("(?&integer)i64", |lex| lex_integer(lex, Some(IntegerKind::I64)))]
    #[regex("(?&integer)u8", |lex| lex_integer(lex, Some(IntegerKind::U8)))]
    #[regex("(?&integer)u16", |lex| lex_integer(lex, Some(IntegerKind::U16)))]
    #[regex("(?&integer)u32", |lex| lex_integer(lex, Some(IntegerKind::U32)))]
    #[regex("(?&integer)u64", |lex| lex_integer(lex, Some(IntegerKind::U64)))]
    Integer((Radix, Option<IntegerKind>)),

    #[token("internal")]
    Internal,

    #[token("[")]
    LeftBracket,

    #[token("{")]
    LeftCurly,

    #[token("(")]
    LeftParen,

    #[token("<")]
    Less,

    #[token("<=")]
    LessEqual,

    #[token("let")]
    Let,

    #[token("loop")]
    Loop,

    #[token("*")]
    Mul,

    #[token("*=")]
    MulAssign,

    #[token("namespace")]
    Namespace,

    #[token("!=")]
    NotEqual,

    #[token("::")]
    PathSeparator,

    #[token("priv")]
    Priv,

    #[token("pub")]
    Pub,

    #[token("?")]
    Question,

    #[token("||")]
    Or,

    #[token("return")]
    Return,

    #[token("]")]
    RightBracket,

    #[token("}")]
    RightCurly,

    #[token(")")]
    RightParen,

    #[token(";")]
    Semicolon,

    #[token("self")]
    SelfRef,

    #[token("\"", lex_string)]
    String(&'source str),

    #[token("struct")]
    Struct,

    #[token("-")]
    Sub,

    #[token("-=")]
    SubAssign,

    #[token("switch")]
    Switch,

    #[token("trait")]
    Trait,

    #[token("true")]
    True,

    #[token("use")]
    Use,

    #[token("while")]
    While,
}

impl TokenKind<'_> {
    pub fn as_type(&self) -> TokenType {
        match self {
            TokenKind::As => TokenType::As,
            TokenKind::Add => TokenType::Add,
            TokenKind::AddAssign => TokenType::AddAssign,
            TokenKind::And => TokenType::And,
            TokenKind::Arrow => TokenType::Arrow,
            TokenKind::ArrowBig => TokenType::ArrowBig,
            TokenKind::Assign => TokenType::Assign,
            TokenKind::DocComment(_) => TokenType::DocComment,
            TokenKind::BinaryAnd => TokenType::BinaryAnd,
            TokenKind::BinaryOr => TokenType::BinaryOr,
            TokenKind::BinaryXor => TokenType::BinaryXor,
            TokenKind::Break => TokenType::Break,
            TokenKind::Builtin => TokenType::Builtin,
            TokenKind::Colon => TokenType::Colon,
            TokenKind::Comma => TokenType::Comma,
            TokenKind::Comment(_) => TokenType::Comment,
            TokenKind::Continue => TokenType::Continue,
            TokenKind::Decrement => TokenType::Decrement,
            TokenKind::Div => TokenType::Div,
            TokenKind::DivAssign => TokenType::DivAssign,
            TokenKind::Dot => TokenType::Dot,
            TokenKind::DotDot => TokenType::DotDot,
            TokenKind::DotDotDot => TokenType::DotDotDot,
            TokenKind::Else => TokenType::Else,
            TokenKind::Enum => TokenType::Enum,
            TokenKind::Eof => TokenType::Eof,
            TokenKind::Equal => TokenType::Equal,
            TokenKind::Exclamation => TokenType::Exclamation,
            TokenKind::External => TokenType::External,
            TokenKind::False => TokenType::False,
            TokenKind::Fn => TokenType::Fn,
            TokenKind::Float(_) => TokenType::Float,
            TokenKind::For => TokenType::For,
            TokenKind::Greater => TokenType::Greater,
            TokenKind::GreaterEqual => TokenType::GreaterEqual,
            TokenKind::Identifier(_) => TokenType::Identifier,
            TokenKind::If => TokenType::If,
            TokenKind::Impl => TokenType::Impl,
            TokenKind::Import => TokenType::Import,
            TokenKind::In => TokenType::In,
            TokenKind::Is => TokenType::Is,
            TokenKind::Increment => TokenType::Increment,
            TokenKind::Integer(_) => TokenType::Integer,
            TokenKind::Internal => TokenType::Internal,
            TokenKind::LeftBracket => TokenType::LeftBracket,
            TokenKind::LeftCurly => TokenType::LeftCurly,
            TokenKind::LeftParen => TokenType::LeftParen,
            TokenKind::Less => TokenType::Less,
            TokenKind::LessEqual => TokenType::LessEqual,
            TokenKind::Let => TokenType::Let,
            TokenKind::Loop => TokenType::Loop,
            TokenKind::Mul => TokenType::Mul,
            TokenKind::MulAssign => TokenType::MulAssign,
            TokenKind::Namespace => TokenType::Namespace,
            TokenKind::NotEqual => TokenType::NotEqual,
            TokenKind::PathSeparator => TokenType::PathSeparator,
            TokenKind::Priv => TokenType::Priv,
            TokenKind::Pub => TokenType::Pub,
            TokenKind::Question => TokenType::Question,
            TokenKind::Or => TokenType::Or,
            TokenKind::Return => TokenType::Return,
            TokenKind::RightBracket => TokenType::RightBracket,
            TokenKind::RightCurly => TokenType::RightCurly,
            TokenKind::RightParen => TokenType::RightParen,
            TokenKind::Semicolon => TokenType::Semicolon,
            TokenKind::SelfRef => TokenType::SelfRef,
            TokenKind::String(_) => TokenType::String,
            TokenKind::Struct => TokenType::Struct,
            TokenKind::Sub => TokenType::Sub,
            TokenKind::SubAssign => TokenType::SubAssign,
            TokenKind::Switch => TokenType::Switch,
            TokenKind::Trait => TokenType::Trait,
            TokenKind::True => TokenType::True,
            TokenKind::Use => TokenType::Use,
            TokenKind::While => TokenType::While,
        }
    }

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
                | TokenKind::Internal
                | TokenKind::Is
                | TokenKind::Loop
                | TokenKind::Namespace
                | TokenKind::Priv
                | TokenKind::Pub
                | TokenKind::Return
                | TokenKind::SelfRef
                | TokenKind::Struct
                | TokenKind::Switch
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
            TokenKind::Integer(_) | TokenKind::Float(_) | TokenKind::String(_) | TokenKind::False | TokenKind::True
        )
    }

    #[inline]
    pub fn is_unary(&self) -> bool {
        matches!(self, TokenKind::Exclamation | TokenKind::Sub)
    }

    #[inline]
    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::Add
                | TokenKind::AddAssign
                | TokenKind::And
                | TokenKind::Assign
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

    /// Gets the precedence of the token kind.
    ///
    /// Returns the precedence of the token kind, or 0 if the token kind is not
    /// an operator.
    #[inline]
    pub fn precedence(&self) -> u8 {
        OPERATOR_PRECEDENCE
            .iter()
            .find(|(k, _)| k == self)
            .map_or(0, |(_, p)| *p)
    }

    /// Determines whether the token is a postfix operator.
    #[inline]
    pub fn is_postfix(&self) -> bool {
        POSTFIX_OPERATORS.iter().any(|k| k == self)
    }

    /// Determines whether the token is a binary operator.
    #[inline]
    pub fn is_binary(&self) -> bool {
        BINARY_OPERATORS.iter().any(|k| k == self)
    }

    /// Determines whether the token is a boolean operator.
    #[inline]
    pub fn is_boolean(&self) -> bool {
        BOOLEAN_OPERATORS.iter().any(|k| k == self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    As,
    Add,
    AddAssign,
    And,
    Arrow,
    ArrowBig,
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
    Is,
    Increment,
    Integer,
    Internal,
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
    Switch,
    Trait,
    True,
    Use,
    While,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            TokenType::As => f.write_str("as"),
            TokenType::Add => f.write_str("+"),
            TokenType::AddAssign => f.write_str("+="),
            TokenType::And => f.write_str("&&"),
            TokenType::Arrow => f.write_str("->"),
            TokenType::ArrowBig => f.write_str("=>"),
            TokenType::Assign => f.write_str("="),
            TokenType::DocComment => f.write_str("doc comment"),
            TokenType::BinaryAnd => f.write_str("&"),
            TokenType::BinaryOr => f.write_str("|"),
            TokenType::BinaryXor => f.write_str("^"),
            TokenType::Break => f.write_str("break"),
            TokenType::Builtin => f.write_str("builtin"),
            TokenType::Colon => f.write_str(":"),
            TokenType::Comma => f.write_str(","),
            TokenType::Comment => f.write_str("comment"),
            TokenType::Continue => f.write_str("continue"),
            TokenType::Decrement => f.write_str("--"),
            TokenType::Div => f.write_str("/"),
            TokenType::DivAssign => f.write_str("/="),
            TokenType::Dot => f.write_str("."),
            TokenType::DotDot => f.write_str(".."),
            TokenType::DotDotDot => f.write_str("..."),
            TokenType::Else => f.write_str("else"),
            TokenType::Enum => f.write_str("enum"),
            TokenType::Eof => f.write_str("EOF"),
            TokenType::Equal => f.write_str("=="),
            TokenType::Exclamation => f.write_str("!"),
            TokenType::External => f.write_str("external"),
            TokenType::False => f.write_str("false"),
            TokenType::Fn => f.write_str("fn"),
            TokenType::Float => f.write_str("float"),
            TokenType::For => f.write_str("for"),
            TokenType::Greater => f.write_str(">"),
            TokenType::GreaterEqual => f.write_str(">="),
            TokenType::If => f.write_str("if"),
            TokenType::Identifier => f.write_str("identifier"),
            TokenType::Impl => f.write_str("impl"),
            TokenType::Import => f.write_str("import"),
            TokenType::In => f.write_str("in"),
            TokenType::Is => f.write_str("is"),
            TokenType::Increment => f.write_str("++"),
            TokenType::Internal => f.write_str("internal"),
            TokenType::LeftBracket => f.write_str("["),
            TokenType::LeftCurly => f.write_str("{"),
            TokenType::LeftParen => f.write_str("("),
            TokenType::Less => f.write_str("<"),
            TokenType::LessEqual => f.write_str("<="),
            TokenType::Let => f.write_str("let"),
            TokenType::Loop => f.write_str("loop"),
            TokenType::Mul => f.write_str("*"),
            TokenType::MulAssign => f.write_str("*="),
            TokenType::Namespace => f.write_str("namespace"),
            TokenType::NotEqual => f.write_str("!="),
            TokenType::Integer => f.write_str("integer"),
            TokenType::PathSeparator => f.write_str("::"),
            TokenType::Priv => f.write_str("priv"),
            TokenType::Pub => f.write_str("pub"),
            TokenType::Question => f.write_str("?"),
            TokenType::Or => f.write_str("||"),
            TokenType::Return => f.write_str("return"),
            TokenType::RightBracket => f.write_str("]"),
            TokenType::RightCurly => f.write_str("}"),
            TokenType::RightParen => f.write_str(")"),
            TokenType::SelfRef => f.write_str("self"),
            TokenType::Semicolon => f.write_str(";"),
            TokenType::String => f.write_str("string"),
            TokenType::Struct => f.write_str("struct"),
            TokenType::Sub => f.write_str("-"),
            TokenType::SubAssign => f.write_str("-="),
            TokenType::Switch => f.write_str("switch"),
            TokenType::Trait => f.write_str("trait"),
            TokenType::True => f.write_str("true"),
            TokenType::Use => f.write_str("use"),
            TokenType::While => f.write_str("while"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegerKind {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Radix {
    Binary,
    Octal,
    Hexadecimal,
    Decimal,
}

impl Radix {
    pub fn base(&self) -> u32 {
        match self {
            Self::Binary => 2,
            Self::Octal => 8,
            Self::Decimal => 10,
            Self::Hexadecimal => 16,
        }
    }
}

fn lex_integer<'src>(
    lex: &mut logos::Lexer<'src, TokenKind<'src>>,
    kind: Option<IntegerKind>,
) -> std::result::Result<(Radix, Option<IntegerKind>), <TokenKind<'src> as Logos<'src>>::Error> {
    let radix = match lex.slice().get(0..2) {
        Some("0b" | "0B") => Radix::Binary,
        Some("0o" | "0O") => Radix::Octal,
        Some("0x" | "0X") => Radix::Hexadecimal,
        _ => Radix::Decimal,
    };

    Ok((radix, kind))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatKind {
    F32,
    F64,
}

fn lex_comment<'src>(
    lex: &mut logos::Lexer<'src, TokenKind<'src>>,
) -> std::result::Result<&'src str, <TokenKind<'src> as Logos<'src>>::Error> {
    let remainder = lex.remainder();
    let line_length = remainder.find('\n').unwrap_or(remainder.len());

    lex.bump(line_length);

    Ok(lex.slice())
}

fn lex_string<'src>(
    lex: &mut logos::Lexer<'src, TokenKind<'src>>,
) -> std::result::Result<&'src str, <TokenKind<'src> as Logos<'src>>::Error> {
    let mut c = lex.remainder().chars();

    loop {
        match c.next() {
            None => return Err(LexerError::MissingEndingQuote),
            Some('"') => {
                lex.bump(1);
                break;
            }
            Some(_) => lex.bump(1),
        }
    }

    // Trim the quotes away from the slice
    let len = lex.slice().len();
    let trim = &lex.slice()[1..len - 1];

    Ok(trim)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'source> {
    /// Defines the kind of the token.
    pub kind: TokenKind<'source>,

    /// Defines the start and end position of the token.
    pub index: Range<usize>,
}

impl Token<'_> {
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
}

impl<'a> Deref for Token<'a> {
    type Target = TokenKind<'a>;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

#[derive(Debug)]
pub struct Lexer<'source> {
    /// Declares the source to lex tokens from.
    pub source: Arc<SourceFile>,

    _data: PhantomData<&'source ()>,
}

impl<'source> Lexer<'source> {
    /// Creates a new lexer instance.
    pub fn new(source: Arc<SourceFile>) -> Self {
        Lexer {
            source,
            _data: PhantomData,
        }
    }

    pub fn lex(&'source mut self) -> Result<Vec<Token<'source>>> {
        Self::lex_ref(&self.source.content)
    }

    pub fn lex_ref(content: &'source str) -> Result<Vec<Token<'source>>> {
        let source_file = Arc::new(SourceFile::internal(content));

        let mut tokens = Vec::new();
        let mut lexer = TokenKind::lexer(content);

        while let Some(kind) = lexer.next() {
            let kind = match kind {
                Ok(kind) => kind,
                Err(LexerError::UnexpectedCharacter(c)) => {
                    return Err(UnexpectedCharacter {
                        source: source_file.clone(),
                        range: lexer.span(),
                        char: c,
                    }
                    .into());
                }
                Err(LexerError::MissingEndingQuote) => {
                    return Err(MissingEndingQuote {
                        source: source_file.clone(),
                        range: lexer.span(),
                    }
                    .into());
                }
                Err(LexerError::Other) => unreachable!(),
            };

            tokens.push(Token {
                kind,
                index: lexer.span(),
            });
        }

        let last_idx = content.len().saturating_sub(1);

        tokens.push(Token {
            kind: TokenKind::Eof,
            index: last_idx.saturating_sub(1)..last_idx,
        });

        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[track_caller]
    pub fn assert_lex<'src>(
        source: &'src str,
        tokens: &[(
            std::result::Result<TokenKind, <TokenKind<'src> as Logos<'src>>::Error>,
            &'src str,
            Range<usize>,
        )],
    ) {
        let mut lex = TokenKind::lexer(source);

        for tuple in tokens {
            assert_eq!(&(lex.next().expect("unexpected end"), lex.slice(), lex.span()), tuple);
        }

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_keywords() {
        assert_lex("as", &[(Ok(TokenKind::As), "as", 0..2)]);
        assert_lex("break", &[(Ok(TokenKind::Break), "break", 0..5)]);
        assert_lex("builtin", &[(Ok(TokenKind::Builtin), "builtin", 0..7)]);
        assert_lex("continue", &[(Ok(TokenKind::Continue), "continue", 0..8)]);
        assert_lex("external", &[(Ok(TokenKind::External), "external", 0..8)]);
        assert_lex("for", &[(Ok(TokenKind::For), "for", 0..3)]);
        assert_lex("fn", &[(Ok(TokenKind::Fn), "fn", 0..2)]);
        assert_lex("if", &[(Ok(TokenKind::If), "if", 0..2)]);
        assert_lex("import", &[(Ok(TokenKind::Import), "import", 0..6)]);
        assert_lex("impl", &[(Ok(TokenKind::Impl), "impl", 0..4)]);
        assert_lex("in", &[(Ok(TokenKind::In), "in", 0..2)]);
        assert_lex("internal", &[(Ok(TokenKind::Internal), "internal", 0..8)]);
        assert_lex("is", &[(Ok(TokenKind::Is), "is", 0..2)]);
        assert_lex("loop", &[(Ok(TokenKind::Loop), "loop", 0..4)]);
        assert_lex("namespace", &[(Ok(TokenKind::Namespace), "namespace", 0..9)]);
        assert_lex("self", &[(Ok(TokenKind::SelfRef), "self", 0..4)]);
        assert_lex("struct", &[(Ok(TokenKind::Struct), "struct", 0..6)]);
        assert_lex("switch", &[(Ok(TokenKind::Switch), "switch", 0..6)]);
        assert_lex("return", &[(Ok(TokenKind::Return), "return", 0..6)]);
        assert_lex("true", &[(Ok(TokenKind::True), "true", 0..4)]);
        assert_lex("false", &[(Ok(TokenKind::False), "false", 0..5)]);
        assert_lex("priv", &[(Ok(TokenKind::Priv), "priv", 0..4)]);
        assert_lex("pub", &[(Ok(TokenKind::Pub), "pub", 0..3)]);
        assert_lex("use", &[(Ok(TokenKind::Use), "use", 0..3)]);
        assert_lex("while", &[(Ok(TokenKind::While), "while", 0..5)]);
    }

    #[test]
    fn test_symbols_map() {
        assert_lex("...", &[(Ok(TokenKind::DotDotDot), "...", 0..3)]);

        assert_lex("+=", &[(Ok(TokenKind::AddAssign), "+=", 0..2)]);
        assert_lex("&&", &[(Ok(TokenKind::And), "&&", 0..2)]);
        assert_lex("->", &[(Ok(TokenKind::Arrow), "->", 0..2)]);
        assert_lex("=>", &[(Ok(TokenKind::ArrowBig), "=>", 0..2)]);
        assert_lex("==", &[(Ok(TokenKind::Equal), "==", 0..2)]);
        assert_lex("--", &[(Ok(TokenKind::Decrement), "--", 0..2)]);
        assert_lex("..", &[(Ok(TokenKind::DotDot), "..", 0..2)]);
        assert_lex("/=", &[(Ok(TokenKind::DivAssign), "/=", 0..2)]);
        assert_lex(">=", &[(Ok(TokenKind::GreaterEqual), ">=", 0..2)]);
        assert_lex("++", &[(Ok(TokenKind::Increment), "++", 0..2)]);
        assert_lex("<=", &[(Ok(TokenKind::LessEqual), "<=", 0..2)]);
        assert_lex("*=", &[(Ok(TokenKind::MulAssign), "*=", 0..2)]);
        assert_lex("!=", &[(Ok(TokenKind::NotEqual), "!=", 0..2)]);
        assert_lex("||", &[(Ok(TokenKind::Or), "||", 0..2)]);
        assert_lex("::", &[(Ok(TokenKind::PathSeparator), "::", 0..2)]);
        assert_lex("-=", &[(Ok(TokenKind::SubAssign), "-=", 0..2)]);

        assert_lex("&", &[(Ok(TokenKind::BinaryAnd), "&", 0..1)]);
        assert_lex("|", &[(Ok(TokenKind::BinaryOr), "|", 0..1)]);
        assert_lex("^", &[(Ok(TokenKind::BinaryXor), "^", 0..1)]);
        assert_lex("=", &[(Ok(TokenKind::Assign), "=", 0..1)]);
        assert_lex(":", &[(Ok(TokenKind::Colon), ":", 0..1)]);
        assert_lex(",", &[(Ok(TokenKind::Comma), ",", 0..1)]);
        assert_lex("/", &[(Ok(TokenKind::Div), "/", 0..1)]);
        assert_lex(".", &[(Ok(TokenKind::Dot), ".", 0..1)]);
        assert_lex("!", &[(Ok(TokenKind::Exclamation), "!", 0..1)]);
        assert_lex(">", &[(Ok(TokenKind::Greater), ">", 0..1)]);
        assert_lex("[", &[(Ok(TokenKind::LeftBracket), "[", 0..1)]);
        assert_lex("{", &[(Ok(TokenKind::LeftCurly), "{", 0..1)]);
        assert_lex("(", &[(Ok(TokenKind::LeftParen), "(", 0..1)]);
        assert_lex("<", &[(Ok(TokenKind::Less), "<", 0..1)]);
        assert_lex("*", &[(Ok(TokenKind::Mul), "*", 0..1)]);
        assert_lex("]", &[(Ok(TokenKind::RightBracket), "]", 0..1)]);
        assert_lex("}", &[(Ok(TokenKind::RightCurly), "}", 0..1)]);
        assert_lex(")", &[(Ok(TokenKind::RightParen), ")", 0..1)]);
        assert_lex(";", &[(Ok(TokenKind::Semicolon), ";", 0..1)]);
        assert_lex("-", &[(Ok(TokenKind::Sub), "-", 0..1)]);
    }

    #[test]
    fn test_parenthesis() {
        assert_lex("()", &[
            (Ok(TokenKind::LeftParen), "(", 0..1),
            (Ok(TokenKind::RightParen), ")", 1..2),
        ]);
    }

    #[test]
    fn test_brackets() {
        assert_lex("[]", &[
            (Ok(TokenKind::LeftBracket), "[", 0..1),
            (Ok(TokenKind::RightBracket), "]", 1..2),
        ]);
    }

    #[test]
    fn test_curly_braces() {
        assert_lex("{}", &[
            (Ok(TokenKind::LeftCurly), "{", 0..1),
            (Ok(TokenKind::RightCurly), "}", 1..2),
        ]);
    }

    #[test]
    fn test_comment() {
        assert_lex("//", &[(Ok(TokenKind::Comment("//")), "//", 0..2)]);
        assert_lex("///", &[(Ok(TokenKind::DocComment("///")), "///", 0..3)]);

        assert_lex("// testing", &[(
            Ok(TokenKind::Comment("// testing")),
            "// testing",
            0..10,
        )]);

        assert_lex("/// testing", &[(
            Ok(TokenKind::DocComment("/// testing")),
            "/// testing",
            0..11,
        )]);

        assert_lex(
            "// comment 1
            // comment 2",
            &[
                (Ok(TokenKind::Comment("// comment 1")), "// comment 1", 0..12),
                (Ok(TokenKind::Comment("// comment 2")), "// comment 2", 25..37),
            ],
        );

        assert_lex(
            "// comment 1
            //
            // comment 2",
            &[
                (Ok(TokenKind::Comment("// comment 1")), "// comment 1", 0..12),
                (Ok(TokenKind::Comment("//")), "//", 25..27),
                (Ok(TokenKind::Comment("// comment 2")), "// comment 2", 40..52),
            ],
        );

        assert_lex(
            "/// comment 1
            /// comment 2",
            &[
                (Ok(TokenKind::DocComment("/// comment 1")), "/// comment 1", 0..13),
                (Ok(TokenKind::DocComment("/// comment 2")), "/// comment 2", 26..39),
            ],
        );

        assert_lex(
            "/// comment 1
            ///
            /// comment 2",
            &[
                (Ok(TokenKind::DocComment("/// comment 1")), "/// comment 1", 0..13),
                (Ok(TokenKind::DocComment("///")), "///", 26..29),
                (Ok(TokenKind::DocComment("/// comment 2")), "/// comment 2", 42..55),
            ],
        );

        assert_lex(
            "/// comment 1
            // comment 2",
            &[
                (Ok(TokenKind::DocComment("/// comment 1")), "/// comment 1", 0..13),
                (Ok(TokenKind::Comment("// comment 2")), "// comment 2", 26..38),
            ],
        );

        assert_lex("// comment 1  ", &[(
            Ok(TokenKind::Comment("// comment 1  ")),
            "// comment 1  ",
            0..14,
        )]);

        assert_lex("/// comment 1  ", &[(
            Ok(TokenKind::DocComment("/// comment 1  ")),
            "/// comment 1  ",
            0..15,
        )]);
    }

    #[test]
    fn test_identifier() {
        assert_lex("foo", &[(Ok(TokenKind::Identifier("foo")), "foo", 0..3)]);
        assert_lex("FOO", &[(Ok(TokenKind::Identifier("FOO")), "FOO", 0..3)]);
        assert_lex("_LUME", &[(Ok(TokenKind::Identifier("_LUME")), "_LUME", 0..5)]);
        assert_lex("__LUME__", &[(Ok(TokenKind::Identifier("__LUME__")), "__LUME__", 0..8)]);
        assert_lex("_", &[(Ok(TokenKind::Identifier("_")), "_", 0..1)]);
        assert_lex("_1", &[(Ok(TokenKind::Identifier("_1")), "_1", 0..2)]);
        assert_lex("_test1", &[(Ok(TokenKind::Identifier("_test1")), "_test1", 0..6)]);
    }

    #[test]
    fn test_number() {
        assert_lex("0 0000", &[
            (Ok(TokenKind::Integer((Radix::Decimal, None))), "0", 0..1),
            (Ok(TokenKind::Integer((Radix::Decimal, None))), "0000", 2..6),
        ]);

        assert_lex("0b01 0B01 0d01 0D01 0x01 0X01 0o01 0O01", &[
            (Ok(TokenKind::Integer((Radix::Binary, None))), "0b01", 0..4),
            (Ok(TokenKind::Integer((Radix::Binary, None))), "0B01", 5..9),
            (Ok(TokenKind::Integer((Radix::Decimal, None))), "0d01", 10..14),
            (Ok(TokenKind::Integer((Radix::Decimal, None))), "0D01", 15..19),
            (Ok(TokenKind::Integer((Radix::Hexadecimal, None))), "0x01", 20..24),
            (Ok(TokenKind::Integer((Radix::Hexadecimal, None))), "0X01", 25..29),
            (Ok(TokenKind::Integer((Radix::Octal, None))), "0o01", 30..34),
            (Ok(TokenKind::Integer((Radix::Octal, None))), "0O01", 35..39),
        ]);

        assert_lex("10.0 10.123", &[
            (Ok(TokenKind::Float(None)), "10.0", 0..4),
            (Ok(TokenKind::Float(None)), "10.123", 5..11),
        ]);

        assert_lex("0_0 00_00 1_000_000", &[
            (Ok(TokenKind::Integer((Radix::Decimal, None))), "0_0", 0..3),
            (Ok(TokenKind::Integer((Radix::Decimal, None))), "00_00", 4..9),
            (Ok(TokenKind::Integer((Radix::Decimal, None))), "1_000_000", 10..19),
        ]);

        assert_lex("-1", &[
            (Ok(TokenKind::Sub), "-", 0..1),
            (Ok(TokenKind::Integer((Radix::Decimal, None))), "1", 1..2),
        ]);

        assert_lex("10e5 10E5 1.0e5 1.0E5", &[
            (Ok(TokenKind::Float(None)), "10e5", 0..4),
            (Ok(TokenKind::Float(None)), "10E5", 5..9),
            (Ok(TokenKind::Float(None)), "1.0e5", 10..15),
            (Ok(TokenKind::Float(None)), "1.0E5", 16..21),
        ]);

        assert_lex("1_u32 1_f32 1.1_f64 0x0_u64 1_000_u32 0xDEAD_BEEF_u32", &[
            (
                Ok(TokenKind::Integer((Radix::Decimal, Some(IntegerKind::U32)))),
                "1_u32",
                0..5,
            ),
            (Ok(TokenKind::Float(Some(FloatKind::F32))), "1_f32", 6..11),
            (Ok(TokenKind::Float(Some(FloatKind::F64))), "1.1_f64", 12..19),
            (
                Ok(TokenKind::Integer((Radix::Hexadecimal, Some(IntegerKind::U64)))),
                "0x0_u64",
                20..27,
            ),
            (
                Ok(TokenKind::Integer((Radix::Decimal, Some(IntegerKind::U32)))),
                "1_000_u32",
                28..37,
            ),
            (
                Ok(TokenKind::Integer((Radix::Hexadecimal, Some(IntegerKind::U32)))),
                "0xDEAD_BEEF_u32",
                38..53,
            ),
        ]);
    }

    #[test]
    fn test_string() {
        assert_lex("\"\"", &[(Ok(TokenKind::String("")), "\"\"", 0..2)]);
        assert_lex("\"    \"", &[(Ok(TokenKind::String("    ")), "\"    \"", 0..6)]);

        assert_lex("\"hello world\"", &[(
            Ok(TokenKind::String("hello world")),
            "\"hello world\"",
            0..13,
        )]);

        assert_lex("\"hello\nworld\"", &[(
            Ok(TokenKind::String("hello\nworld")),
            "\"hello\nworld\"",
            0..13,
        )]);

        assert_lex("\"", &[(Err(LexerError::MissingEndingQuote), "\"", 0..1)]);

        assert_lex("\"hello world", &[(
            Err(LexerError::MissingEndingQuote),
            "\"hello world",
            0..12,
        )]);
    }
}
