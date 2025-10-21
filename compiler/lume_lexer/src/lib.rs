use std::ops::Range;
use std::sync::Arc;

use error_snippet::Result;
use logos::Logos;
use lume_ast::Identifier;
use lume_span::SourceFile;

use crate::errors::*;

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
    (IDENTIFIER_SEPARATOR, 11),
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
    fn from_lexer<'src>(lex: &mut logos::Lexer<'src, TokenType<'src>>) -> <TokenType<'src> as Logos<'src>>::Error {
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
pub enum TokenType<'source> {
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

impl TokenType<'_> {
    #[inline]
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            TokenType::As
                | TokenType::Break
                | TokenType::Continue
                | TokenType::Else
                | TokenType::External
                | TokenType::False
                | TokenType::Fn
                | TokenType::For
                | TokenType::If
                | TokenType::Impl
                | TokenType::Import
                | TokenType::In
                | TokenType::Is
                | TokenType::Loop
                | TokenType::Namespace
                | TokenType::Priv
                | TokenType::Pub
                | TokenType::Return
                | TokenType::SelfRef
                | TokenType::Struct
                | TokenType::Switch
                | TokenType::Trait
                | TokenType::True
                | TokenType::Use
                | TokenType::While
        )
    }

    #[inline]
    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            TokenType::Integer(_) | TokenType::Float(_) | TokenType::String(_) | TokenType::False | TokenType::True
        )
    }

    #[inline]
    pub fn is_unary(&self) -> bool {
        matches!(self, TokenType::Exclamation | TokenType::Sub)
    }

    #[inline]
    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            TokenType::Add
                | TokenType::AddAssign
                | TokenType::And
                | TokenType::Assign
                | TokenType::BinaryAnd
                | TokenType::BinaryOr
                | TokenType::BinaryXor
                | TokenType::Decrement
                | TokenType::Div
                | TokenType::DivAssign
                | TokenType::Equal
                | TokenType::Greater
                | TokenType::GreaterEqual
                | TokenType::Increment
                | TokenType::Less
                | TokenType::LessEqual
                | TokenType::Mul
                | TokenType::MulAssign
                | TokenType::NotEqual
                | TokenType::Or
                | TokenType::Sub
                | TokenType::SubAssign
        )
    }

    // /// Gets the precedence of the token kind.
    // ///
    // /// Returns the precedence of the token kind, or 0 if the token kind is not
    // /// an operator.
    // #[inline]
    // pub fn precedence(&self) -> u8 {
    //     OPERATOR_PRECEDENCE
    //         .iter()
    //         .find(|(k, _)| k == self)
    //         .map_or(0, |(_, p)| *p)
    // }

    // /// Determines whether the token is a postfix operator.
    // #[inline]
    // pub fn is_postfix(&self) -> bool {
    //     POSTFIX_OPERATORS.iter().any(|k| k == self)
    // }

    // /// Determines whether the token is a binary operator.
    // #[inline]
    // pub fn is_binary(&self) -> bool {
    //     BINARY_OPERATORS.iter().any(|k| k == self)
    // }

    // /// Determines whether the token is a boolean operator.
    // #[inline]
    // pub fn is_boolean(&self) -> bool {
    //     BOOLEAN_OPERATORS.iter().any(|k| k == self)
    // }
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

fn lex_integer<'src>(
    lex: &mut logos::Lexer<'src, TokenType<'src>>,
    kind: Option<IntegerKind>,
) -> std::result::Result<(Radix, Option<IntegerKind>), <TokenType<'src> as Logos<'src>>::Error> {
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
    lex: &mut logos::Lexer<'src, TokenType<'src>>,
) -> std::result::Result<&'src str, <TokenType<'src> as Logos<'src>>::Error> {
    let remainder = lex.remainder();
    let line_length = remainder.find('\n').unwrap_or(remainder.len());

    lex.bump(line_length);

    Ok(lex.slice())
}

fn lex_string<'src>(
    lex: &mut logos::Lexer<'src, TokenType<'src>>,
) -> std::result::Result<&'src str, <TokenType<'src> as Logos<'src>>::Error> {
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

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[track_caller]
    pub fn assert_lex<'src>(
        source: &'src str,
        tokens: &[(
            std::result::Result<TokenType, <TokenType<'src> as Logos<'src>>::Error>,
            &'src str,
            Range<usize>,
        )],
    ) {
        let mut lex = TokenType::lexer(source);

        for tuple in tokens {
            assert_eq!(&(lex.next().expect("unexpected end"), lex.slice(), lex.span()), tuple);
        }

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_keywords() {
        assert_lex("as", &[(Ok(TokenType::As), "as", 0..2)]);
        assert_lex("break", &[(Ok(TokenType::Break), "break", 0..5)]);
        assert_lex("continue", &[(Ok(TokenType::Continue), "continue", 0..8)]);
        assert_lex("external", &[(Ok(TokenType::External), "external", 0..8)]);
        assert_lex("for", &[(Ok(TokenType::For), "for", 0..3)]);
        assert_lex("fn", &[(Ok(TokenType::Fn), "fn", 0..2)]);
        assert_lex("if", &[(Ok(TokenType::If), "if", 0..2)]);
        assert_lex("import", &[(Ok(TokenType::Import), "import", 0..6)]);
        assert_lex("impl", &[(Ok(TokenType::Impl), "impl", 0..4)]);
        assert_lex("in", &[(Ok(TokenType::In), "in", 0..2)]);
        assert_lex("is", &[(Ok(TokenType::Is), "is", 0..2)]);
        assert_lex("loop", &[(Ok(TokenType::Loop), "loop", 0..4)]);
        assert_lex("namespace", &[(Ok(TokenType::Namespace), "namespace", 0..9)]);
        assert_lex("self", &[(Ok(TokenType::SelfRef), "self", 0..4)]);
        assert_lex("struct", &[(Ok(TokenType::Struct), "struct", 0..6)]);
        assert_lex("switch", &[(Ok(TokenType::Switch), "switch", 0..6)]);
        assert_lex("return", &[(Ok(TokenType::Return), "return", 0..6)]);
        assert_lex("true", &[(Ok(TokenType::True), "true", 0..4)]);
        assert_lex("false", &[(Ok(TokenType::False), "false", 0..5)]);
        assert_lex("priv", &[(Ok(TokenType::Priv), "priv", 0..4)]);
        assert_lex("pub", &[(Ok(TokenType::Pub), "pub", 0..3)]);
        assert_lex("use", &[(Ok(TokenType::Use), "use", 0..3)]);
        assert_lex("while", &[(Ok(TokenType::While), "while", 0..5)]);
    }

    #[test]
    fn test_symbols_map() {
        assert_lex("...", &[(Ok(TokenType::DotDotDot), "...", 0..3)]);

        assert_lex("+=", &[(Ok(TokenType::AddAssign), "+=", 0..2)]);
        assert_lex("&&", &[(Ok(TokenType::And), "&&", 0..2)]);
        assert_lex("->", &[(Ok(TokenType::Arrow), "->", 0..2)]);
        assert_lex("=>", &[(Ok(TokenType::ArrowBig), "=>", 0..2)]);
        assert_lex("==", &[(Ok(TokenType::Equal), "==", 0..2)]);
        assert_lex("--", &[(Ok(TokenType::Decrement), "--", 0..2)]);
        assert_lex("..", &[(Ok(TokenType::DotDot), "..", 0..2)]);
        assert_lex("/=", &[(Ok(TokenType::DivAssign), "/=", 0..2)]);
        assert_lex(">=", &[(Ok(TokenType::GreaterEqual), ">=", 0..2)]);
        assert_lex("++", &[(Ok(TokenType::Increment), "++", 0..2)]);
        assert_lex("<=", &[(Ok(TokenType::LessEqual), "<=", 0..2)]);
        assert_lex("*=", &[(Ok(TokenType::MulAssign), "*=", 0..2)]);
        assert_lex("!=", &[(Ok(TokenType::NotEqual), "!=", 0..2)]);
        assert_lex("||", &[(Ok(TokenType::Or), "||", 0..2)]);
        assert_lex("::", &[(Ok(TokenType::PathSeparator), "::", 0..2)]);
        assert_lex("-=", &[(Ok(TokenType::SubAssign), "-=", 0..2)]);

        assert_lex("&", &[(Ok(TokenType::BinaryAnd), "&", 0..1)]);
        assert_lex("|", &[(Ok(TokenType::BinaryOr), "|", 0..1)]);
        assert_lex("^", &[(Ok(TokenType::BinaryXor), "^", 0..1)]);
        assert_lex("=", &[(Ok(TokenType::Assign), "=", 0..1)]);
        assert_lex(":", &[(Ok(TokenType::Colon), ":", 0..1)]);
        assert_lex(",", &[(Ok(TokenType::Comma), ",", 0..1)]);
        assert_lex("/", &[(Ok(TokenType::Div), "/", 0..1)]);
        assert_lex(".", &[(Ok(TokenType::Dot), ".", 0..1)]);
        assert_lex("!", &[(Ok(TokenType::Exclamation), "!", 0..1)]);
        assert_lex(">", &[(Ok(TokenType::Greater), ">", 0..1)]);
        assert_lex("[", &[(Ok(TokenType::LeftBracket), "[", 0..1)]);
        assert_lex("{", &[(Ok(TokenType::LeftCurly), "{", 0..1)]);
        assert_lex("(", &[(Ok(TokenType::LeftParen), "(", 0..1)]);
        assert_lex("<", &[(Ok(TokenType::Less), "<", 0..1)]);
        assert_lex("*", &[(Ok(TokenType::Mul), "*", 0..1)]);
        assert_lex("]", &[(Ok(TokenType::RightBracket), "]", 0..1)]);
        assert_lex("}", &[(Ok(TokenType::RightCurly), "}", 0..1)]);
        assert_lex(")", &[(Ok(TokenType::RightParen), ")", 0..1)]);
        assert_lex(";", &[(Ok(TokenType::Semicolon), ";", 0..1)]);
        assert_lex("-", &[(Ok(TokenType::Sub), "-", 0..1)]);
    }

    #[test]
    fn test_parenthesis() {
        assert_lex("()", &[
            (Ok(TokenType::LeftParen), "(", 0..1),
            (Ok(TokenType::RightParen), ")", 1..2),
        ]);
    }

    #[test]
    fn test_brackets() {
        assert_lex("[]", &[
            (Ok(TokenType::LeftBracket), "[", 0..1),
            (Ok(TokenType::RightBracket), "]", 1..2),
        ]);
    }

    #[test]
    fn test_curly_braces() {
        assert_lex("{}", &[
            (Ok(TokenType::LeftCurly), "{", 0..1),
            (Ok(TokenType::RightCurly), "}", 1..2),
        ]);
    }

    #[test]
    fn test_comment() {
        assert_lex("//", &[(Ok(TokenType::Comment("//")), "//", 0..2)]);
        assert_lex("///", &[(Ok(TokenType::DocComment("///")), "///", 0..3)]);

        assert_lex("// testing", &[(
            Ok(TokenType::Comment("// testing")),
            "// testing",
            0..10,
        )]);

        assert_lex("/// testing", &[(
            Ok(TokenType::DocComment("/// testing")),
            "/// testing",
            0..11,
        )]);

        assert_lex(
            "// comment 1
            // comment 2",
            &[
                (Ok(TokenType::Comment("// comment 1")), "// comment 1", 0..12),
                (Ok(TokenType::Comment("// comment 2")), "// comment 2", 25..37),
            ],
        );

        assert_lex(
            "// comment 1
            //
            // comment 2",
            &[
                (Ok(TokenType::Comment("// comment 1")), "// comment 1", 0..12),
                (Ok(TokenType::Comment("//")), "//", 25..27),
                (Ok(TokenType::Comment("// comment 2")), "// comment 2", 40..52),
            ],
        );

        assert_lex(
            "/// comment 1
            /// comment 2",
            &[
                (Ok(TokenType::DocComment("/// comment 1")), "/// comment 1", 0..13),
                (Ok(TokenType::DocComment("/// comment 2")), "/// comment 2", 26..39),
            ],
        );

        assert_lex(
            "/// comment 1
            ///
            /// comment 2",
            &[
                (Ok(TokenType::DocComment("/// comment 1")), "/// comment 1", 0..13),
                (Ok(TokenType::DocComment("///")), "///", 26..29),
                (Ok(TokenType::DocComment("/// comment 2")), "/// comment 2", 42..55),
            ],
        );

        assert_lex(
            "/// comment 1
            // comment 2",
            &[
                (Ok(TokenType::DocComment("/// comment 1")), "/// comment 1", 0..13),
                (Ok(TokenType::Comment("// comment 2")), "// comment 2", 26..38),
            ],
        );

        assert_lex("// comment 1  ", &[(
            Ok(TokenType::Comment("// comment 1  ")),
            "// comment 1  ",
            0..14,
        )]);

        assert_lex("/// comment 1  ", &[(
            Ok(TokenType::DocComment("/// comment 1  ")),
            "/// comment 1  ",
            0..15,
        )]);
    }

    #[test]
    fn test_identifier() {
        assert_lex("foo", &[(Ok(TokenType::Identifier("foo")), "foo", 0..3)]);
        assert_lex("FOO", &[(Ok(TokenType::Identifier("FOO")), "FOO", 0..3)]);
        assert_lex("_LUME", &[(Ok(TokenType::Identifier("_LUME")), "_LUME", 0..5)]);
        assert_lex("__LUME__", &[(Ok(TokenType::Identifier("__LUME__")), "__LUME__", 0..8)]);
        assert_lex("_", &[(Ok(TokenType::Identifier("_")), "_", 0..1)]);
        assert_lex("_1", &[(Ok(TokenType::Identifier("_1")), "_1", 0..2)]);
        assert_lex("_test1", &[(Ok(TokenType::Identifier("_test1")), "_test1", 0..6)]);
    }

    #[test]
    fn test_number() {
        assert_lex("0 0000", &[
            (Ok(TokenType::Integer((Radix::Decimal, None))), "0", 0..1),
            (Ok(TokenType::Integer((Radix::Decimal, None))), "0000", 2..6),
        ]);

        assert_lex("0b01 0B01 0d01 0D01 0x01 0X01 0o01 0O01", &[
            (Ok(TokenType::Integer((Radix::Binary, None))), "0b01", 0..4),
            (Ok(TokenType::Integer((Radix::Binary, None))), "0B01", 5..9),
            (Ok(TokenType::Integer((Radix::Decimal, None))), "0d01", 10..14),
            (Ok(TokenType::Integer((Radix::Decimal, None))), "0D01", 15..19),
            (Ok(TokenType::Integer((Radix::Hexadecimal, None))), "0x01", 20..24),
            (Ok(TokenType::Integer((Radix::Hexadecimal, None))), "0X01", 25..29),
            (Ok(TokenType::Integer((Radix::Octal, None))), "0o01", 30..34),
            (Ok(TokenType::Integer((Radix::Octal, None))), "0O01", 35..39),
        ]);

        assert_lex("10.0 10.123", &[
            (Ok(TokenType::Float(None)), "10.0", 0..4),
            (Ok(TokenType::Float(None)), "10.123", 5..11),
        ]);

        assert_lex("0_0 00_00 1_000_000", &[
            (Ok(TokenType::Integer((Radix::Decimal, None))), "0_0", 0..3),
            (Ok(TokenType::Integer((Radix::Decimal, None))), "00_00", 4..9),
            (Ok(TokenType::Integer((Radix::Decimal, None))), "1_000_000", 10..19),
        ]);

        assert_lex("-1", &[
            (Ok(TokenType::Sub), "-", 0..1),
            (Ok(TokenType::Integer((Radix::Decimal, None))), "1", 1..2),
        ]);

        assert_lex("10e5 10E5 1.0e5 1.0E5", &[
            (Ok(TokenType::Float(None)), "10e5", 0..4),
            (Ok(TokenType::Float(None)), "10E5", 5..9),
            (Ok(TokenType::Float(None)), "1.0e5", 10..15),
            (Ok(TokenType::Float(None)), "1.0E5", 16..21),
        ]);

        assert_lex("1_u32 1_f32 1.1_f64 0x0_u64 1_000_u32 0xDEAD_BEEF_u32", &[
            (
                Ok(TokenType::Integer((Radix::Decimal, Some(IntegerKind::U32)))),
                "1_u32",
                0..5,
            ),
            (Ok(TokenType::Float(Some(FloatKind::F32))), "1_f32", 6..11),
            (Ok(TokenType::Float(Some(FloatKind::F64))), "1.1_f64", 12..19),
            (
                Ok(TokenType::Integer((Radix::Hexadecimal, Some(IntegerKind::U64)))),
                "0x0_u64",
                20..27,
            ),
            (
                Ok(TokenType::Integer((Radix::Decimal, Some(IntegerKind::U32)))),
                "1_000_u32",
                28..37,
            ),
            (
                Ok(TokenType::Integer((Radix::Hexadecimal, Some(IntegerKind::U32)))),
                "0xDEAD_BEEF_u32",
                38..53,
            ),
        ]);
    }

    #[test]
    fn test_string() {
        assert_lex("\"\"", &[(Ok(TokenType::String("")), "\"\"", 0..2)]);
        assert_lex("\"    \"", &[(Ok(TokenType::String("    ")), "\"    \"", 0..6)]);

        assert_lex("\"hello world\"", &[(
            Ok(TokenType::String("hello world")),
            "\"hello world\"",
            0..13,
        )]);

        assert_lex("\"hello\nworld\"", &[(
            Ok(TokenType::String("hello\nworld")),
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

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
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
    Newline,
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
            TokenKind::ArrowBig => "=>",
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
            TokenKind::Is => "is",
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
            TokenKind::Newline => "\\n",
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
            TokenKind::Switch => "switch",
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

    /// Defines the "kind" of the token. This is only used for integer- and
    /// floating-point literals.
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
    /// Returns the precedence of the token kind, or 0 if the token kind is not
    /// an operator.
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

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Whitespace {
    #[default]
    Ignore,
    Newline,
}

#[derive(Debug)]
pub struct Lexer {
    /// Declares the source to lex tokens from.
    pub source: Arc<SourceFile>,

    /// Defines how to lex whitespace in the source content.
    whitespace: Whitespace,

    /// Represents an index of all codepoints within the source content.
    indexed_source: Vec<usize>,

    /// Defines the current position in the source.
    position: usize,

    /// Defines the total length of the source code.
    length: usize,
}

impl Lexer {
    /// Creates a new lexer instance.
    pub fn new(source: Arc<SourceFile>) -> Self {
        let indexed_source = source.content.char_indices().map(|(i, _)| i).collect();

        Lexer {
            length: source.content.len(),
            whitespace: Whitespace::default(),
            indexed_source,
            source,
            position: 0,
        }
    }

    #[inline]
    pub fn enable_whitespace(&mut self) {
        self.whitespace = Whitespace::Newline;
    }

    #[inline]
    pub fn is_eof(&self) -> bool {
        self.position >= self.indexed_source.len()
    }

    /// Tries to get the character at the current cursor position.
    ///
    /// Returns `None` if the cursor is at the end of the source.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn current_char(&self) -> Option<char> {
        self.at_offset(0)
    }

    /// Tries to get the character at the current cursor position.
    ///
    /// Returns `\0` if the cursor is at the end of the source.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn current_char_or_eof(&self) -> char {
        self.current_char().unwrap_or('\0')
    }

    /// Tries to get the character which is at the current cursor position,
    /// offset by `offset`.
    ///
    /// Returns `None` if the cursor is at the end of the source.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn at_offset(&self, offset: usize) -> Option<char> {
        let idx = self.position + offset;

        if idx >= self.indexed_source.len() {
            return None;
        }

        let start = self.indexed_source[idx];
        let end = if idx + 1 < self.indexed_source.len() {
            self.indexed_source[idx + 1]
        } else {
            self.length
        };

        self.source.content.get(start..end)?.chars().next()
    }

    /// Tries to get the character which is at the current cursor position,
    /// offset by `offset`.
    ///
    /// Returns `\0` if the cursor is at the end of the source.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn at_offset_or_eof(&self, offset: usize) -> char {
        self.at_offset(offset).unwrap_or('\0')
    }

    /// Tries to get the slice of the content stream, given the index and
    /// length.
    ///
    /// Returns an empty string if the cursor is at the end of the source.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn content_slice(&self, start: usize, mut end: usize) -> &str {
        if start > end || start >= self.indexed_source.len() {
            return "";
        }

        if end >= self.indexed_source.len() {
            end = self.indexed_source.len();
        }

        let byte_start = self.indexed_source[start];
        let byte_end = if end < self.indexed_source.len() {
            self.indexed_source[end]
        } else {
            self.length
        };

        self.source.content.get(byte_start..byte_end).unwrap_or_default()
    }

    /// Advances the cursor position of the lexer to the next line.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn next(&mut self) {
        self.position += 1;
    }

    /// Gets the character at the current cursor position and advances the
    /// cursor position to the next position.
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

        let sliced = self.content_slice(start, self.length);
        let found_idx = match sliced.find(c).map(|i| i + start) {
            Some(idx) => idx,
            None => self.source.content.len(),
        };

        self.position = found_idx;
        self.content_slice(start, found_idx)
    }

    /// Gets characters while the predicate returns `true`.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn take_while(&mut self, predicate: impl Fn(char) -> bool) -> String {
        let start = self.position;

        self.eat_while(predicate);

        self.content_slice(start, self.position).to_string()
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

    /// Gets the token at the current cursor position. The cursor is advanced to
    /// the start of the next token.
    ///
    /// # Errors
    ///
    /// This method will return `Err` if the expected token was formatted
    /// incorrectly, or if the lexer unexpectedly encountered end-of-file.
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

        let Some(first_char) = self.current_char() else {
            return Ok(Token::empty(TokenKind::Eof));
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

            // Newlines
            '\n' | '\r' if self.whitespace == Whitespace::Newline => {
                self.next();

                return Ok(Token {
                    kind: TokenKind::Newline,
                    index: (start_idx..start_idx + 1),
                    value: None,
                    ty: None,
                });
            }

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
    fn comment(&mut self) -> Option<Token> {
        let kind = self.eat_comment_prefix()?;
        let content = self.take_until('\n').trim().to_string();

        Some(Token::new(kind, content))
    }

    #[tracing::instrument(level = "TRACE", skip(self), ret)]
    fn eat_comment_prefix(&mut self) -> Option<TokenKind> {
        // Skip over all the whitespace characters, before attempting to eat the comment
        // prefix.
        self.eat_while(char::is_whitespace);

        // Eat the comment prefix characters.
        match self.take_while(|c| c == '/').len() {
            0..=1 => None,
            3 => Some(TokenKind::DocComment),
            _ => Some(TokenKind::Comment),
        }
    }

    /// Parses a block of comment tokens at the current cursor position.
    #[tracing::instrument(level = "DEBUG", skip(self), ret)]
    fn comment_block(&mut self) -> Token {
        let mut kind = TokenKind::Comment;
        let mut comments = Vec::new();

        loop {
            let position = self.position;
            let Some(token) = self.comment() else {
                break;
            };

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
            "is" => Token::empty(TokenKind::Is),
            "let" => Token::empty(TokenKind::Let),
            "loop" => Token::empty(TokenKind::Loop),
            "namespace" => Token::empty(TokenKind::Namespace),
            "priv" => Token::empty(TokenKind::Priv),
            "pub" => Token::empty(TokenKind::Pub),
            "return" => Token::empty(TokenKind::Return),
            "self" => Token::empty(TokenKind::SelfRef),
            "struct" => Token::empty(TokenKind::Struct),
            "switch" => Token::empty(TokenKind::Switch),
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
        let slice = self.content_slice(self.position, self.position + max_len);

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
                ('=', '>') => return Ok((TokenKind::ArrowBig, 2)),
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

        // Attempt to parse any following groups, such as decimal point or float
        // exponent.
        match self.current_char_or_eof() {
            '.' if self.at_offset_or_eof(1).is_ascii_digit() => {
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
        let number_str = self.content_slice(start_index, end_index).to_string();

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
    /// Radix prefixes are used to specify the base of a number literal, such
    /// as:
    ///   - Binary: `0b1010`
    ///   - Octal: `0o755`
    ///   - Hexadecimal: `0x1A`
    ///
    /// This method consumes characters from the token stream until it
    /// encounters a non-digit character. If no radix prefix is found, the
    /// method reads all base-10 digits.
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
        assert_token!("is", TokenKind::Is, None::<String>, 0, 2);
        assert_token!("loop", TokenKind::Loop, None::<String>, 0, 4);
        assert_token!("namespace", TokenKind::Namespace, None::<String>, 0, 9);
        assert_token!("self", TokenKind::SelfRef, None::<String>, 0, 4);
        assert_token!("struct", TokenKind::Struct, None::<String>, 0, 6);
        assert_token!("switch", TokenKind::Switch, None::<String>, 0, 6);
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
        assert_token!("=>", TokenKind::ArrowBig, Some("=>"), 0, 2);
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

    #[test]
    fn test_fuzz() {
        lex_all("1_1_").unwrap();
    }
}
