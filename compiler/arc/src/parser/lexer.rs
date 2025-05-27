use std::ops::Range;
use std::sync::Arc;

use error_snippet::{IntoDiagnostic, Result};
use error_snippet_derive::Diagnostic;
use lume_span::SourceFile;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "end of file", code = "ARC0100", help = "has the file been fully written?")]
pub struct UnexpectedEndOfFile {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("unexpected end-of-file")]
    pub range: Range<usize>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "unexpected character", code = "ARC0101", help = "check your syntax")]
pub struct UnexpectedCharacter {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("unexpected character '{char}'")]
    pub range: Range<usize>,

    pub char: char,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "invalid integer", code = "ARC0150")]
pub struct InvalidInteger {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("the integer value '{value:?}' is invalid")]
    pub range: Range<usize>,

    pub value: String,

    #[cause]
    pub reason: Vec<error_snippet::Error>,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "expected ending quote",
    code = "ARC0152",
    help = "did you forget to end your string?"
)]
pub struct MissingEndingQuote {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("string literal was started, but has no matching end quote")]
    pub range: Range<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Identifier(String),
    String(String),
    Integer(i64),
    Comma,
    Equal,
    BraceLeft,
    BraceRight,
    CurlyLeft,
    CurlyRight,
    Eof,
}

impl TokenKind {
    pub fn is_value(&self) -> bool {
        matches!(self, TokenKind::String(_) | TokenKind::Integer(_))
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(val) | Self::String(val) => write!(f, "{val}"),
            Self::Integer(val) => write!(f, "{val}"),
            Self::Comma => write!(f, ","),
            Self::Equal => write!(f, "="),
            Self::BraceLeft => write!(f, "["),
            Self::BraceRight => write!(f, "]"),
            Self::CurlyLeft => write!(f, "{{"),
            Self::CurlyRight => write!(f, "}}"),
            Self::Eof => write!(f, "EOF"),
        }
    }
}

const SYMBOLS: &[char] = &[',', '.', '=', '[', ']', '{', '}'];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    /// Defines the kind of the token.
    pub kind: TokenKind,

    /// Defines the start and end position of the token.
    pub index: Range<usize>,
}

#[allow(dead_code)]
impl Token {
    pub fn len(&self) -> usize {
        self.index.end - self.index.start
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn start(&self) -> usize {
        self.index.start
    }

    pub fn end(&self) -> usize {
        self.index.end
    }
}

#[derive(Debug)]
pub struct Lexer {
    /// Declares the source to lex tokens from.
    pub source: Arc<SourceFile>,

    /// Defines the current position in the source.
    position: usize,
}

impl Lexer {
    /// Creates a new [`Lexer`] instance with the given source file.
    pub fn new(source: Arc<SourceFile>) -> Self {
        Lexer { source, position: 0 }
    }

    /// Lexes all the [`Token`]s from the current cursor position. The [`Lexer`] will
    /// continue until it encounters end-of-file on the source input.
    ///
    /// # Errors
    ///
    /// This method will return `Err` if the expected token was formatted incorrectly,
    /// or if the lexer unexpectedly encountered end-of-file.
    #[allow(clippy::range_plus_one, reason = "type only accepts `Range<usize>`")]
    pub fn all(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next()?;
            if token.kind == TokenKind::Eof {
                break;
            }

            tokens.push(token);
        }

        Ok(tokens)
    }

    /// Lexes the [`Token`] at the current cursor position. The cursor is advanced
    /// to the start of the next token.
    ///
    /// # Errors
    ///
    /// This method will return `Err` if the expected token was formatted incorrectly,
    /// or if the lexer unexpectedly encountered end-of-file.
    #[allow(clippy::range_plus_one, reason = "type only accepts `Range<usize>`")]
    pub fn next(&mut self) -> Result<Token> {
        let start_idx = self.position;
        let source = self.source.clone();

        if self.is_eof() {
            return Ok(Token {
                kind: TokenKind::Eof,
                index: start_idx..start_idx,
            });
        }

        let token = match self.current_char_or_eof()? {
            // Identifiers, such as keywords and standalone words.
            'a'..='z' | 'A'..='Z' | '_' => {
                let end_idx = self.move_while(|c| c.is_ascii_alphanumeric() || c == '_');
                let content = &self.source.content[start_idx..end_idx];

                Token {
                    kind: TokenKind::Identifier(content.to_string()),
                    index: start_idx..end_idx,
                }
            }

            // Symbols, such as operators and punctuation.
            c if SYMBOLS.contains(&c) => {
                let kind = match c {
                    ',' => TokenKind::Comma,
                    '=' => TokenKind::Equal,
                    '[' => TokenKind::BraceLeft,
                    ']' => TokenKind::BraceRight,
                    '{' => TokenKind::CurlyLeft,
                    '}' => TokenKind::CurlyRight,
                    _ => {
                        return Err(UnexpectedCharacter {
                            source,
                            range: start_idx..self.position,
                            char: c,
                        }
                        .into());
                    }
                };

                Token {
                    kind,
                    index: start_idx..self.position + 1,
                }
            }

            // Numbers
            '0'..='9' | '-' => {
                if self.current_char_or_eof()? == '-' {
                    self.skip();
                }

                let end_idx = self.move_while(|c| c.is_ascii_digit());
                let number_str = &self.source.content[start_idx..end_idx];

                let number = match number_str.parse::<i64>() {
                    Ok(num) => num,
                    Err(err) => {
                        return Err(InvalidInteger {
                            source,
                            range: start_idx..end_idx,
                            value: number_str.to_string(),
                            reason: vec![err.into_diagnostic()],
                        }
                        .into());
                    }
                };

                Token {
                    kind: TokenKind::Integer(number),
                    index: start_idx..end_idx,
                }
            }

            // String literals
            '"' => {
                // Consume the opening quote
                self.consume()?;

                // Skip the quote character when slicing the string content.
                let slice_start_idx = self.position;

                loop {
                    match self.consume() {
                        // Once we found the closing quote, break the loop.
                        Ok('"') => break,

                        // If the input has reached EOF or there's a newline in the string,
                        // throw an error towards the user.
                        Ok('\n') | Err(_) => {
                            return Err(MissingEndingQuote {
                                source,
                                range: self.position..self.position + 1,
                            }
                            .into());
                        }
                        _ => {}
                    }
                }

                // Skip the quote character when slicing the string content.
                let slice_end_idx = self.position - 1;
                let content = &self.source.content[slice_start_idx..slice_end_idx];

                Token {
                    kind: TokenKind::String(content.to_string()),
                    index: start_idx..self.position,
                }
            }

            // Whitespace
            ' ' | '\t' | '\n' | '\r' => {
                self.eat_while(char::is_whitespace);

                return self.next();
            }
            char => {
                return Err(UnexpectedCharacter {
                    source: self.source.clone(),
                    range: self.position..self.position + 1,
                    char,
                }
                .into());
            }
        };

        // Advance the cursor position to the end of the token.
        self.position = token.end();

        Ok(token)
    }

    /// Checks whether the lexer has reached the end of the source.
    fn is_eof(&self) -> bool {
        self.position >= self.source.content.len()
    }

    /// Tries to get the character at the given cursor position.
    ///
    /// Returns `None` if the cursor is at the end of the source.
    fn at(&self, pos: usize) -> Option<char> {
        self.source.content.chars().nth(pos)
    }

    /// Tries to get the character at the current cursor position.
    ///
    /// Returns `None` if the cursor is at the end of the source.
    fn current_char(&self) -> Option<char> {
        self.at(self.position)
    }

    /// Gets the character at the current cursor position.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the cursor is at the end of the source.
    fn current_char_or_eof(&self) -> Result<char> {
        self.current_char().ok_or_else(|| {
            #[allow(clippy::range_plus_one, reason = "type only accepts `Range<usize>`")]
            UnexpectedEndOfFile {
                source: self.source.clone(),
                range: self.position..self.position + 1,
            }
            .into()
        })
    }

    /// Advances the cursor position of the lexer to the next line.
    fn skip(&mut self) {
        self.position += 1;
    }

    /// Gets the character at the current cursor position and advances the cursor position to the next position.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the cursor is at the end of the source.
    fn consume(&mut self) -> Result<char> {
        let c = self.current_char_or_eof()?;

        self.skip();

        Ok(c)
    }

    /// Gets characters while the predicate returns `true`.
    fn move_while(&self, predicate: impl Fn(char) -> bool) -> usize {
        let mut index = self.position;

        loop {
            match self.at(index) {
                Some(c) if predicate(c) => index += 1,
                _ => break,
            }
        }

        index
    }

    /// Consumes characters while the predicate returns `true`.
    fn eat_while(&mut self, predicate: impl Fn(char) -> bool) {
        self.position = self.move_while(predicate);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lexer(src: &'static str) -> Lexer {
        Lexer::new(Arc::new(SourceFile::internal(src)))
    }

    #[test]
    fn empty() {
        let mut lexer = lexer("");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Eof,
                index: 0..0
            }
        );
    }

    #[test]
    fn ident() {
        let mut lexer = lexer("identifier");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Identifier(String::from("identifier")),
                index: 0..10
            }
        );
    }

    #[test]
    fn idents() {
        let mut lexer = lexer("ident ident");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Identifier(String::from("ident")),
                index: 0..5
            }
        );

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Identifier(String::from("ident")),
                index: 6..11
            }
        );
    }

    #[test]
    fn ident_with_number() {
        let mut lexer = lexer("ident123");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Identifier(String::from("ident123")),
                index: 0..8
            }
        );
    }

    #[test]
    fn ident_with_underscore() {
        let mut lexer = lexer("ident_");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Identifier(String::from("ident_")),
                index: 0..6
            }
        );
    }

    #[test]
    fn ident_with_underscore_beginning() {
        let mut lexer = lexer("__ident");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Identifier(String::from("__ident")),
                index: 0..7
            }
        );
    }

    #[test]
    fn ident_with_uppercase() {
        let mut lexer = lexer("IDENT");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Identifier(String::from("IDENT")),
                index: 0..5
            }
        );
    }

    #[test]
    fn ident_with_leading_number() {
        let mut lexer = lexer("123ident");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Integer(123),
                index: 0..3
            }
        );

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Identifier(String::from("ident")),
                index: 3..8
            }
        );
    }

    #[test]
    fn ident_with_only_underscore() {
        let mut lexer = lexer("_");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Identifier(String::from("_")),
                index: 0..1
            }
        );
    }

    #[test]
    fn string() {
        let mut lexer = lexer("\"hello world\"");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::String(String::from("hello world")),
                index: 0..13
            }
        );
    }

    #[test]
    fn string_empty() {
        let mut lexer = lexer("\"\"");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::String(String::new()),
                index: 0..2
            }
        );
    }

    #[test]
    fn string_space() {
        let mut lexer = lexer("\"      \"");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::String(String::from("      ")),
                index: 0..8
            }
        );
    }

    #[test]
    fn string_newline() {
        let mut lexer = lexer("\"\n\"");

        assert_eq!(lexer.next().unwrap_err().to_string(), "expected ending quote");
    }

    #[test]
    fn string_missing_quote() {
        let mut lexer = lexer("\"hello world");

        assert_eq!(lexer.next().unwrap_err().to_string(), "expected ending quote");
    }

    #[test]
    fn digit() {
        let mut lexer = lexer("1");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Integer(1),
                index: 0..1
            }
        );
    }

    #[test]
    fn integer() {
        let mut lexer = lexer("123");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Integer(123),
                index: 0..3
            }
        );
    }

    #[test]
    fn integers() {
        let mut lexer = lexer("123 456");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Integer(123),
                index: 0..3
            }
        );

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Integer(456),
                index: 4..7
            }
        );
    }

    #[test]
    fn integer_negative() {
        let mut lexer = lexer("-123");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Integer(-123),
                index: 0..4
            }
        );
    }

    #[test]
    fn invalid_integer() {
        let mut lexer = lexer("123456789123456789123456789");

        assert_eq!(lexer.next().unwrap_err().to_string(), "invalid integer");
    }

    #[test]
    fn braces() {
        let mut lexer = lexer("[]");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::BraceLeft,
                index: 0..1
            }
        );

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::BraceRight,
                index: 1..2
            }
        );
    }

    #[test]
    fn curly() {
        let mut lexer = lexer("{}");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::CurlyLeft,
                index: 0..1
            }
        );

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::CurlyRight,
                index: 1..2
            }
        );
    }

    #[test]
    fn comma() {
        let mut lexer = lexer(",");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Comma,
                index: 0..1
            }
        );
    }

    #[test]
    fn equal() {
        let mut lexer = lexer("=");

        assert_eq!(
            lexer.next().unwrap(),
            Token {
                kind: TokenKind::Equal,
                index: 0..1
            }
        );
    }
}
