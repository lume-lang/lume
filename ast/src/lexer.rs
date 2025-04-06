use std::ops::Range;

use crate::parser::errors::*;
use diag::Result;
use diag::source::NamedSource;

const SYMBOLS: &[char] = &[
    '+', '-', '*', '/', '=', '!', '<', '>', '&', '|', '{', '}', '(', ')', '[', ']', ',', '.', ':',
];

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Add,
    AddAssign,
    And,
    Arrow,
    Assign,
    Break,
    Builtin,
    Class,
    Colon,
    Comma,
    Comment,
    Const,
    Continue,
    Decrement,
    Div,
    DivAssign,
    Dot,
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
    Import,
    In,
    Increment,
    Integer,
    LeftBracket,
    LeftCurly,
    LeftParen,
    Less,
    LessEqual,
    Let,
    Loop,
    Mul,
    MulAssign,
    NotEqual,
    Pipe,
    Pub,
    Return,
    RightBracket,
    RightCurly,
    RightParen,
    String,
    Sub,
    SubAssign,
    Trait,
    True,
    Type,
    Unless,
    While,
    Whitespace,
}

impl TokenKind {
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            TokenKind::Add
                | TokenKind::AddAssign
                | TokenKind::And
                | TokenKind::Arrow
                | TokenKind::Assign
                | TokenKind::Break
                | TokenKind::Class
                | TokenKind::Colon
                | TokenKind::Comma
                | TokenKind::Const
                | TokenKind::Continue
                | TokenKind::Decrement
                | TokenKind::Div
                | TokenKind::DivAssign
                | TokenKind::Dot
                | TokenKind::Else
                | TokenKind::Equal
                | TokenKind::Exclamation
                | TokenKind::External
                | TokenKind::False
                | TokenKind::Fn
                | TokenKind::For
                | TokenKind::Greater
                | TokenKind::GreaterEqual
                | TokenKind::If
                | TokenKind::Import
                | TokenKind::In
                | TokenKind::Increment
                | TokenKind::LeftBracket
                | TokenKind::LeftCurly
                | TokenKind::LeftParen
                | TokenKind::Less
                | TokenKind::LessEqual
                | TokenKind::Loop
                | TokenKind::Mul
                | TokenKind::MulAssign
                | TokenKind::NotEqual
                | TokenKind::Pipe
                | TokenKind::Pub
                | TokenKind::Return
                | TokenKind::RightBracket
                | TokenKind::RightCurly
                | TokenKind::RightParen
                | TokenKind::Sub
                | TokenKind::SubAssign
                | TokenKind::Trait
                | TokenKind::True
                | TokenKind::Type
                | TokenKind::Unless
                | TokenKind::While
                | TokenKind::Whitespace
        )
    }

    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            TokenKind::Integer | TokenKind::Float | TokenKind::String | TokenKind::False | TokenKind::True
        )
    }

    pub fn is_unary(&self) -> bool {
        matches!(self, TokenKind::Exclamation | TokenKind::Sub)
    }

    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::Add
                | TokenKind::AddAssign
                | TokenKind::And
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
                | TokenKind::Sub
                | TokenKind::SubAssign
        )
    }
}

impl Into<&'static str> for TokenKind {
    fn into(self) -> &'static str {
        match self {
            TokenKind::Add => "+",
            TokenKind::AddAssign => "+=",
            TokenKind::And => "&",
            TokenKind::Arrow => "->",
            TokenKind::Assign => "=",
            TokenKind::Break => "break",
            TokenKind::Builtin => "builtin",
            TokenKind::Class => "class",
            TokenKind::Colon => ":",
            TokenKind::Comma => ",",
            TokenKind::Comment => "comment",
            TokenKind::Const => "const",
            TokenKind::Continue => "continue",
            TokenKind::Decrement => "--",
            TokenKind::Div => "/",
            TokenKind::DivAssign => "/=",
            TokenKind::Dot => ".",
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
            TokenKind::NotEqual => "!=",
            TokenKind::Integer => "integer",
            TokenKind::Pipe => "|",
            TokenKind::Pub => "pub",
            TokenKind::Return => "return",
            TokenKind::RightBracket => "]",
            TokenKind::RightCurly => "}",
            TokenKind::RightParen => ")",
            TokenKind::String => "string",
            TokenKind::Sub => "-",
            TokenKind::SubAssign => "-=",
            TokenKind::Trait => "trait",
            TokenKind::True => "true",
            TokenKind::Type => "type",
            TokenKind::Unless => "unless",
            TokenKind::While => "while",
            TokenKind::Whitespace => "whitespace",
        }
    }
}

impl Into<String> for TokenKind {
    fn into(self) -> String {
        <TokenKind as Into<&'static str>>::into(self).to_string()
    }
}

impl std::fmt::Debug for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let desc: &'static str = (*self).into();

        write!(f, "{}", desc)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    /// Defines the type of the token.
    pub kind: TokenKind,

    /// Defines the start and end position of the token.
    pub index: Range<usize>,

    /// Defines the value of the token.
    pub value: Option<String>,
}

impl Token {
    pub fn new(kind: TokenKind, value: String) -> Self {
        Token {
            kind,
            index: 0..0,
            value: Some(value),
        }
    }

    pub fn empty(kind: TokenKind) -> Self {
        Token {
            kind,
            index: 0..0,
            value: None,
        }
    }

    pub fn len(&self) -> usize {
        self.index.end - self.index.start
    }

    pub fn start(&self) -> usize {
        self.index.start
    }

    pub fn end(&self) -> usize {
        self.index.end
    }
}

impl Into<&'static str> for Token {
    fn into(self) -> &'static str {
        self.kind.into()
    }
}

impl Into<String> for Token {
    fn into(self) -> String {
        self.kind.into()
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)?;

        if !self.kind.is_keyword() {
            if let Some(value) = &self.value {
                write!(f, " ({})", value)?;
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct Lexer {
    /// Declares the source to lex tokens from.
    pub source: NamedSource,

    /// Defines the current position in the source.
    position: usize,
}

impl Lexer {
    /// Creates a new lexer instance.
    pub fn new(source: NamedSource) -> Self {
        Lexer { source, position: 0 }
    }

    pub fn is_eof(&self) -> bool {
        self.position >= self.source.content.len()
    }

    /// Tries to get the character at the current cursor position.
    ///
    /// Returns `None` if the cursor is at the end of the source.
    fn current_char(&self) -> Option<char> {
        self.source.content.chars().nth(self.position)
    }

    /// Tries to get the character at the current cursor position.
    ///
    /// Returns `\0` if the cursor is at the end of the source.
    fn current_char_or_eof(&self) -> char {
        self.source.content.chars().nth(self.position).unwrap_or('\0')
    }

    /// Tries to get the character which is at the current cursor position, offset by `offset`.
    ///
    /// Returns `\0` if the cursor is at the end of the source.
    fn at_offset(&self, offset: usize) -> char {
        self.source.content.chars().nth(self.position + offset).unwrap_or('\0')
    }

    /// Advances the cursor position of the lexer to the next line.
    fn next(&mut self) {
        self.position += 1;
    }

    /// Gets the character at the current cursor position and advances the cursor position to the next position.
    fn consume(&mut self) -> char {
        let c = self.current_char_or_eof();

        self.next();
        c
    }

    /// Gets characters while the predicate returns `true`.
    fn take_while(&mut self, predicate: impl Fn(char) -> bool) -> String {
        let mut result = String::new();

        loop {
            let c = self.current_char();
            if c.is_none() || !predicate(c.unwrap()) {
                break;
            }

            result.push(self.consume());
        }

        result
    }

    /// Skips characters while the predicate returns `true`.
    fn skip_while(&mut self, predicate: impl Fn(char) -> bool) {
        loop {
            let c = self.current_char();
            if c.is_none() || !predicate(c.unwrap()) {
                break;
            }

            self.next();
        }
    }

    /// Gets the token at the current cursor position. The cursor is advanced to the start of the next token.
    pub fn next_token(&mut self) -> Result<Token> {
        let start_idx = self.position;
        let first_char = match self.current_char_or_eof() {
            '\0' => return Ok(Token::empty(TokenKind::Eof)),
            c => c,
        };

        let mut token = match first_char {
            // Block comments
            '#' => self.comment(),

            // Identifiers, such as keywords and standalone words.
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),

            // Symbols, such as operators and punctuation.
            c if SYMBOLS.contains(&c) => self.symbol()?,

            // Numbers
            '0'..='9' => self.number(),

            // String literals
            '"' => self.string()?,

            // Whitespace
            ' ' | '\t' | '\n' | '\r' => self.whitespace(),
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
        token.index = start_idx..end_idx;

        Ok(token)
    }

    /// Parses a comment token at the current cursor position.
    fn comment(&mut self) -> Token {
        let content = self.take_while(|c| c != '\n');

        Token::new(TokenKind::Comment, content)
    }

    /// Parses an identifier token at the current cursor position.
    fn identifier(&mut self) -> Token {
        let content = self.take_while(|c| c.is_ascii_alphanumeric() || c == '_');

        match content.as_str() {
            "break" => Token::empty(TokenKind::Break),
            "builtin" => Token::empty(TokenKind::Builtin),
            "class" => Token::empty(TokenKind::Class),
            "const" => Token::empty(TokenKind::Const),
            "continue" => Token::empty(TokenKind::Continue),
            "else" => Token::empty(TokenKind::Else),
            "enum" => Token::empty(TokenKind::Enum),
            "external" => Token::empty(TokenKind::External),
            "fn" => Token::empty(TokenKind::Fn),
            "for" => Token::empty(TokenKind::For),
            "if" => Token::empty(TokenKind::If),
            "import" => Token::empty(TokenKind::Import),
            "in" => Token::empty(TokenKind::In),
            "let" => Token::empty(TokenKind::Let),
            "loop" => Token::empty(TokenKind::Loop),
            "type" => Token::empty(TokenKind::Type),
            "pub" => Token::empty(TokenKind::Pub),
            "return" => Token::empty(TokenKind::Return),
            "trait" => Token::empty(TokenKind::Trait),
            "true" => Token::empty(TokenKind::True),
            "false" => Token::empty(TokenKind::False),
            "unless" => Token::empty(TokenKind::Unless),
            "while" => Token::empty(TokenKind::While),
            _ => Token::new(TokenKind::Identifier, content),
        }
    }

    /// Parses a symbol token at the current cursor position.
    fn symbol(&mut self) -> Result<Token> {
        let slice = self
            .source
            .content
            .chars()
            .skip(self.position)
            .take(2)
            .collect::<String>();

        let chars: Vec<char> = slice.chars().collect();
        let (kind, len) = match self.symbol_value(&chars) {
            Ok((kind, len)) => (kind, len),
            Err(err) => return Err(err),
        };

        let symbol: String = chars[..len].into_iter().collect();

        for _ in 0..len {
            self.next();
        }

        Ok(Token::new(kind, symbol))
    }

    fn symbol_value(&mut self, chars: &Vec<char>) -> Result<(TokenKind, usize)> {
        if chars.len() >= 2 {
            match (chars[0], chars[1]) {
                ('+', '=') => return Ok((TokenKind::AddAssign, 2)),
                ('-', '>') => return Ok((TokenKind::Arrow, 2)),
                ('=', '=') => return Ok((TokenKind::Equal, 2)),
                ('/', '=') => return Ok((TokenKind::DivAssign, 2)),
                ('>', '=') => return Ok((TokenKind::GreaterEqual, 2)),
                ('<', '=') => return Ok((TokenKind::LessEqual, 2)),
                ('*', '=') => return Ok((TokenKind::MulAssign, 2)),
                ('!', '=') => return Ok((TokenKind::NotEqual, 2)),
                ('-', '=') => return Ok((TokenKind::SubAssign, 2)),
                _ => {}
            }
        }

        if chars.len() >= 1 {
            match chars[0] {
                '+' => return Ok((TokenKind::Add, 1)),
                '&' => return Ok((TokenKind::And, 1)),
                '=' => return Ok((TokenKind::Assign, 1)),
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
                '|' => return Ok((TokenKind::Pipe, 1)),
                ']' => return Ok((TokenKind::RightBracket, 1)),
                '}' => return Ok((TokenKind::RightCurly, 1)),
                ')' => return Ok((TokenKind::RightParen, 1)),
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
    fn number(&mut self) -> Token {
        let mut token_kind = TokenKind::Integer;
        let start_index = self.position;

        // Read the radix prefix, if any.
        let radix = self.parse_radix_prefix();

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

        Token::new(token_kind, number_str)
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
                '0'..='9' | '_' => self.consume_digits(radix),

                // Scientific notation - not a radix prefix
                '.' | 'e' | 'E' => {}

                // Otherwise, it's just a zero.
                _ => {}
            }
        } else {
            self.consume_digits(radix);
        }

        radix
    }

    fn consume_digits(&mut self, radix: u32) {
        self.skip_while(|c| c.is_digit(radix) || c == '_');
    }

    fn consume_float_exponent(&mut self) {
        if matches!(self.current_char_or_eof(), '-' | '+') {
            self.next();
        }

        self.consume_digits(10);
    }

    /// Parses a string token at the current cursor position.
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
            };

            content.push(c);
        }

        Ok(Token::new(TokenKind::String, content))
    }

    /// Parses a single whitespace token at the current cursor position.
    fn whitespace(&mut self) -> Token {
        let whitespace = self.take_while(|c| c.is_whitespace());

        Token::new(TokenKind::Whitespace, whitespace)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lexer(source: &str) -> Lexer {
        let source = NamedSource {
            name: "<empty>".into(),
            content: source.to_owned(),
        };

        Lexer::new(source.clone())
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

            if token.kind.is_keyword() && $value.is_some() {
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
                assert_eq!(token.index, exp.index);

                if token.kind.is_keyword() && exp.value.is_some() {
                    let val: String = exp.value.unwrap().to_string();
                    assert_eq!(token.value, Some(val.to_string()));
                }
            }
        };
    }

    macro_rules! set_snapshot_suffix {
        ($($expr:expr),*) => {
            let mut settings = insta::Settings::clone_current();
            settings.set_snapshot_suffix(format!($($expr,)*));
            let _guard = settings.bind_to_scope();
        }
    }

    macro_rules! assert_snap_eq {
        (
            $input: expr,
            $($expr:expr),+
        ) => {
            set_snapshot_suffix!( $($expr),+ );

            insta::assert_debug_snapshot!(lex_all($input));
        };
    }

    #[test]
    fn test_keywords_map() {
        assert_token!("break", TokenKind::Break, None::<String>, 0, 5);
        assert_token!("const", TokenKind::Const, None::<String>, 0, 5);
        assert_token!("continue", TokenKind::Continue, None::<String>, 0, 8);
        assert_token!("external", TokenKind::External, None::<String>, 0, 8);
        assert_token!("for", TokenKind::For, None::<String>, 0, 3);
        assert_token!("fn", TokenKind::Fn, None::<String>, 0, 2);
        assert_token!("if", TokenKind::If, None::<String>, 0, 2);
        assert_token!("import", TokenKind::Import, None::<String>, 0, 6);
        assert_token!("in", TokenKind::In, None::<String>, 0, 2);
        assert_token!("loop", TokenKind::Loop, None::<String>, 0, 4);
        assert_token!("type", TokenKind::Type, None::<String>, 0, 4);
        assert_token!("return", TokenKind::Return, None::<String>, 0, 6);
        assert_token!("true", TokenKind::True, None::<String>, 0, 4);
        assert_token!("false", TokenKind::False, None::<String>, 0, 5);
        assert_token!("unless", TokenKind::Unless, None::<String>, 0, 6);
        assert_token!("while", TokenKind::While, None::<String>, 0, 5);
    }

    #[test]
    fn test_symbols_map() {
        assert_token!("+=", TokenKind::AddAssign, Some("+="), 0, 2);
        assert_token!("->", TokenKind::Arrow, Some("->"), 0, 2);
        assert_token!("==", TokenKind::Equal, Some("=="), 0, 2);
        assert_token!("/=", TokenKind::DivAssign, Some("/="), 0, 2);
        assert_token!(">=", TokenKind::GreaterEqual, Some(">="), 0, 2);
        assert_token!("<=", TokenKind::LessEqual, Some("<="), 0, 2);
        assert_token!("*=", TokenKind::MulAssign, Some("*="), 0, 2);
        assert_token!("!=", TokenKind::NotEqual, Some("!="), 0, 2);
        assert_token!("-=", TokenKind::SubAssign, Some("-="), 0, 2);

        assert_token!("&", TokenKind::And, Some("&"), 0, 1);
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
        assert_token!("|", TokenKind::Pipe, Some("|"), 0, 1);
        assert_token!("]", TokenKind::RightBracket, Some("]"), 0, 1);
        assert_token!("}", TokenKind::RightCurly, Some("}"), 0, 1);
        assert_token!(")", TokenKind::RightParen, Some(")"), 0, 1);
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
        assert_token!("#", TokenKind::Comment, Some("#"), 0, 1);
        assert_token!("# testing", TokenKind::Comment, Some("# testing"), 0, 9);

        assert_tokens!(
            "# comment 1\n# comment 2",
            token(TokenKind::Comment, Some("# comment 1".into()), 0, 11),
            token(TokenKind::Whitespace, Some("\n".into()), 11, 12),
            token(TokenKind::Comment, Some("# comment 2".into()), 12, 23)
        );

        assert_tokens!(
            "# comment 1  ",
            token(TokenKind::Comment, Some("# comment 1  ".into()), 0, 13)
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
        assert_token!("1", TokenKind::Integer, Some("1"), 0, 1);
        assert_token!("10", TokenKind::Integer, Some("10"), 0, 2);
        assert_token!("0b01", TokenKind::Integer, Some("0b01"), 0, 4);
        assert_token!("0B01", TokenKind::Integer, Some("0B01"), 0, 4);
        assert_token!("0x01", TokenKind::Integer, Some("0x01"), 0, 4);
        assert_token!("0X01", TokenKind::Integer, Some("0X01"), 0, 4);
        assert_token!("0o01", TokenKind::Integer, Some("0o01"), 0, 4);
        assert_token!("0O01", TokenKind::Integer, Some("0O01"), 0, 4);
        assert_token!("0_0", TokenKind::Integer, Some("0_0"), 0, 3);
        assert_token!("0_000", TokenKind::Integer, Some("0_000"), 0, 5);
        assert_token!("10.0", TokenKind::Float, Some("10.0"), 0, 4);
        assert_token!("10.123", TokenKind::Float, Some("10.123"), 0, 6);

        assert_tokens!(
            "-1",
            token(TokenKind::Sub, None, 0, 1),
            token(TokenKind::Integer, Some("1".into()), 1, 2)
        );

        assert_tokens!("10e5", token(TokenKind::Float, Some("10e5".into()), 0, 4));
        assert_tokens!("10E5", token(TokenKind::Float, Some("10E5".into()), 0, 4));
        assert_tokens!("1.0e5", token(TokenKind::Float, Some("1.0e5".into()), 0, 5));
        assert_tokens!("1.0E5", token(TokenKind::Float, Some("1.0E5".into()), 0, 5));
    }

    #[test]
    fn test_string() {
        assert_token!("\"\"", TokenKind::String, Some(""), 0, 2);
        assert_token!("\"    \"", TokenKind::String, Some("    "), 0, 6);
        assert_token!("\"hello world\"", TokenKind::String, Some("hello world"), 0, 13);
        assert_token!("\"hello\nworld\"", TokenKind::String, Some("hello\nworld"), 0, 13);

        assert_snap_eq!("\"hello world", "unended_string");
    }
}
