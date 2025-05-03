use std::sync::Arc;

use lume_ast::*;
use lume_diag::Error;
use lume_diag::Result;
use lume_span::SourceFile;
use lume_span::SourceFileId;

use crate::lexer::*;
use crate::parser::errors::*;

pub mod errors;

const IDENTIFIER_SEPARATOR: TokenKind = TokenKind::PathSeparator;

const OPERATOR_PRECEDENCE: &[(TokenKind, u8)] = &[
    (TokenKind::Assign, 1),
    (TokenKind::AddAssign, 1),
    (TokenKind::SubAssign, 1),
    (TokenKind::MulAssign, 1),
    (TokenKind::DivAssign, 1),
    (TokenKind::Equal, 3),
    (TokenKind::NotEqual, 3),
    (TokenKind::Greater, 4),
    (TokenKind::Less, 4),
    (TokenKind::GreaterEqual, 4),
    (TokenKind::LessEqual, 4),
    (TokenKind::Add, 5),
    (TokenKind::Sub, 5),
    (TokenKind::Mul, 6),
    (TokenKind::Div, 6),
    (TokenKind::Increment, 7),
    (TokenKind::Decrement, 7),
    (TokenKind::Dot, 9),
];

/// Defines the precedence for unary operators, such as `-` or `!`.
///
/// They cannot be defined within [`OPERATOR_PRECEDENCE`], as unary operators
/// share the same operators as other operators, but have different meanings.
///
/// For example, `-` can mean both "minus some amount", but when it's within it's own
/// expression before a number expression, it'd mean "the negative of the following expression".
const UNARY_PRECEDENCE: u8 = 3;

impl Token {
    /// Gets the precedence of the token kind.
    ///
    /// Returns the precedence of the token kind, or 0 if the token kind is not an operator.
    pub fn precedence(&self) -> u8 {
        OPERATOR_PRECEDENCE
            .iter()
            .find(|(k, _)| k == &self.kind)
            .map_or(0, |(_, p)| *p)
    }
}

pub struct Parser {
    /// Defines the source code which is being parsed.
    source: Arc<SourceFile>,

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
    pub fn new(source: Arc<SourceFile>) -> Self {
        let lexer = Lexer::new(source.clone());

        Parser {
            source,
            lexer,
            index: 0,
            position: 0,
            tokens: Vec::new(),
            doc_token: None,
        }
    }

    /// Parses the given source.
    ///
    /// This function iterates through the tokens of the module source code,
    /// parsing each top-level expression and collecting them into a vector.
    pub fn parse_str(str: &str) -> Result<Vec<TopLevelExpression>> {
        let source = SourceFile::internal(str);
        let mut parser = Parser::new(Arc::new(source));

        parser.parse()
    }

    /// Parses the given source text.
    ///
    /// This function iterates through the tokens of the module source code,
    /// parsing each top-level expression and collecting them into a vector.
    pub fn parse_src(state: &lume_state::State, file: SourceFileId) -> Result<Vec<TopLevelExpression>> {
        let src = state.source_of(file)?;
        let mut parser = Parser::new(src);

        parser.parse()
    }

    /// Parses the module within the parser state.
    ///
    /// This function iterates through the tokens of the module source code,
    /// parsing each top-level expression and collecting them into a vector.
    pub fn parse(&mut self) -> Result<Vec<TopLevelExpression>> {
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

    /// Parses the last token from the lexer.
    ///
    /// Returns the parsed token or a parsing error.
    fn last_token(&self) -> Result<Token> {
        self.token_at(self.tokens.len() - 1)
    }

    /// Parses the previous token from the lexer.
    ///
    /// Returns the parsed token or a parsing error.
    fn previous_token(&self) -> Result<Token> {
        self.token_at(self.tokens.len() - 2)
    }

    /// Peeks the next token from the lexer at some offset and returns it if it matches the expected kind.
    ///
    /// Returns a boolean indicating whether the token matches the expected kind.
    fn peek_next(&self, kind: TokenKind, offset: usize) -> Result<bool> {
        let token = match self.token_at(self.index + offset) {
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
    fn peek(&self, kind: TokenKind) -> Result<bool> {
        self.peek_next(kind, 0)
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

    /// Checks whether the current token is an `Builtin` token.
    fn check_builtin(&mut self) -> bool {
        self.check(TokenKind::Builtin)
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
        let mut segments = Vec::new();

        loop {
            segments.push(self.parse_identifier()?);

            if self.consume_if(IDENTIFIER_SEPARATOR)?.is_none() {
                break;
            }
        }

        let start = segments.first().unwrap().location.0.start;
        let end = segments.last().unwrap().location.0.end;

        let path = NamespacePath {
            path: segments,
            location: (start..end).into(),
        };

        Ok(path)
    }

    /// Returns a block for functions or methods.
    fn parse_block(&mut self) -> Result<Block> {
        let (statements, location) = self.consume_with_loc(|p| p.consume_curly_seq(|p| p.parse_statement()))?;

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

    fn parse_top_level_expression(&mut self) -> Result<TopLevelExpression> {
        let current = self.token()?;

        match current.kind {
            TokenKind::Import => self.parse_import(),
            TokenKind::Namespace => self.parse_namespace(),
            TokenKind::Fn | TokenKind::Pub => self.parse_fn(),
            TokenKind::Class => self.parse_class(),
            TokenKind::Trait => self.parse_trait(),
            TokenKind::Enum => self.parse_enum(),
            TokenKind::Type => self.parse_type_alias(),
            TokenKind::Use => self.parse_use(),
            _ => Err(err!(self, InvalidTopLevelStatement, actual, current.kind)),
        }
    }

    fn parse_import(&mut self) -> Result<TopLevelExpression> {
        let start = self.consume(TokenKind::Import)?.start();
        let path = self.parse_namespace_path()?;

        if self.eof() {
            return Err(err!(self, InvalidImportPath, found, TokenKind::Eof));
        }

        let names = match self.token()?.kind {
            TokenKind::LeftParen => self.consume_paren_seq(|p| p.parse_identifier())?,
            kind => return Err(err!(self, InvalidImportPath, found, kind)),
        };

        let end = self.token_at(self.index - 1)?.end();

        let import_def = Import {
            path,
            names,
            location: (start..end).into(),
        };

        Ok(TopLevelExpression::Import(Box::new(import_def)))
    }

    fn parse_namespace(&mut self) -> Result<TopLevelExpression> {
        let (path, location) = self.consume_with_loc(|p| {
            p.consume(TokenKind::Namespace)?;

            p.parse_namespace_path()
        })?;

        Ok(TopLevelExpression::Namespace(Box::new(Namespace { path, location })))
    }

    fn parse_fn(&mut self) -> Result<TopLevelExpression> {
        let start = self.token()?.start();
        let visibility = self.parse_visibility()?;

        self.expect_fn()?;

        let external = self.check_external();
        let name = self.parse_ident_or_err(err!(self, ExpectedFunctionName))?;
        let type_parameters = self.parse_type_parameters()?;
        let parameters = self.parse_fn_params()?;
        let return_type = self.parse_fn_return_type()?;
        let block = self.parse_opt_external_block(external)?;

        let end = self.last_token()?.end();

        let function_def = FunctionDefinition {
            visibility,
            external,
            name,
            parameters,
            type_parameters,
            return_type,
            block,
            location: (start..end).into(),
            documentation: self.doc_token.take(),
        };

        Ok(TopLevelExpression::FunctionDefinition(Box::new(function_def)))
    }

    fn parse_fn_params(&mut self) -> Result<Vec<Parameter>> {
        // If no opening parenthesis, no parameters are defined.
        if !self.peek(TokenKind::LeftParen)? {
            return Ok(Vec::new());
        }

        self.consume_paren_seq(|p| p.parse_fn_param())
    }

    fn parse_fn_param(&mut self) -> Result<Parameter> {
        if let Some(token) = self.consume_if(TokenKind::SelfRef)? {
            let location = token.index;

            return Ok(Parameter {
                name: Identifier {
                    name: "self".into(),
                    location: location.clone().into(),
                },
                param_type: Type::SelfType(Box::new(SelfType {
                    location: location.clone().into(),
                })),
                location: location.into(),
            });
        }

        let name = self.parse_identifier()?;

        self.consume(TokenKind::Colon)?;

        let param_type = self.parse_type()?;

        let start = name.location().start();
        let end = param_type.location().end();

        Ok(Parameter {
            name,
            param_type,
            location: (start..end).into(),
        })
    }

    /// Parses the return type of the current function definition.
    fn parse_fn_return_type(&mut self) -> Result<Option<Box<Type>>> {
        if self.consume_if(TokenKind::Arrow)?.is_none() {
            return Ok(None);
        }

        Ok(Some(Box::new(self.parse_type()?)))
    }

    fn parse_class(&mut self) -> Result<TopLevelExpression> {
        let start = self.consume(TokenKind::Class)?.start();

        let builtin = self.check_builtin();
        let name = self.parse_ident_or_err(err!(self, ExpectedClassName))?;
        let type_parameters = self.parse_type_parameters()?;
        let members = self.consume_curly_seq(|p| p.parse_class_member())?;

        let end = self.last_token()?.end();

        let class_def = ClassDefinition {
            name,
            builtin,
            members,
            type_parameters,
            location: (start..end).into(),
            documentation: self.doc_token.take(),
        };

        Ok(TopLevelExpression::TypeDefinition(Box::new(TypeDefinition::Class(
            Box::new(class_def),
        ))))
    }

    fn parse_class_member(&mut self) -> Result<ClassMember> {
        self.read_doc_comment()?;

        let visibility = self.parse_visibility()?;

        match self.token()?.kind {
            TokenKind::Let => self.parse_property(visibility),
            TokenKind::Fn => self.parse_method(visibility),
            _ => Err(err!(self, ExpectedClassMember)),
        }
    }

    fn parse_visibility(&mut self) -> Result<Visibility> {
        match self.token()?.kind {
            // If the member is marked as public, make it so.
            TokenKind::Pub => {
                let location = self.consume(TokenKind::Pub)?.index;

                Ok(Visibility::Public(Box::new(Public {
                    location: location.into(),
                })))
            }

            // By default, make it private.
            _ => {
                let location = self.token()?.index;

                Ok(Visibility::Private(Box::new(Private {
                    location: location.into(),
                })))
            }
        }
    }

    fn parse_property(&mut self, visibility: Visibility) -> Result<ClassMember> {
        let start = self.consume(TokenKind::Let)?.start();

        let name = match self.parse_identifier() {
            Ok(name) => name,
            Err(_) => return Err(err!(self, ExpectedClassMember)),
        };

        self.consume(TokenKind::Colon)?;

        let property_type = self.parse_type()?;
        let default_value = self.parse_opt_assignment()?;

        let end = self.expect_semi()?.end();

        let property_def = Property {
            visibility,
            name,
            property_type,
            default_value,
            location: (start..end).into(),
            documentation: self.doc_token.take(),
        };

        Ok(ClassMember::Property(Box::new(property_def)))
    }

    fn parse_method(&mut self, visibility: Visibility) -> Result<ClassMember> {
        let start = self.expect_fn()?.start();
        let external = self.check_external();

        let name = self.parse_method_name()?;
        let type_parameters = self.parse_type_parameters()?;
        let parameters = self.parse_fn_params()?;
        let return_type = self.parse_fn_return_type()?;
        let block = self.parse_opt_external_block(external)?;

        let end = self.token_at(self.index - 1)?.end();

        let method_def = MethodDefinition {
            visibility,
            external,
            name,
            parameters,
            type_parameters,
            return_type,
            block,
            location: (start..end).into(),
            documentation: self.doc_token.take(),
        };

        Ok(ClassMember::MethodDefinition(Box::new(method_def)))
    }

    fn parse_method_name(&mut self) -> Result<Identifier> {
        match self.consume_any()? {
            // Allow actual identifiers.
            t if t.kind == TokenKind::Identifier => Ok(t.into()),

            // As well as operator tokens, so we can do operator overloading.
            t if t.kind.is_operator() => Ok(t.into()),

            _ => Err(err!(self, ExpectedFunctionName)),
        }
    }

    fn parse_trait(&mut self) -> Result<TopLevelExpression> {
        let start = self.consume(TokenKind::Trait)?.start();

        let name = self.parse_ident_or_err(err!(self, ExpectedTraitName))?;
        let type_parameters = self.parse_type_parameters()?;
        let methods = self.consume_curly_seq(|p| p.parse_trait_method())?;

        let end = self.last_token()?.end();

        let trait_def = TraitDefinition {
            name,
            methods,
            type_parameters,
            location: (start..end).into(),
            documentation: self.doc_token.take(),
        };

        Ok(TopLevelExpression::TypeDefinition(Box::new(TypeDefinition::Trait(
            Box::new(trait_def),
        ))))
    }

    fn parse_trait_method(&mut self) -> Result<TraitMethodDefinition> {
        self.read_doc_comment()?;

        let visibility = self.parse_visibility()?;
        let start = self.expect_fn()?.start();

        let name = match self.parse_identifier() {
            Ok(name) => name,
            Err(_) => return Err(err!(self, ExpectedFunctionName)),
        };

        let type_parameters = self.parse_type_parameters()?;
        let parameters = self.parse_fn_params()?;
        let return_type = self.parse_fn_return_type()?;
        let block = self.parse_opt_block()?;

        let end = self.previous_token()?.end();

        Ok(TraitMethodDefinition {
            visibility,
            name,
            parameters,
            type_parameters,
            return_type,
            block,
            location: (start..end).into(),
            documentation: self.doc_token.take(),
        })
    }

    /// Parses a single enum type definition, such as:
    ///
    /// ```ignore
    /// enum IpAddrKind {
    ///   V4,
    ///   V6,
    /// }
    /// ```
    fn parse_enum(&mut self) -> Result<TopLevelExpression> {
        let start = self.consume(TokenKind::Enum)?.start();

        let name = self.parse_identifier()?;
        let cases = self.consume_comma_seq(TokenKind::LeftCurly, TokenKind::RightCurly, |p| p.parse_enum_case())?;

        let end = self.last_token()?.end();

        Ok(TopLevelExpression::TypeDefinition(Box::new(TypeDefinition::Enum(
            Box::new(EnumDefinition {
                name,
                cases,
                location: (start..end).into(),
                documentation: self.doc_token.take(),
            }),
        ))))
    }

    /// Parses a single enum type case, such as `V4` or `V4(String)`.
    fn parse_enum_case(&mut self) -> Result<EnumDefinitionCase> {
        self.read_doc_comment()?;

        let name = self.parse_identifier()?;

        let parameters = if self.peek(TokenKind::LeftParen)? {
            self.consume_comma_seq(TokenKind::LeftParen, TokenKind::RightParen, |p| {
                Ok(Box::new(p.parse_type()?))
            })?
        } else {
            Vec::new()
        };

        let start = name.location.start();
        let end = self.token_at(self.index - 1)?.end();

        Ok(EnumDefinitionCase {
            name,
            parameters,
            location: (start..end).into(),
            documentation: self.doc_token.take(),
        })
    }

    /// Parses a single type alias definition, such as:
    ///
    /// ```ignore
    /// type Alias = String | Int
    /// ```
    fn parse_type_alias(&mut self) -> Result<TopLevelExpression> {
        let start = self.consume(TokenKind::Type)?.start();
        let name = self.parse_identifier()?;

        // Skip equal sign between name and type
        self.consume(TokenKind::Assign)?;

        let definition = self.parse_type()?;
        let end = definition.location().end();

        let alias = AliasDefinition {
            name,
            definition: Box::new(definition),
            location: (start..end).into(),
            documentation: self.doc_token.take(),
        };

        Ok(TopLevelExpression::TypeDefinition(Box::new(TypeDefinition::Alias(
            Box::new(alias),
        ))))
    }

    /// Parses zero-or-more type parameters.
    fn parse_type_parameters(&mut self) -> Result<Vec<TypeParameter>> {
        if !self.peek(TokenKind::Less)? {
            return Ok(Vec::new());
        }

        self.consume_comma_seq(TokenKind::Less, TokenKind::Greater, |p| {
            let name = p.parse_identifier()?;
            let mut constraints = Vec::new();

            if p.consume_if(TokenKind::Colon)?.is_some() {
                constraints.push(Box::new(p.parse_type()?));

                while p.consume_if(TokenKind::Add)?.is_some() {
                    constraints.push(Box::new(p.parse_type()?));
                }
            }

            Ok(TypeParameter { name, constraints })
        })
    }

    fn parse_use(&mut self) -> Result<TopLevelExpression> {
        let start = self.consume(TokenKind::Use)?.start();
        let name = self.parse_type()?;

        self.consume(TokenKind::In)?;
        let target = self.parse_type()?;

        let methods = self.consume_curly_seq(|p| p.parse_use_impl())?;
        let end = self.last_token()?.end();

        let use_trait = UseTrait {
            name: Box::new(name),
            target: Box::new(target),
            methods,
            location: (start..end).into(),
        };

        Ok(TopLevelExpression::Use(Box::new(use_trait)))
    }

    fn parse_use_impl(&mut self) -> Result<TraitMethodImplementation> {
        let visibility = self.parse_visibility()?;
        self.expect_fn()?;

        let start = visibility.location().start();

        let name = match self.parse_identifier() {
            Ok(name) => name,
            Err(_) => return Err(err!(self, ExpectedFunctionName)),
        };

        let type_parameters = self.parse_type_parameters()?;
        let parameters = self.parse_fn_params()?;
        let return_type = self.parse_fn_return_type()?;
        let block = self.parse_block()?;

        let end = block.location.end();

        Ok(TraitMethodImplementation {
            visibility,
            name,
            parameters,
            type_parameters,
            return_type,
            block,
            location: (start..end).into(),
        })
    }

    /// Parses some abstract type at the current cursor position.
    fn parse_type(&mut self) -> Result<Type> {
        let token = self.token()?;

        match token.kind {
            TokenKind::Identifier => self.parse_scalar_or_generic_type(),
            TokenKind::LeftBracket => self.parse_array_type(),
            _ => Err(err!(self, UnexpectedType, actual, token.kind)),
        }
    }

    /// Parses either a scalar- or generic-type at the current cursor position.
    fn parse_scalar_or_generic_type(&mut self) -> Result<Type> {
        let name = self.parse_identifier()?;

        if self.peek(TokenKind::Less)? {
            let identifier = Identifier {
                name: name.name,
                location: name.location,
            };

            self.parse_generic_type_arguments(identifier)
        } else {
            let location = name.location;

            Ok(Type::Scalar(Box::new(ScalarType {
                name: name.name,
                location,
            })))
        }
    }

    /// Parses an array type at the current cursor position.
    fn parse_array_type(&mut self) -> Result<Type> {
        let start = self.consume(TokenKind::LeftBracket)?.start();

        let element_type = Box::new(self.parse_type()?);

        let end = self.consume(TokenKind::RightBracket)?.end();

        let array_type = ArrayType {
            element_type,
            location: (start..end).into(),
        };

        Ok(Type::Array(Box::new(array_type)))
    }

    /// Parses a generic type, with zero-or-more type parameters at the current cursor position.
    fn parse_generic_type_arguments(&mut self, name: Identifier) -> Result<Type> {
        let (type_params, location) = self.consume_with_loc(|p| {
            p.consume_comma_seq(TokenKind::Less, TokenKind::Greater, |p| Ok(Box::new(p.parse_type()?)))
        })?;

        Ok(Type::Generic(Box::new(GenericType {
            name,
            type_params,
            location,
        })))
    }

    /// Parses some abstract type at the current cursor position.
    fn parse_opt_type(&mut self) -> Result<Option<Type>> {
        if self.consume_if(TokenKind::Colon)?.is_none() {
            Ok(None)
        } else {
            Ok(Some(self.parse_type()?))
        }
    }

    /// Parses some abstract statement at the current cursor position.
    fn parse_statement(&mut self) -> Result<Statement> {
        match self.token()?.kind {
            TokenKind::Let => self.parse_variable_declaration(),
            TokenKind::Break => self.parse_break(),
            TokenKind::Continue => self.parse_continue(),
            TokenKind::Return => self.parse_return(),
            TokenKind::If | TokenKind::Unless => Ok(self.parse_conditional()?),
            TokenKind::Loop => Ok(self.parse_infinite_loop()?),
            TokenKind::For => Ok(self.parse_iterator_loop()?),
            TokenKind::While => Ok(self.parse_predicate_loop()?),
            _ => {
                let expression = self.parse_expression()?;

                Ok(Statement::Expression(Box::new(expression)))
            }
        }
    }

    /// Parses a variable declaration statement at the current cursor position.
    fn parse_variable_declaration(&mut self) -> Result<Statement> {
        // Whatever the token is, consume it.
        let start = self.consume_any()?.start();

        let name = self.parse_identifier()?;
        let variable_type = self.parse_opt_type()?;

        self.consume(TokenKind::Assign)?;

        let value = self.parse_expression()?;
        let end = self.expect_semi()?.end();

        let variable = VariableDeclaration {
            name,
            variable_type,
            value,
            location: (start..end).into(),
        };

        Ok(Statement::VariableDeclaration(Box::new(variable)))
    }

    /// Parses a conditional statement at the current cursor position.
    fn parse_conditional(&mut self) -> Result<Statement> {
        match self.token()?.kind {
            TokenKind::If => self.parse_if_conditional(),
            TokenKind::Unless => self.parse_unless_conditional(),
            k => panic!("invalid conditional token given: {}", k),
        }
    }

    /// Parses an "if" conditional statement at the current cursor position.
    fn parse_if_conditional(&mut self) -> Result<Statement> {
        let start = self.consume(TokenKind::If)?.start();
        let mut cases = Vec::new();

        // Append the primary case
        self.parse_conditional_case(&mut cases)?;

        // Append the `else if` case
        self.parse_else_if_conditional_cases(&mut cases)?;

        // Append the `else` case
        self.parse_else_conditional_case(&mut cases)?;

        let end = cases.last().unwrap().location.end();

        let conditional = IfCondition {
            cases: Vec::new(),
            location: (start..end).into(),
        };

        Ok(Statement::If(Box::new(conditional)))
    }

    /// Parses an "unless" conditional statement at the current cursor position.
    fn parse_unless_conditional(&mut self) -> Result<Statement> {
        let start = self.consume(TokenKind::Unless)?.start();
        let mut cases = Vec::new();

        // Append the primary case
        self.parse_conditional_case(&mut cases)?;

        // Moan if any `else if` blocks are found
        if self.peek(TokenKind::Else)? && self.peek_next(TokenKind::If, 1)? {
            return Err(err!(self, UnlessElseIfClause));
        }

        // Append the `else` case
        self.parse_else_conditional_case(&mut cases)?;

        let end = cases.last().unwrap().location.end();

        let conditional = UnlessCondition {
            cases: Vec::new(),
            location: (start..end).into(),
        };

        Ok(Statement::Unless(Box::new(conditional)))
    }

    /// Parses a case within a conditional statement at the current cursor position.
    fn parse_conditional_case(&mut self, cases: &mut Vec<Condition>) -> Result<()> {
        let condition = self.parse_expression()?;
        let block = self.parse_block()?;

        let start = condition.location().start();
        let end = block.location().end();

        let case = Condition {
            condition: Some(condition),
            block,
            location: (start..end).into(),
        };

        cases.push(case);

        Ok(())
    }

    /// Parses zero-or-more `else-if` cases within a conditional statement at the current cursor position.
    fn parse_else_if_conditional_cases(&mut self, cases: &mut Vec<Condition>) -> Result<()> {
        loop {
            if !self.peek(TokenKind::Else)? || !self.peek_next(TokenKind::If, 1)? {
                break;
            }

            self.consume(TokenKind::Else)?;
            self.consume(TokenKind::If)?;

            self.parse_conditional_case(cases)?;
        }

        Ok(())
    }

    /// Parses zero-or-one `else` cases within a conditional statement at the current cursor position.
    fn parse_else_conditional_case(&mut self, cases: &mut Vec<Condition>) -> Result<()> {
        let start = match self.consume_if(TokenKind::Else)? {
            Some(t) => t.index.start,
            None => return Ok(()),
        };

        let block = self.parse_block()?;
        let end = block.location.end();

        let case = Condition {
            condition: None,
            block,
            location: (start..end).into(),
        };

        cases.push(case);

        Ok(())
    }

    /// Parses an infinite loop statement at the current cursor position.
    fn parse_infinite_loop(&mut self) -> Result<Statement> {
        let start = self.consume(TokenKind::Loop)?.start();
        let block = self.parse_block()?;

        let location = start..block.location.end();

        Ok(Statement::InfiniteLoop(Box::new(InfiniteLoop {
            block,
            location: location.into(),
        })))
    }

    /// Parses an iterator loop statement at the current cursor position.
    fn parse_iterator_loop(&mut self) -> Result<Statement> {
        let start = self.consume(TokenKind::For)?.start();

        let pattern = self.parse_identifier()?;

        self.consume(TokenKind::In)?;

        let collection = self.parse_expression()?;
        let block = self.parse_block()?;

        let location = start..block.location.end();

        Ok(Statement::IteratorLoop(Box::new(IteratorLoop {
            pattern,
            collection,
            block,
            location: location.into(),
        })))
    }

    /// Parses a predicate loop statement at the current cursor position.
    fn parse_predicate_loop(&mut self) -> Result<Statement> {
        let start = self.consume(TokenKind::While)?.start();

        let condition = self.parse_expression()?;
        let block = self.parse_block()?;

        let location = start..block.location.end();

        Ok(Statement::PredicateLoop(Box::new(PredicateLoop {
            condition,
            block,
            location: location.into(),
        })))
    }

    /// Parses a `break` statement at the current cursor position.
    fn parse_break(&mut self) -> Result<Statement> {
        let start = self.consume(TokenKind::Break)?.start();
        let end = self.expect_semi()?.end();

        Ok(Statement::Break(Box::new(Break {
            location: (start..end).into(),
        })))
    }

    /// Parses a `continue` statement at the current cursor position.
    fn parse_continue(&mut self) -> Result<Statement> {
        let start = self.consume(TokenKind::Continue)?.start();
        let end = self.expect_semi()?.end();

        Ok(Statement::Continue(Box::new(Continue {
            location: (start..end).into(),
        })))
    }

    /// Parses a return statement at the current cursor position.
    fn parse_return(&mut self) -> Result<Statement> {
        let start = self.consume(TokenKind::Return)?.start();

        let value = self.parse_opt_expression()?;
        let end = self.expect_semi()?.end();

        let statement = Statement::Return(Box::new(Return {
            value,
            location: (start..end).into(),
        }));

        Ok(statement)
    }

    /// Parses an expression on the current cursor position.
    fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_expression_with_precedence(0)
    }

    /// Parses an expression on the current cursor position, if one is defined.
    fn parse_opt_expression(&mut self) -> Result<Option<Expression>> {
        if self.peek(TokenKind::Semicolon)? {
            Ok(None)
        } else {
            Ok(Some(self.parse_expression()?))
        }
    }

    /// Parses an expression on the current cursor position, with a minimum precedence.
    fn parse_expression_with_precedence(&mut self, precedence: u8) -> Result<Expression> {
        let mut left = self.parse_prefix_expression()?;

        while precedence < self.token()?.precedence() {
            left = self.parse_infix_expression(left)?;
        }

        Ok(left)
    }

    /// Parses a prefix expression at the current cursor position.
    ///
    /// Prefix expressions are expressions which appear at the start of an expression,
    /// such as literals of prefix operators. In Pratt Parsing, this is also called "Nud" or "Null Denotation".
    fn parse_prefix_expression(&mut self) -> Result<Expression> {
        let kind = self.token()?.kind;

        match kind {
            TokenKind::LeftParen => Ok(self.parse_nested_expression()?),
            TokenKind::LeftBracket => Ok(self.parse_array_expression()?),
            TokenKind::New => Ok(self.parse_new_expression()?),
            TokenKind::SelfRef => Ok(self.parse_self_reference()?),
            TokenKind::Identifier => Ok(self.parse_named_expression()?),

            k if k.is_literal() => Ok(self.parse_literal()?),
            k if k.is_unary() => Ok(self.parse_unary()?),

            _ => Err(err!(self, InvalidExpression, actual, kind)),
        }
    }

    /// Parses a infix expression at the current cursor position.
    ///
    /// Infix expressions are expressions which appear in the middle of an expression,
    /// such as infix of postfix operators. In Pratt Parsing, this is also called "Led" or "Left Denotation".
    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression> {
        // If the next token is a '.', it's a chained expression and we should parse it as a member access expression.
        //
        // In essence, this statement is handling cases where:
        //
        //   let c = new Foo().bar()
        //
        // would be parsed as an operator expression, such as `c = Call(New('Foo'), '.', [Call('bar')])`, where
        // it really should be something like `c = Member(New('Foo'), 'bar')`.
        if self.peek(TokenKind::Dot)? {
            return self.parse_member(left);
        }

        let operator = match self.consume_any()? {
            t if t.kind.is_operator() => t,
            t => return Err(err!(self, InvalidExpression, actual, t.kind)),
        };

        let operator_loc = operator.index.clone();

        let right = self.parse_expression_with_precedence(operator.precedence())?;
        let name: String = operator.into();

        let start = left.location().start();
        let end = right.location().start();

        Ok(Expression::Call(Box::new(Call {
            callee: Some(left),
            name: Identifier {
                name,
                location: operator_loc.into(),
            },
            arguments: vec![right],
            type_parameters: vec![],
            location: (start..end).into(),
        })))
    }

    /// Parses an expression on the current cursor position, which is nested within parentheses.
    fn parse_nested_expression(&mut self) -> Result<Expression> {
        self.consume(TokenKind::LeftParen)?;

        let expression = self.parse_expression_with_precedence(0)?;

        // If the expression is followed by two dots ('..'), it's a range expression.
        if self.peek(TokenKind::Dot)? && self.peek_next(TokenKind::Dot, 1)? {
            return self.parse_range_expression(expression);
        }

        self.consume(TokenKind::RightParen)?;

        Ok(expression)
    }

    /// Parses an array expression on the current cursor position.
    fn parse_array_expression(&mut self) -> Result<Expression> {
        let token = self.token()?;
        let location: Location = token.index.into();

        let values = self.consume_comma_seq(TokenKind::LeftBracket, TokenKind::RightBracket, |p| {
            p.parse_expression()
        })?;

        Ok(Expression::Array(Box::new(Array { values, location })))
    }

    /// Parses a range expression on the current cursor position.
    fn parse_range_expression(&mut self, lower: Expression) -> Result<Expression> {
        self.consume(TokenKind::Dot)?;
        self.consume(TokenKind::Dot)?;

        let inclusive = self.consume_if(TokenKind::Assign)?.is_some();
        let upper = self.parse_expression()?;

        let start = lower.location().start();
        let end = upper.location().end();
        let location: Location = (start..end).into();

        let range = Range {
            lower,
            upper,
            inclusive,
            location,
        };

        Ok(Expression::Range(Box::new(range)))
    }

    /// Parses a `New` expression on the current cursor position.
    fn parse_new_expression(&mut self) -> Result<Expression> {
        let start = self.consume(TokenKind::New)?.start();

        let name = self.parse_type()?;
        let arguments = self.parse_call_arguments()?;

        let end = match arguments.last() {
            Some(a) => a.location().end(),
            None => name.location().end(),
        };

        Ok(Expression::New(Box::new(New {
            name: Box::new(name),
            arguments,
            location: (start..end).into(),
        })))
    }

    /// Parses a `self` reference expression on the current cursor position.
    fn parse_self_reference(&mut self) -> Result<Expression> {
        let location = self.consume(TokenKind::SelfRef)?.index;

        Ok(Expression::Variable(Box::new(Variable {
            name: lume_ast::Identifier {
                name: "self".to_string(),
                location: location.into(),
            },
        })))
    }

    /// Parses an expression on the current cursor position, which is preceded by some identifier.
    fn parse_named_expression(&mut self) -> Result<Expression> {
        let identifier = self.parse_identifier()?;
        let expression = Expression::Variable(Box::new(Variable {
            name: identifier.clone(),
        }));

        match self.token()?.kind {
            // If the next token is an opening parenthesis, it's a method invocation
            TokenKind::LeftParen | TokenKind::Less => self.parse_call(None, identifier),

            // If the next token is a dot, it's some form of member access
            TokenKind::Dot => self.parse_member(expression),

            // If the next token is an equal sign, it's an assignment expression
            TokenKind::Assign => self.parse_assignment(expression),

            // If the name stands alone, it's likely a variable reference
            _ => self.parse_variable(identifier),
        }
    }

    /// Parses a call expression on the current cursor position.
    fn parse_call(&mut self, callee: Option<Expression>, name: Identifier) -> Result<Expression> {
        let type_parameters = self.parse_type_parameters()?;
        let arguments = self.parse_call_arguments()?;

        let start = name.location.start();
        let end = match arguments.last() {
            Some(a) => a.location().end(),
            None => name.location.end(),
        };

        let call = Call {
            callee,
            name,
            arguments,
            type_parameters,
            location: (start..end).into(),
        };

        Ok(Expression::Call(Box::new(call)))
    }

    /// Parses zero-or-more call arguments at the current cursor position.
    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>> {
        self.consume_paren_seq(|p| p.parse_expression())
    }

    /// Parses a member expression on the current cursor position, which is preceded by some identifier.
    fn parse_member(&mut self, target: Expression) -> Result<Expression> {
        // If the expression is followed by two dots ('..'), it's a range expression.
        if self.peek(TokenKind::Dot)? && self.peek_next(TokenKind::Dot, 1)? {
            return self.parse_range_expression(target);
        }

        // Consume the dot token
        self.consume(TokenKind::Dot)?;

        let name = self.consume(TokenKind::Identifier)?;

        // If the next token is an opening parenthesis, it's a method invocation
        if self.peek(TokenKind::LeftParen)? || self.peek(TokenKind::Less)? {
            let identifier = Identifier {
                name: name.value.unwrap(),
                location: name.index.into(),
            };

            return self.parse_call(Some(target), identifier);
        }

        let start = target.location().start();
        let end = name.index.end;

        let expression = Expression::Member(Box::new(Member {
            callee: target,
            name: name.value.unwrap(),
            location: (start..end).into(),
        }));

        // If there is yet another dot, it's part of a longer expression.
        if self.peek(TokenKind::Dot)? {
            return self.parse_member(expression);
        }

        // Otherwise, return the expression, as-is.
        Ok(expression)
    }

    /// Parses an assignment expression on the current cursor position.
    fn parse_assignment(&mut self, target: Expression) -> Result<Expression> {
        // Consume the equal sign
        self.consume(TokenKind::Assign)?;

        let value = self.parse_expression()?;

        let start = target.location().start();
        let end = value.location().end();

        Ok(Expression::Assignment(Box::new(Assignment {
            target,
            value,
            location: (start..end).into(),
        })))
    }

    /// Parses a variable reference expression on the current cursor position.
    fn parse_variable(&mut self, target: Identifier) -> Result<Expression> {
        let variable = Variable { name: target };

        Ok(Expression::Variable(Box::new(variable)))
    }

    /// Parses a literal value expression on the current cursor position.
    fn parse_literal(&mut self) -> Result<Expression> {
        let token = self.token()?;
        let location: Location = token.index.into();

        let literal = match token.kind {
            TokenKind::Integer(radix) => {
                let mut value = token.value.unwrap();

                // Remove all underscores from the literal
                value.remove_matches("_");

                // Remove radix prefixes, as `from_str_radix` does not support them
                // being included.
                if value.len() >= 2 && value.starts_with("0") {
                    let c = value.as_bytes()[1] as char;

                    if matches!(c.to_ascii_lowercase(), 'b' | 'o' | 'd' | 'x') {
                        let mut value_chars = value.chars();

                        value_chars.next();
                        value_chars.next();

                        value = value_chars.collect();
                    }
                }

                let int_value = match i64::from_str_radix(&value, radix) {
                    Ok(v) => v,
                    Err(_) => return Err(err!(self, InvalidLiteral, value, value, target, token.kind)),
                };

                let kind = if let Some(ty) = token.ty {
                    match ty.as_str() {
                        "i8" => IntKind::I8,
                        "u8" => IntKind::U8,
                        "i16" => IntKind::I16,
                        "u16" => IntKind::U16,
                        "i32" => IntKind::I32,
                        "u32" => IntKind::U32,
                        "i64" => IntKind::I64,
                        "u64" => IntKind::U64,
                        t => return Err(err!(self, InvalidLiteralType, found, t.to_string())),
                    }
                } else {
                    IntKind::I32
                };

                Literal::Int(Box::new(IntLiteral {
                    value: int_value,
                    location,
                    kind,
                }))
            }
            TokenKind::Float => {
                let value = token.value.unwrap();
                let float_value = match value.parse::<f64>() {
                    Ok(v) => v,
                    Err(_) => return Err(err!(self, InvalidLiteral, value, value, target, token.kind)),
                };

                let kind = if let Some(ty) = token.ty {
                    match ty.as_str() {
                        "f32" => FloatKind::F32,
                        "f64" => FloatKind::F64,
                        t => return Err(err!(self, InvalidLiteralType, found, t.to_string())),
                    }
                } else {
                    FloatKind::F64
                };

                Literal::Float(Box::new(FloatLiteral {
                    value: float_value,
                    location,
                    kind,
                }))
            }
            TokenKind::String => {
                let value = token.value.unwrap();

                Literal::String(Box::new(StringLiteral { value, location }))
            }
            TokenKind::True => Literal::Boolean(Box::new(BooleanLiteral { value: true, location })),
            TokenKind::False => Literal::Boolean(Box::new(BooleanLiteral { value: false, location })),
            k => return Err(err!(self, Unimplemented, desc, format!("{:?} literals", k))),
        };

        self.skip()?;

        Ok(Expression::Literal(Box::new(literal)))
    }

    /// Parses a unary expression on the current cursor position.
    fn parse_unary(&mut self) -> Result<Expression> {
        let operator = match self.consume_any()? {
            t if t.kind.is_unary() => t,
            t => return Err(err!(self, InvalidExpression, actual, t.kind)),
        };

        let right = self.parse_expression_with_precedence(UNARY_PRECEDENCE)?;

        // As a quality of life feature, we can apply the unary operator directly to the expression,
        // if it can be done at parsing time. If not, we create an ordinary unary expression.
        if let Expression::Literal(literal) = right {
            let inner = match *literal {
                // If the operator is a unary minus and the right-hand side is a number literal, we can negate
                // the value directly.
                Literal::Int(mut int_literal) if operator.kind == TokenKind::Sub => {
                    int_literal.value *= -1;

                    Expression::Literal(Box::new(Literal::Int(int_literal)))
                }
                Literal::Float(mut float_literal) if operator.kind == TokenKind::Sub => {
                    float_literal.value *= -1.0;

                    Expression::Literal(Box::new(Literal::Float(float_literal)))
                }

                // If the operator is a unary negation and the right-hand side is a boolean literal, we can negate
                // the value directly.
                Literal::Boolean(mut bool_value) if operator.kind == TokenKind::Exclamation => {
                    bool_value.value = !bool_value.value;

                    Expression::Literal(Box::new(Literal::Boolean(bool_value)))
                }

                // Otherwise, leave it be.
                expr => Expression::Literal(Box::new(expr)),
            };

            return Ok(inner);
        }

        Ok(right)
    }

    /// Parses some expression, if an equal sign is consumed. Otherwise, returns `None`.
    fn parse_opt_assignment(&mut self) -> Result<Option<Expression>> {
        if self.consume_if(TokenKind::Assign)?.is_none() {
            Ok(None)
        } else {
            Ok(Some(self.parse_expression()?))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lume_diag::Error;

    #[track_caller]
    fn parse(input: &str) -> Vec<TopLevelExpression> {
        let parser = Parser::parse_str(input);

        parser.unwrap()
    }

    #[track_caller]
    fn parse_err(input: &str) -> Error {
        let parser = Parser::parse_str(input);

        parser.unwrap_err()
    }

    #[track_caller]
    fn parse_expr(input: &str) -> Vec<TopLevelExpression> {
        let source = format!("fn main() -> void {{ {0} }}", input);

        parse(&source)
    }

    #[track_caller]
    fn parse_expr_err(input: &str) -> Error {
        let source = format!("fn main() -> void {{ {0} }}", input);

        parse_err(&source)
    }

    macro_rules! assert_module_eq {
        (
            $input: expr,
            $expression: expr
        ) => {
            let parsed = parse($input);

            assert_eq!(parsed, $expression)
        };
    }

    macro_rules! assert_err_eq {
        (
            $input: expr,
            $message: expr
        ) => {
            let error = parse_err($input);

            assert_eq!(error.message(), $message)
        };
    }

    macro_rules! assert_expr_err_eq {
        (
            $input: expr,
            $message: expr
        ) => {
            let error = parse_expr_err($input);

            assert_eq!(error.message(), $message)
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

            insta::assert_debug_snapshot!(parse($input));
        };
    }

    macro_rules! assert_err_snap_eq {
        (
            $input: expr,
            $($expr:expr),+
        ) => {
            set_snapshot_suffix!( $($expr),+ );

            insta::assert_debug_snapshot!(parse_err($input));
        };
    }

    macro_rules! assert_expr_snap_eq {
        (
            $input: expr,
            $($expr:expr),+
        ) => {
            set_snapshot_suffix!( $($expr),+ );

            insta::assert_debug_snapshot!(parse_expr($input));
        };
    }

    macro_rules! assert_expr_err_snap_eq {
        (
            $input: expr,
            $($expr:expr),+
        ) => {
            set_snapshot_suffix!( $($expr),+ );

            insta::assert_debug_snapshot!(parse_expr_err($input));
        };
    }

    #[test]
    fn test_empty_module() {
        assert_module_eq!("", vec![]);
    }

    #[test]
    fn test_whitespace_module() {
        assert_module_eq!("    ", vec![]);
    }

    #[test]
    fn test_newline_module() {
        assert_module_eq!("\n\n    \n", vec![]);
    }

    #[test]
    fn test_imports() {
        assert_eq!(
            Parser::parse_str("import std (Int)").unwrap(),
            vec![TopLevelExpression::Import(Box::new(Import {
                path: NamespacePath {
                    path: vec![Identifier {
                        name: "std".into(),
                        location: Location(7..10)
                    }],
                    location: Location(7..10)
                },
                names: vec![Identifier {
                    name: "Int".into(),
                    location: Location(12..15)
                }],
                location: Location(0..16)
            }))]
        );

        assert_eq!(
            Parser::parse_str("import std::io (File)").unwrap(),
            vec![TopLevelExpression::Import(Box::new(Import {
                path: NamespacePath {
                    path: vec![
                        Identifier {
                            name: "std".into(),
                            location: Location(7..10)
                        },
                        Identifier {
                            name: "io".into(),
                            location: Location(12..14)
                        }
                    ],
                    location: Location(8..14)
                },
                names: vec![Identifier {
                    name: "File".into(),
                    location: Location(16..20)
                }],
                location: Location(0..21)
            }))]
        );

        assert_eq!(
            Parser::parse_str("import std::io (File, Buffer)").unwrap(),
            vec![TopLevelExpression::Import(Box::new(Import {
                path: NamespacePath {
                    path: vec![
                        Identifier {
                            name: "std".into(),
                            location: Location(7..10)
                        },
                        Identifier {
                            name: "io".into(),
                            location: Location(12..14)
                        }
                    ],
                    location: Location(7..14)
                },
                names: vec![
                    Identifier {
                        name: "File".into(),
                        location: Location(16..20)
                    },
                    Identifier {
                        name: "Buffer".into(),
                        location: Location(22..28)
                    }
                ],
                location: Location(0..29)
            }))]
        );

        assert_eq!(
            Parser::parse_str("import std::io ()").unwrap(),
            vec![TopLevelExpression::Import(Box::new(Import {
                path: NamespacePath {
                    path: vec![
                        Identifier {
                            name: "std".into(),
                            location: Location(7..10)
                        },
                        Identifier {
                            name: "io".into(),
                            location: Location(12..14)
                        }
                    ],
                    location: Location(7..14)
                },
                names: vec![],
                location: Location(0..17)
            }))]
        );

        assert_err_eq!("import std::io", "Invalid import path");
        assert_err_eq!("import std::io::", "Expected identifier");
        assert_err_eq!("import ::std::io", "Expected identifier");
    }

    #[test]
    fn test_namespace_snapshots() {
        assert_snap_eq!("namespace std", "path_1");
        assert_snap_eq!("namespace std::io", "path_2");
        assert_snap_eq!("namespace std::io::path", "path_3");
        assert_snap_eq!("namespace System::IO", "path_casing");
        assert_err_eq!("namespace", "Expected identifier");
        assert_err_eq!("namespace ::std", "Expected identifier");
        assert_err_eq!("namespace std::io::", "Expected identifier");
    }

    #[test]
    fn test_function_definition_snapshots() {
        assert_snap_eq!("fn main() -> void {}", "empty");
        assert_snap_eq!("fn main() -> void { let a = 0; }", "statement");
        assert_snap_eq!("fn main() -> void { let a = 0; let b = 1; }", "statements");
        assert_snap_eq!("fn main() {}", "no_return_type");
        assert_snap_eq!("fn main(argc: u8) -> void { }", "parameter");
        assert_snap_eq!("fn main(argc: u8, arcv: [String]) -> void { }", "parameters");
        assert_snap_eq!("fn external main() -> void", "external");
        assert_err_snap_eq!("fn external main() -> void {}", "external_body");
        assert_snap_eq!("pub fn main() -> void {}", "pub_modifier");
        assert_snap_eq!("fn loop() -> void {}", "reserved_keyword");
    }

    #[test]
    fn test_literal_snapshots() {
        assert_expr_snap_eq!("\"\"", "string_empty");
        assert_expr_snap_eq!("\"string\"", "string_content");
        assert_expr_snap_eq!("true", "bool_true");
        assert_expr_snap_eq!("false", "bool_false");
        assert_expr_snap_eq!("ident", "ident");
        assert_expr_snap_eq!("IDENT", "ident_case");
        assert_expr_snap_eq!("__IDENT__", "ident_underscore");
        assert_expr_snap_eq!("0", "int");
        assert_expr_snap_eq!("55", "int_positive");
        assert_expr_snap_eq!("-55", "int_negative");
        assert_expr_snap_eq!("0x55", "int_hex_positive");
        assert_expr_snap_eq!("-0x55", "int_hex_negative");
        assert_expr_snap_eq!("0b01010101", "int_bin_positive");
        assert_expr_snap_eq!("-0b01010101", "int_bin_negative");
        assert_expr_snap_eq!("0o125", "int_oct_positive");
        assert_expr_snap_eq!("-0o125", "int_oct_negative");
    }

    #[test]
    fn test_self_snapshots() {
        assert_expr_snap_eq!("self", "self");
        assert_expr_snap_eq!("self + self", "self_binary_op");
        assert_expr_snap_eq!("self.invoke()", "self_call");
    }

    #[test]
    fn test_conditional_snapshots() {
        assert_expr_snap_eq!("if true { }", "if_empty");
        assert_expr_snap_eq!("if true { let a = 1; }", "if_statement");
        assert_expr_snap_eq!("if true { } else if false { }", "if_else_if_empty");
        assert_expr_snap_eq!("if true { } else { }", "if_else_empty");
        assert_expr_snap_eq!("if true { } else if false { } else { }", "if_else_if_else_empty");
        assert_expr_snap_eq!("unless true { }", "unless_empty");
        assert_expr_snap_eq!("unless true { } else { }", "unless_else_empty");
        assert_expr_snap_eq!("if a == 1 { }", "equality_empty");
        assert_expr_snap_eq!("if a != 1 { }", "inequality_empty");
        assert_expr_snap_eq!("if true { let a = 0; }", "if_statement");
        assert_expr_snap_eq!("if true { let a = 0; let b = 0; }", "if_statements");
        assert_expr_snap_eq!(
            "if true { let a = 0; } else if false { let a = 0; }",
            "else_if_statements"
        );
        assert_expr_snap_eq!("unless true { let a = 0; }", "unless_statement");
        assert_expr_err_snap_eq!("unless true { } else if false {}", "unless_else_if");
        assert_expr_err_eq!("unless true { } else if false { }", "Unexpected `else if` clause");
    }

    #[test]
    fn test_loop_snapshots() {
        assert_expr_snap_eq!("loop { }", "inf_loop_empty");
        assert_expr_snap_eq!("loop { let a = 0; }", "inf_loop_statement");
        assert_expr_snap_eq!("loop { break; }", "inf_loop_break");
        assert_expr_snap_eq!("loop { continue; }", "inf_loop_continue");

        assert_expr_snap_eq!("while true { }", "pred_loop_empty");
        assert_expr_snap_eq!("while true { let a = 0; }", "pred_loop_statement");
        assert_expr_snap_eq!("while true { break; }", "pred_loop_break");
        assert_expr_snap_eq!("while true { continue; }", "pred_loop_continue");

        assert_expr_snap_eq!("for pattern in collection { }", "iter_loop_empty");
        assert_expr_snap_eq!("for pattern in collection { let a = 0; }", "iter_loop_statement");
        assert_expr_snap_eq!("for pattern in collection { break; }", "iter_loop_break");
        assert_expr_snap_eq!("for pattern in collection { continue; }", "iter_loop_continue");
        assert_expr_snap_eq!("for pattern in [1, 2, 3] { }", "iter_expr");
    }

    #[test]
    fn test_array_snapshots() {
        assert_expr_snap_eq!("let _ = [];", "empty");
        assert_expr_snap_eq!("let _ = [a];", "single");
        assert_expr_snap_eq!("let _ = [a, b];", "multiple");
        assert_expr_snap_eq!("let _ = [a, [a, b]];", "nested");
        assert_expr_err_snap_eq!("let _ = [a b];", "missing_comma");
        assert_expr_err_snap_eq!("let _ = [a, ];", "extra_comma");
    }

    #[test]
    fn test_range_snapshots() {
        assert_expr_snap_eq!("let _ = (0..1);", "literal_exclusive");
        assert_expr_snap_eq!("let _ = (0..=1);", "literal_inclusive");
        assert_expr_snap_eq!("let _ = (a..b);", "expr_exclusive");
        assert_expr_snap_eq!("let _ = (a..=b);", "expr_inclusive");
        assert_expr_snap_eq!("let _ = ((a + b)..(a + b + 1));", "expr_nested_exclusive");
        assert_expr_snap_eq!("let _ = ((a + b)..=(a + b + 1));", "expr_nested_inclusive");
    }

    #[test]
    fn test_new_snapshots() {
        assert_expr_snap_eq!("let _ = new A();", "empty");
        assert_expr_snap_eq!("let _ = new A(a);", "param_1");
        assert_expr_snap_eq!("let _ = new A(a, b);", "param_2");
        assert_expr_snap_eq!("let _ = new A<T>(a, b);", "generic");
        assert_expr_snap_eq!("let _ = new [A](a);", "array");
    }

    #[test]
    fn test_call_snapshots() {
        assert_expr_snap_eq!("let _ = call();", "function_empty");
        assert_expr_snap_eq!("let _ = call(a);", "function_param_1");
        assert_expr_snap_eq!("let _ = call(a, b);", "function_param_2");
        assert_expr_snap_eq!("let _ = call<T>(a, b);", "function_generic");

        assert_expr_snap_eq!("let _ = a.call();", "method_empty");
        assert_expr_snap_eq!("let _ = a.call(a);", "method_param_1");
        assert_expr_snap_eq!("let _ = a.call(a, b);", "method_param_2");
        assert_expr_snap_eq!("let _ = a.call<T>(a, b);", "method_generic");
    }

    #[test]
    fn test_return_snapshots() {
        assert_expr_snap_eq!("return;", "empty");
        assert_expr_snap_eq!("return 1;", "scalar");
        assert_expr_snap_eq!("return a.b(c);", "call");
        assert_expr_err_snap_eq!("return;;", "extra_semi");
    }

    #[test]
    fn test_generic_function_snapshots() {
        assert_snap_eq!("fn test() -> void {}", "no_generics");
        assert_snap_eq!("fn test<>() -> void {}", "empty_generics");
        assert_snap_eq!("fn test<T>() -> void {}", "single_generic");
        assert_snap_eq!("fn test<T1, T2>() -> void {}", "multiple_generics");
        assert_snap_eq!("fn test<T: Numeric>() -> void {}", "constrained_generic");
        assert_snap_eq!("fn test<T1: Numeric, T2: Numeric>() -> void {}", "constrained_generics");
        assert_snap_eq!(
            "fn test<T: Numeric + Floating>() -> void {}",
            "multiple_constrained_types"
        );
        assert_snap_eq!(
            "fn test<T1: Numeric + Floating, T2: Numeric + Floating>() -> void {}",
            "multiple_types_with_multiple_constraints"
        );
        assert_err_snap_eq!("fn test<T1,>() -> void {}", "missing_generic");
        assert_err_snap_eq!("fn test<T1 T2>() -> void {}", "missing_comma");
    }

    #[test]
    fn test_class_snapshots() {
        assert_snap_eq!("class Int32 {}", "empty");
        assert_snap_eq!("class builtin Int32 {}", "builtin");
        assert_err_snap_eq!("class 1A {}", "invalid_name");

        assert_snap_eq!(
            r#"
            class Foo {
                fn bar() -> Int32 {
                    return 0;
                }
            }"#,
            "method"
        );

        assert_snap_eq!(
            r#"
            class Foo {
                fn bar() { }
            }"#,
            "method_no_ret"
        );

        assert_snap_eq!(
            r#"
            class Foo {
                pub fn bar() -> Int32 {
                    return 0;
                }
            }"#,
            "pub_method"
        );

        assert_snap_eq!(
            r#"
            class Foo {
                fn external bar() -> Int32
            }"#,
            "ext_method"
        );

        assert_snap_eq!(
            r#"
            class Foo {
                pub fn ==() -> bool {
                    return true;
                }
            }"#,
            "operator_method"
        );

        assert_snap_eq!(
            r#"
            class Foo {
                pub fn bar(self) -> bool {
                    return true;
                }
            }"#,
            "self_method"
        );

        assert_snap_eq!(
            r#"
            class Foo {
                fn bar<T>() -> Int32 { }
            }"#,
            "generic_method"
        );

        assert_snap_eq!(
            r#"
            class Foo {
                let x: Int32 = 0;
            }"#,
            "property"
        );

        assert_snap_eq!(
            r#"
            class Foo {
                let x: Int32;
            }"#,
            "property_no_default"
        );

        assert_snap_eq!(
            r#"
            class Foo {
                pub let x: Int32 = 1;
            }"#,
            "pub_property"
        );
    }

    #[test]
    fn test_generic_class_snapshots() {
        assert_snap_eq!("class Test {}", "no_generics");
        assert_snap_eq!("class Test<> {}", "empty_generics");
        assert_snap_eq!("class Test<T> {}", "single_generic");
        assert_snap_eq!("class Test<T1, T2> {}", "multiple_generics");
        assert_snap_eq!("class Test<T: Numeric> {}", "constrained_generic");
        assert_snap_eq!("class Test<T1: Numeric, T2: Numeric> {}", "constrained_generics");
        assert_err_snap_eq!("class Test<T1,> {}", "missing_generic");
        assert_err_snap_eq!("class Test<T1 T2> {}", "missing_comma");
    }

    #[test]
    fn test_generic_method_snapshots() {
        assert_snap_eq!("class Test { fn test() -> void {} }", "no_generics");
        assert_snap_eq!("class Test { fn test<>() -> void {} }", "empty_generics");
        assert_snap_eq!("class Test { fn test<T>() -> void {} }", "single_generic");
        assert_snap_eq!("class Test { fn test<T1, T2>() -> void {} }", "multiple_generics");
        assert_snap_eq!("class Test { fn test<T: Numeric>() -> void {} }", "constrained_generic");
        assert_snap_eq!(
            "class Test { fn test<T1: Numeric, T2: Numeric>() -> void {} }",
            "constrained_generics"
        );
        assert_err_snap_eq!("class Test { fn test<T1,>() -> void {} }", "missing_generic");
        assert_err_snap_eq!("class Test { fn test<T1 T2>() -> void {} }", "missing_comma");
    }

    #[test]
    fn test_enum_snapshots() {
        assert_snap_eq!("enum Foo {}", "empty");
        assert_snap_eq!("enum Foo { Bar }", "single_variant");
        assert_snap_eq!("enum Foo { Bar, Baz }", "multiple_variants");
        assert_err_snap_eq!("enum Foo { Bar Baz }", "missing_comma");
        assert_err_snap_eq!("enum Foo { Bar, Baz, }", "extra_comma");

        assert_snap_eq!(
            r#"
            enum Foo {
                Bar()
            }"#,
            "variant_param_empty"
        );

        assert_snap_eq!(
            r#"
            enum Foo {
                Bar(int)
            }"#,
            "variant_param_single"
        );

        assert_snap_eq!(
            r#"
            enum Foo {
                Bar(int, int)
            }"#,
            "variant_param_multiple"
        );

        assert_snap_eq!(
            r#"
            enum Foo {
                Bar(int, int),
                Baz(int, int)
            }"#,
            "multiple_variants_multiple_params"
        );
    }

    #[test]
    fn test_type_alias_snapshots() {
        assert_snap_eq!("type A = B", "scalar");
        assert_snap_eq!("type A = [B]", "array");
        assert_snap_eq!("type A = B<C>", "generic");
    }

    #[test]
    fn test_trait_snapshots() {
        assert_snap_eq!("trait Add { }", "empty");
        assert_snap_eq!("trait Add { pub fn add(other: int) -> int }", "method");
        assert_snap_eq!(
            "trait Add { pub fn add(other: int) -> int { return self + other; } }",
            "method_impl"
        );
        assert_snap_eq!("trait Add<T> { }", "generic");
        assert_snap_eq!("trait Add<T1, T2> { }", "generics");
        assert_snap_eq!("trait Add { fn add(other: int) -> int }", "private_method");
        assert_snap_eq!("trait Add<T: Numeric> {}", "constrained_generic");
        assert_snap_eq!("trait Add<T1: Numeric, T2: Numeric> {}", "constrained_generics");
        assert_snap_eq!("trait Add { pub fn add(other: int) { } }", "method_no_ret");
    }

    #[test]
    fn test_use_trait_snapshots() {
        assert_snap_eq!("use Add in Int32 {}", "empty");

        assert_snap_eq!(
            r#"
            use Add in Int32 {
                fn add(other: Int32) -> Int32 {
                    return self + other;
                }
            }"#,
            "priv_method"
        );

        assert_snap_eq!(
            r#"
            use Add in Int32 {
                pub fn add(other: Int32) -> Int32 {
                    return self + other;
                }
            }"#,
            "pub_method"
        );

        assert_snap_eq!(
            r#"
            use Add in Int32 {
                fn add(other: Int32) {}
            }"#,
            "method_no_ret"
        );

        assert_snap_eq!(
            r#"
            use Cast in Int32 {
                pub fn to_string() -> String {
                    return self;
                }

                pub fn to_int() -> Int32 {
                    return self;
                }
            }"#,
            "methods"
        );

        assert_snap_eq!(
            r#"
            use Add<Int32> in Int32 {
                pub fn add(other: Int32) -> Int32 {
                    return self + other;
                }
            }"#,
            "generic"
        );

        assert_snap_eq!(
            r#"
            use Add<Int32, Int64> in Int32 {
                pub fn add(other: Int32) -> Int64 {
                    return self + other;
                }
            }"#,
            "generics"
        );

        assert_snap_eq!(
            r#"
            use Enumerable<T> in Vector<T> {
                pub fn next() -> T { }
            }"#,
            "generic_type"
        );
    }

    #[test]
    fn test_doc_comments_snapshots() {
        assert_snap_eq!(
            r#"/// This is a doc comment
            fn foo() -> void { }"#,
            "function"
        );

        assert_snap_eq!(
            r#"/// This is a doc comment
            class Foo { }"#,
            "class"
        );

        assert_snap_eq!(
            r#"class Foo {
                /// This is a doc comment
                pub let bar: Int32 = 0;
            }"#,
            "property"
        );

        assert_snap_eq!(
            r#"class Foo {
                /// This is a doc comment
                pub fn bar() -> void { }
            }"#,
            "method"
        );

        assert_snap_eq!(
            r#"/// This is a doc comment
            trait Foo { }"#,
            "trait"
        );

        assert_snap_eq!(
            r#"trait Foo {
                /// This is a doc comment
                pub fn bar() -> void { }
            }"#,
            "trait_method"
        );

        assert_snap_eq!(
            r#"/// This is a doc comment
            enum Foo {
                Bar
            }"#,
            "enum"
        );

        assert_snap_eq!(
            r#"enum Foo {
                /// This is a doc comment
                Bar
            }"#,
            "enum_case"
        );

        assert_snap_eq!(
            r#"/// This is a doc comment
            type Foo = Bar"#,
            "type_alias"
        );
    }
}
