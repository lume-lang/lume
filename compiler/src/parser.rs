use diag::Result;

use crate::ast::*;
use crate::lexer::*;
use crate::parser::errors::*;
use crate::{Module, State};

pub mod errors;

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

pub struct Parser<'a> {
    /// Defines the module which is being parsed.
    module: &'a mut Module,

    /// Defines the lexer which tokenizes the module source code.
    lexer: Lexer,

    /// Defines the current position within the module source code, given in a zero-based index.
    position: usize,

    /// Defines the next token within the module source code.
    ///
    /// This token is saved, to prevent multiple calls to `peek` from consuming irrelevant tokens.
    peeked: Option<Token>,
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
            source: $self.module.source.clone(),
            range: $self.token()?.index,
            $( $field: $value ),*
        }
        .into()
    };
}

impl<'a> Parser<'a> {
    pub fn new(module: &'a mut Module) -> Self {
        let lexer = Lexer::new(module.source.clone());

        Parser {
            module,
            lexer,
            position: 0,
            peeked: None,
        }
    }

    /// Parses all the modules within the given state.
    pub fn parse(state: &mut State) -> Result<()> {
        for module in &mut state.modules {
            let mut parser = Parser::new(module);
            parser.parse_module()?;
        }

        Ok(())
    }

    /// Parses the module within the parser state.
    ///
    /// This function iterates through the tokens of the module source code,
    /// parsing each top-level expression and collecting them into a vector.
    fn parse_module(&mut self) -> Result<()> {
        loop {
            if self.lexer.is_eof() {
                break;
            }

            let expression = self.top_level_expression()?;

            self.module.expressions.push(expression);
        }

        Ok(())
    }

    /// Parses a single token from the lexer.
    ///
    /// Returns the parsed token or a parsing error.
    fn token(&mut self) -> Result<Token> {
        // If a token is buffered, return it instead.
        if let Some(token) = self.peeked.clone() {
            return Ok(token);
        }

        let token = loop {
            let token = self.lexer.next_token()?;

            match token.kind {
                TokenKind::Whitespace => {
                    self.position += token.len();
                    continue;
                }
                _ => break token,
            }
        };

        self.position += token.len();
        self.peeked = Some(token.clone());

        Ok(token)
    }

    /// Peeks the next token from the lexer and returns it if it matches the expected kind.
    ///
    /// Returns a boolean indicating whether the token matches the expected kind.
    fn peek(&mut self, kind: TokenKind) -> Result<bool> {
        let token = self.token()?;

        if token.kind == kind {
            return Ok(true);
        }

        Ok(false)
    }

    /// Advances the cursor position by a single token.
    fn skip(&mut self) -> Result<()> {
        self.peeked = None;
        self.token()?;

        Ok(())
    }

    /// Consumes the next token from the lexer and returns it if it matches the expected kind.
    ///
    /// If the token does not match the expected kind, an error is returned.
    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        let current = self.token()?;

        match self.peek(kind)? {
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
        let current = self.token()?;

        match self.peek(kind)? {
            true => {
                self.skip()?;

                Ok(Some(current))
            }
            false => Ok(None),
        }
    }

    /// Parses the next token as an identifier.
    fn identifier(&mut self) -> Result<Identifier> {
        let name = self.consume(TokenKind::Identifier)?.value.unwrap();
        let identifier = Identifier { name };

        Ok(identifier)
    }

    fn block(&mut self) -> Result<Block> {
        let mut block = Block { statements: Vec::new() };

        self.consume(TokenKind::LeftCurly)?;

        loop {
            if self.peek(TokenKind::RightCurly)? {
                break;
            }
            block.statements.push(self.statement()?);
        }

        self.consume(TokenKind::RightCurly)?;

        Ok(block)
    }

    /// Returns an empty block for external functions.
    ///
    /// Also functions as an extra layer to report errors, if a function body is declared.
    fn external_block(&mut self) -> Result<Block> {
        if self.peek(TokenKind::LeftCurly)? {
            return Err(err!(self, ExternalFunctionBody));
        }

        Ok(Block::empty())
    }

    fn top_level_expression(&mut self) -> Result<TopLevelExpression> {
        let current = self.token()?;

        match current.kind {
            TokenKind::Import => self.import(),
            TokenKind::Class => self.class(),
            TokenKind::Fn | TokenKind::Pub => self.function(),
            TokenKind::Enum | TokenKind::Type => self.type_definition(),
            _ => Err(err!(self, InvalidTopLevelStatement, actual, current.kind.clone())),
        }
    }

    fn import(&mut self) -> Result<TopLevelExpression> {
        self.consume(TokenKind::Import)?;

        let name = match self.identifier() {
            Ok(name) => name,
            Err(_) => return Err(err!(self, ExpectedIdentifier)),
        };

        let import_def = Import { name };

        Ok(TopLevelExpression::Import(Box::new(import_def)))
    }

    fn class(&mut self) -> Result<TopLevelExpression> {
        self.consume(TokenKind::Class)?;

        let builtin = self.consume_if(TokenKind::Builtin)?.is_some();

        let name = match self.identifier() {
            Ok(name) => name,
            Err(_) => return Err(err!(self, ExpectedClassName)),
        };

        self.consume(TokenKind::LeftCurly)?;
        let members = self.class_members()?;
        self.consume(TokenKind::RightCurly)?;

        let class_def = ClassDefinition { name, builtin, members };

        Ok(TopLevelExpression::Class(Box::new(class_def)))
    }

    fn class_members(&mut self) -> Result<Vec<ClassMember>> {
        let mut members = Vec::new();

        loop {
            if self.peek(TokenKind::RightCurly)? {
                break;
            }

            members.push(self.class_member()?);
        }

        Ok(members)
    }

    fn class_member(&mut self) -> Result<ClassMember> {
        let visibility = self.visibility()?;

        match self.token()?.kind {
            TokenKind::Let => self.property(visibility),
            TokenKind::Fn => self.method(visibility),
            kind => return Err(err!(self, ExpectedClassMember, actual, kind)),
        }
    }

    fn visibility(&mut self) -> Result<Visibility> {
        match self.token()?.kind {
            // If the member is marked as public, make it so.
            TokenKind::Pub => {
                self.skip()?;

                Ok(Visibility::Public(Box::new(Public {})))
            }

            // By default, make it private.
            _ => Ok(Visibility::Private(Box::new(Private {}))),
        }
    }

    fn property(&mut self, visibility: Visibility) -> Result<ClassMember> {
        self.consume(TokenKind::Let)?;

        let kind = self.token()?.kind;
        let name = match self.identifier() {
            Ok(name) => name,
            Err(_) => return Err(err!(self, ExpectedClassMember, actual, kind)),
        };

        let property_type = if self.consume_if(TokenKind::Colon)?.is_some() {
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };

        let default_value = if self.consume_if(TokenKind::Assign)?.is_some() {
            Some(self.expression()?)
        } else {
            None
        };

        let property_def = Property {
            visibility,
            name,
            property_type,
            default_value,
        };

        Ok(ClassMember::Property(Box::new(property_def)))
    }

    fn method(&mut self, visibility: Visibility) -> Result<ClassMember> {
        self.consume(TokenKind::Fn)?;

        let external = self.consume_if(TokenKind::External)?.is_some();

        let name = match self.identifier() {
            Ok(name) => name,
            Err(_) => return Err(err!(self, ExpectedFunctionName)),
        };

        let parameters = self.parameters()?;

        self.consume(TokenKind::Arrow)?;
        let return_type = self.parse_type()?;
        let block = if external {
            self.external_block()?
        } else {
            self.block()?
        };

        let method_def = MethodDefinition {
            visibility,
            external,
            name,
            parameters,
            return_type: Box::new(return_type),
            block,
        };

        Ok(ClassMember::MethodDefinition(Box::new(method_def)))
    }

    fn function(&mut self) -> Result<TopLevelExpression> {
        let visibility = self.visibility()?;

        self.consume(TokenKind::Fn)?;

        let external = self.consume_if(TokenKind::External)?.is_some();

        let name = match self.identifier() {
            Ok(name) => name,
            Err(_) => return Err(err!(self, ExpectedFunctionName)),
        };

        let parameters = self.parameters()?;

        self.consume(TokenKind::Arrow)?;
        let return_type = self.parse_type()?;
        let block = if external {
            self.external_block()?
        } else {
            self.block()?
        };

        let function_def = FunctionDefinition {
            visibility,
            external,
            name,
            parameters,
            return_type: Box::new(return_type),
            block,
        };

        Ok(TopLevelExpression::FunctionDefinition(Box::new(function_def)))
    }

    fn parameters(&mut self) -> Result<Vec<Parameter>> {
        // If no opening parenthesis, no parameters are defined.
        if !self.peek(TokenKind::LeftParen)? {
            return Ok(Vec::new());
        }

        self.consume(TokenKind::LeftParen)?;

        let mut parameters = Vec::new();

        while self.consume_if(TokenKind::RightParen)?.is_none() {
            if !parameters.is_empty() && !self.peek(TokenKind::RightParen)? {
                self.consume(TokenKind::Comma)?;
            }

            parameters.push(self.parameter()?);
        }

        Ok(parameters)
    }

    fn parameter(&mut self) -> Result<Parameter> {
        let name = self.identifier()?;

        self.consume(TokenKind::Colon)?;

        let param_type = self.parse_type()?;

        Ok(Parameter { name, param_type })
    }

    fn type_definition(&mut self) -> Result<TopLevelExpression> {
        let def = match self.token()?.kind {
            TokenKind::Enum => self.enum_definition()?,
            TokenKind::Type => self.type_alias_definition()?,
            _ => return Err(err!(self, ExpectedIdentifier)),
        };

        Ok(TopLevelExpression::TypeDefinition(Box::new(def)))
    }

    /// Parses a single enum type definition, such as:
    ///
    /// ```ignore
    /// enum IpAddrKind {
    ///   V4,
    ///   V6,
    /// }
    /// ```
    fn enum_definition(&mut self) -> Result<TypeDefinition> {
        self.consume(TokenKind::Enum)?;

        let name = match self.identifier() {
            Ok(name) => name,
            Err(_) => return Err(err!(self, ExpectedIdentifier)),
        };

        self.consume(TokenKind::LeftCurly)?;

        let mut cases = Vec::new();
        while !self.peek(TokenKind::RightCurly)? {
            cases.push(self.enum_case_definition()?);

            // If there's no comma on the last line, the enum case is done.
            if self.consume_if(TokenKind::Comma)?.is_none() {
                break;
            }
        }

        self.consume(TokenKind::RightCurly)?;

        Ok(TypeDefinition::Enum(Box::new(EnumDefinition { name, cases })))
    }

    /// Parses a single enum type case, such as `V4` or `V4(String)`.
    fn enum_case_definition(&mut self) -> Result<EnumDefinitionCase> {
        let name = match self.identifier() {
            Ok(name) => name,
            Err(_) => return Err(err!(self, ExpectedIdentifier)),
        };

        let parameters = if self.consume_if(TokenKind::LeftParen)?.is_some() {
            let mut params = Vec::new();

            while !self.peek(TokenKind::RightParen)? {
                params.push(Box::new(self.parse_type()?));

                // If there's no comma on the last line, the enum case parameters are done.
                if self.consume_if(TokenKind::Comma)?.is_none() {
                    break;
                }
            }

            self.consume(TokenKind::RightParen)?;

            params
        } else {
            Vec::new()
        };

        Ok(EnumDefinitionCase { name, parameters })
    }

    /// Parses a single type alias definition, such as:
    ///
    /// ```ignore
    /// type Alias = String | Int
    /// ```
    fn type_alias_definition(&mut self) -> Result<TypeDefinition> {
        self.consume(TokenKind::Type)?;

        let name = match self.identifier() {
            Ok(name) => name,
            Err(_) => return Err(err!(self, ExpectedIdentifier)),
        };

        // Skip equal sign between name and type
        self.consume(TokenKind::Assign)?;

        let definition = self.parse_type()?;

        let alias = AliasDefinition {
            name,
            definition: Box::new(definition),
        };

        Ok(TypeDefinition::Alias(Box::new(alias)))
    }

    fn arguments(&mut self) -> Result<Vec<Expression>> {
        // If no opening parenthesis, no arguments are defined.
        if !self.peek(TokenKind::LeftParen)? {
            return Ok(Vec::new());
        }

        self.consume(TokenKind::LeftParen)?;

        let mut arguments = Vec::new();

        while self.consume_if(TokenKind::RightParen)?.is_none() {
            if !arguments.is_empty() && !self.peek(TokenKind::RightParen)? {
                self.consume(TokenKind::Comma)?;
            }

            arguments.push(self.expression()?);
        }

        Ok(arguments)
    }

    /// Parses some abstract type at the current cursor position.
    fn parse_type(&mut self) -> Result<Type> {
        let token = self.token()?;

        match token.kind {
            TokenKind::Identifier => self.scalar_or_generic_type(),
            TokenKind::LeftBracket => self.array_type(),
            _ => Err(err!(self, UnexpectedType, actual, token.kind.clone())),
        }
    }

    /// Parses either a scalar- or generic-type at the current cursor position.
    fn scalar_or_generic_type(&mut self) -> Result<Type> {
        let name = self.consume(TokenKind::Identifier)?.value.unwrap();

        if self.peek(TokenKind::Less)? {
            self.generic_type_arguments()
        } else {
            Ok(Type::Scalar(Box::new(ScalarType { name })))
        }
    }

    /// Parses an array type at the current cursor position.
    fn array_type(&mut self) -> Result<Type> {
        self.consume(TokenKind::LeftBracket)?;

        let element_type = ArrayType {
            element_type: Box::new(self.parse_type()?),
        };

        self.consume(TokenKind::RightBracket)?;

        Ok(Type::Array(Box::new(element_type)))
    }

    /// Parses a generic type, with zero-or-more type parameters at the current cursor position.
    fn generic_type_arguments(&mut self) -> Result<Type> {
        self.consume(TokenKind::Less)?;

        let mut type_params = Vec::new();

        while self.consume_if(TokenKind::Greater)?.is_none() {
            if !type_params.is_empty() && !self.peek(TokenKind::Greater)? {
                self.consume(TokenKind::Comma)?;
            }

            type_params.push(Box::new(self.parse_type()?));
        }

        let generic_type = GenericType {
            element_types: type_params,
        };

        Ok(Type::Generic(Box::new(generic_type)))
    }

    /// Parses some abstract statement at the current cursor position.
    fn statement(&mut self) -> Result<Statement> {
        match self.token()?.kind {
            TokenKind::Let => self.variable_declaration(),
            TokenKind::Return => self.return_statement(),
            kind => Err(err!(self, InvalidStatement, actual, kind)),
        }
    }

    /// Parses a variable declaration statement at the current cursor position.
    fn variable_declaration(&mut self) -> Result<Statement> {
        self.consume(TokenKind::Let)?;

        let name = self.identifier()?;
        let variable_type = if self.consume_if(TokenKind::Colon)?.is_some() {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.consume(TokenKind::Assign)?;

        let value = self.expression()?;

        let variable = VariableDeclaration {
            name,
            variable_type,
            value,
        };

        Ok(Statement::VariableDeclaration(Box::new(variable)))
    }

    /// Parses a return statement at the current cursor position.
    fn return_statement(&mut self) -> Result<Statement> {
        self.consume(TokenKind::Return)?;

        let value = self.expression()?;
        let statement = Statement::Return(Box::new(Return { value }));

        Ok(statement)
    }

    /// Parses an expression on the current cursor position.
    fn expression(&mut self) -> Result<Expression> {
        self.expression_with_precedence(0)
    }

    /// Parses an expression on the current cursor position, with a minimum precedence.
    fn expression_with_precedence(&mut self, precedence: u8) -> Result<Expression> {
        let mut left = self.prefix_expression()?;

        while precedence < self.token()?.precedence() {
            left = self.infix_expression(left)?;
        }

        Ok(left)
    }

    /// Parses a prefix expression at the current cursor position.
    ///
    /// Prefix expressions are expressions which appear at the start of an expression,
    /// such as literals of prefix operators. In Pratt Parsing, this is also called "Nud" or "Null Denotation".
    fn prefix_expression(&mut self) -> Result<Expression> {
        let kind = self.token()?.kind;

        if kind == TokenKind::LeftParen {
            return Ok(self.nested_expression()?);
        }

        if kind == TokenKind::Identifier {
            return Ok(self.named_expression()?);
        }

        // Handle literal values, such as strings, numbers and booleans
        if kind.is_literal() {
            return Ok(self.literal()?);
        }

        // Handle unary operators, such as `!` and `-`
        if kind.is_unary() {
            return Ok(self.unary()?);
        }

        Err(err!(self, InvalidExpression, actual, kind))
    }

    /// Parses a infix expression at the current cursor position.
    ///
    /// Infix expressions are expressions which appear in the middle of an expression,
    /// such as infix of postfix operators. In Pratt Parsing, this is also called "Led" or "Left Denotation".
    fn infix_expression(&mut self, left: Expression) -> Result<Expression> {
        // If the next token is a '.', it's a chained expression and we should parse it as a member access expression.
        //
        // In essence, this statement is handling cases where:
        //
        //   let c = new Foo().bar()
        //
        // would be parsed as an operator expression, such as `c = Call(New('Foo'), '.', [Call('bar')])`, where
        // it really should be something like `c = Member(New('Foo'), 'bar')`.
        if self.peek(TokenKind::Dot)? {
            return self.member(left);
        }

        let operator = match self.consume_any()? {
            t if t.kind.is_operator() => t,
            t => return Err(err!(self, InvalidExpression, actual, t.kind)),
        };

        let right = self.expression_with_precedence(operator.precedence())?;
        let name: String = operator.into();

        Ok(Call::new(Some(left), name.into(), vec![right]))
    }

    /// Parses an expression on the current cursor position, which is nested within parentheses.
    fn nested_expression(&mut self) -> Result<Expression> {
        self.consume(TokenKind::LeftParen)?;

        let expression = self.expression_with_precedence(0);

        self.consume(TokenKind::RightParen)?;

        expression
    }

    /// Parses an expression on the current cursor position, which is preceded by some identifier.
    fn named_expression(&mut self) -> Result<Expression> {
        let identifier = self.identifier()?;
        let expression = Expression::Identifier(Box::new(identifier.clone()));

        match self.token()?.kind {
            // If the next token is an opening parenthesis, it's a method invocation
            TokenKind::LeftParen => self.call(identifier),

            // If the next token is a dot, it's some form of member access
            TokenKind::Dot => self.member(expression),

            // If the next token is an equal sign, it's an assignment expression
            TokenKind::Assign => self.assignment(expression),

            // If the name stands alone, it's likely a variable reference
            _ => self.variable(identifier),
        }
    }

    /// Parses a call expression on the current cursor position.
    fn call(&mut self, target: Identifier) -> Result<Expression> {
        let arguments = self.arguments()?;
        let call = Call::new(None, target, arguments);

        return Ok(call);
    }

    /// Parses a member expression on the current cursor position, which is preceded by some identifier.
    fn member(&mut self, target: Expression) -> Result<Expression> {
        // Consume the dot token
        self.consume(TokenKind::Dot)?;

        let name = self.consume(TokenKind::Identifier)?.value.unwrap();

        // If the next token is an opening parenthesis, it's a method invocation
        if self.peek(TokenKind::LeftParen)? {
            let arguments = self.arguments()?;
            let call = Call::new(Some(target), name.into(), arguments);

            return Ok(call);
        }

        let expression = Expression::Member(Box::new(Member { callee: target, name }));

        // If there is yet another dot, it's part of a longer expression.
        if self.peek(TokenKind::Dot)? {
            return self.member(expression);
        }

        // Otherwise, return the expression, as-is.
        Ok(expression)
    }

    /// Parses an assignment expression on the current cursor position.
    fn assignment(&mut self, target: Expression) -> Result<Expression> {
        // Consume the equal sign
        self.consume(TokenKind::Assign)?;

        let value = self.expression()?;

        Ok(Expression::Assignment(Box::new(Assignment { target, value })))
    }

    /// Parses a variable reference expression on the current cursor position.
    fn variable(&mut self, target: Identifier) -> Result<Expression> {
        let variable = Variable { name: target };

        Ok(Expression::Variable(Box::new(variable)))
    }

    /// Parses a literal value expression on the current cursor position.
    fn literal(&mut self) -> Result<Expression> {
        let token = self.token()?;

        let literal = match token.kind {
            TokenKind::Integer => {
                let value = token.value.unwrap();
                let int_value = match value.parse::<i64>() {
                    Ok(v) => v,
                    Err(_) => return Err(err!(self, InvalidLiteral, value, value, target, token.kind)),
                };

                Literal::Int(Box::new(IntLiteral { value: int_value }))
            }
            TokenKind::Float => {
                let value = token.value.unwrap();
                let float_value = match value.parse::<f64>() {
                    Ok(v) => v,
                    Err(_) => return Err(err!(self, InvalidLiteral, value, value, target, token.kind)),
                };

                Literal::Float(Box::new(FloatLiteral { value: float_value }))
            }
            TokenKind::String => {
                let value = token.value.unwrap();

                Literal::String(Box::new(StringLiteral { value }))
            }
            TokenKind::True => Literal::Boolean(Box::new(BooleanLiteral { value: true })),
            TokenKind::False => Literal::Boolean(Box::new(BooleanLiteral { value: false })),
            k => return Err(err!(self, Unimplemented, desc, format!("{:?} literals", k))),
        };

        self.skip()?;

        Ok(Expression::Literal(Box::new(literal)))
    }

    /// Parses a unary expression on the current cursor position.
    fn unary(&mut self) -> Result<Expression> {
        let operator = match self.consume_any()? {
            t if t.kind.is_unary() => t,
            t => return Err(err!(self, InvalidExpression, actual, t.kind)),
        };

        let right = self.expression_with_precedence(UNARY_PRECEDENCE)?;

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
}
