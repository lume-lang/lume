use error_snippet::Result;
use lume_ast::*;
use lume_lexer::{IDENTIFIER_SEPARATOR, Token, TokenKind, UNARY_PRECEDENCE};

use crate::{Parser, err, errors::*};

impl Parser {
    /// Parses an expression on the current cursor position.
    pub(super) fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_expression_with_precedence(0)
    }

    /// Parses an expression on the current cursor position, if one is defined.
    pub(super) fn parse_opt_expression(&mut self) -> Result<Option<Expression>> {
        if self.peek(TokenKind::Semicolon)? {
            Ok(None)
        } else {
            Ok(Some(self.parse_expression()?))
        }
    }

    /// Parses an expression on the current cursor position, with a minimum precedence.
    fn parse_expression_with_precedence(&mut self, precedence: u8) -> Result<Expression> {
        let mut left = self.parse_prefix_expression()?;

        // If a semicolon is present, consider the expression finished.
        if self.peek(TokenKind::Semicolon)? {
            return Ok(left);
        }

        while precedence < self.token()?.precedence() {
            left = self.parse_following_expression(left)?;
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
            TokenKind::SelfRef => Ok(self.parse_self_reference()?),
            TokenKind::Identifier => Ok(self.parse_named_expression()?),

            k if k.is_literal() => Ok(self.parse_literal()?),
            k if k.is_unary() => Ok(self.parse_unary()?),

            _ => Err(err!(self, InvalidExpression, actual, kind)),
        }
    }

    /// Parses an expression at the current cursor position, which is followed by some other
    /// expression.
    fn parse_following_expression(&mut self, left: Expression) -> Result<Expression> {
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

        if operator.is_postfix() {
            self.parse_postfix_expression(left, operator)
        } else {
            self.parse_infix_expression(left, operator)
        }
    }

    /// Parses a infix expression at the current cursor position.
    ///
    /// Infix expressions are expressions which appear in the middle of an expression,
    /// such as infix of postfix operators. In Pratt Parsing, this is also called "Led" or "Left Denotation".
    /// such as increment or decrement operators.
    fn parse_infix_expression(&mut self, left: Expression, operator: Token) -> Result<Expression> {
        let operator_loc = operator.index.clone();

        let right = self.parse_expression_with_precedence(operator.precedence())?;
        let name: String = operator.into();

        let start = left.location().start();
        let end = right.location().start();

        Ok(Expression::Call(Box::new(Call {
            callee: Some(left),
            name: Path {
                name: Identifier {
                    name,
                    location: operator_loc.clone().into(),
                },
                root: NamespacePath::empty(),
                location: operator_loc.into(),
            },
            arguments: vec![right],
            type_arguments: vec![],
            location: (start..end).into(),
        })))
    }

    /// Parses a postfix expression at the current cursor position.
    ///
    /// Postfix expressions are expressions which appear at the end of an expression,
    /// such as increment or decrement operators.
    fn parse_postfix_expression(&mut self, left: Expression, operator: Token) -> Result<Expression> {
        let operator_loc = operator.index.clone();

        let start = left.location().start();
        let end = operator.end();

        Ok(Expression::Call(Box::new(Call {
            callee: Some(left),
            name: Path {
                name: Identifier {
                    name: operator.into(),
                    location: operator_loc.clone().into(),
                },
                root: NamespacePath::empty(),
                location: operator_loc.into(),
            },
            arguments: vec![],
            type_arguments: vec![],
            location: (start..end).into(),
        })))
    }

    /// Parses an expression on the current cursor position, which is nested within parentheses.
    fn parse_nested_expression(&mut self) -> Result<Expression> {
        self.consume(TokenKind::LeftParen)?;

        let expression = self.parse_expression_with_precedence(0)?;

        // If the expression is followed by two dots ('..'), it's a range expression.
        if self.peek(TokenKind::Dot)? && self.peek_next(TokenKind::Dot)? {
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

    /// Parses a `self` reference expression on the current cursor position.
    fn parse_self_reference(&mut self) -> Result<Expression> {
        let location = self.consume(TokenKind::SelfRef)?.index;
        let identifier = Identifier {
            name: "self".to_string(),
            location: location.into(),
        };

        if self.peek(IDENTIFIER_SEPARATOR)? {
            return self.parse_path_expression(identifier);
        }

        Ok(Expression::Variable(Box::new(Variable { name: identifier })))
    }

    /// Parses an expression on the current cursor position, which is preceded by some identifier.
    fn parse_named_expression(&mut self) -> Result<Expression> {
        let identifier = self.parse_identifier()?;
        let expression = Expression::Variable(Box::new(Variable {
            name: identifier.clone(),
        }));

        match self.token()?.kind {
            // If the next token is an opening parenthesis, it's a method invocation
            TokenKind::LeftParen => self.parse_call(None, identifier),

            // If the next token is a dot, it's some form of member access
            TokenKind::Dot => self.parse_member(expression),

            // If the next token is an equal sign, it's an assignment expression
            TokenKind::Assign => self.parse_assignment(expression),

            // If the identifier is following by a separator, it's refering to a namespaced identifier
            IDENTIFIER_SEPARATOR => self.parse_path_expression(identifier),

            // If the name stands alone, it's likely a variable reference
            _ => self.parse_variable(identifier),
        }
    }

    /// Parses a call expression on the current cursor position.
    fn parse_call(&mut self, callee: Option<Expression>, name: impl Into<Path>) -> Result<Expression> {
        let name = name.into();

        let type_arguments = self.parse_type_arguments()?;
        let arguments = self.parse_call_arguments()?;

        let start = name.location.start();
        let end = self.token()?.end();

        let call = Call {
            callee,
            name,
            arguments,
            type_arguments,
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
        if self.peek(TokenKind::Dot)? && self.peek_next(TokenKind::Dot)? {
            return self.parse_range_expression(target);
        }

        // Consume the dot token
        self.consume(TokenKind::Dot)?;

        let name = self.consume(TokenKind::Identifier)?;

        // If the next token is an opening parenthesis, it's a method invocation
        if self.peek(TokenKind::LeftParen)? || self.consume_if(IDENTIFIER_SEPARATOR)?.is_some() {
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

    /// Parses a path expression on the current cursor position.
    fn parse_path_expression(&mut self, name: Identifier) -> Result<Expression> {
        self.consume(IDENTIFIER_SEPARATOR)?;

        if self.peek(TokenKind::Less)? {
            return self.parse_call(None, name);
        }

        let path = Path::rooted(name);
        let mut expression = self.parse_expression()?;

        Self::merge_expression_with_path(&mut expression, path);

        Ok(expression)
    }

    /// Merges a path with an expression.
    fn merge_expression_with_path(expr: &mut Expression, path: Path) {
        match expr {
            Expression::Call(call) => {
                call.name.merge(path);
            }
            Expression::Member(member) => {
                Self::merge_expression_with_path(&mut member.callee, path);
            }
            _ => {}
        }
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
    pub(super) fn parse_opt_assignment(&mut self) -> Result<Option<Expression>> {
        if self.consume_if(TokenKind::Assign)?.is_none() {
            Ok(None)
        } else {
            Ok(Some(self.parse_expression()?))
        }
    }
}
