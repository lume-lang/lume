use error_snippet::Result;
use lume_ast::*;
use lume_lexer::{Token, TokenKind, TokenType, UNARY_PRECEDENCE};

use crate::Parser;
use crate::errors::*;

impl Parser<'_> {
    /// Parses an expression on the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    pub(super) fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_expression_with_precedence(0)
    }

    /// Parses an expression on the current cursor position, if one is defined.
    #[libftrace::traced(level = Trace, err)]
    pub(super) fn parse_opt_expression(&mut self) -> Result<Option<Expression>> {
        if self.peek(TokenType::Semicolon) {
            Ok(None)
        } else {
            Ok(Some(self.parse_expression()?))
        }
    }

    /// Parses an expression on the current cursor position, with a minimum
    /// precedence.
    #[libftrace::traced(level = Trace, err)]
    fn parse_expression_with_precedence(&mut self, precedence: u8) -> Result<Expression> {
        let mut left = self.parse_prefix_expression()?;

        // If a semicolon is present, consider the expression finished.
        if self.peek(TokenType::Semicolon) {
            return Ok(left);
        }

        while precedence < self.token().precedence() {
            left = self.parse_following_expression(left)?;
        }

        Ok(left)
    }

    /// Parses a prefix expression at the current cursor position.
    ///
    /// Prefix expressions are expressions which appear at the start of an
    /// expression, such as literals of prefix operators. In Pratt Parsing,
    /// this is also called "Nud" or "Null Denotation".
    #[libftrace::traced(level = Trace, err)]
    fn parse_prefix_expression(&mut self) -> Result<Expression> {
        let kind = self.token().kind;

        libftrace::trace!("expression kind: {}", kind.as_type());

        match kind {
            TokenKind::LeftParen => self.parse_nested_expression(),
            TokenKind::LeftBracket => self.parse_array_expression(),
            TokenKind::LeftCurly => self.parse_scope_expression(),
            TokenKind::If => self.parse_if_conditional(),
            TokenKind::Switch => self.parse_switch_expression(),
            TokenKind::Identifier(_) | TokenKind::SelfRef => self.parse_named_expression(),

            k if k.is_literal() => self.parse_literal(),
            k if k.is_unary() => self.parse_unary(),

            k => Err(InvalidExpression {
                source: self.source.clone(),
                range: self.token().index,
                actual: k.as_type(),
            }
            .into()),
        }
    }

    /// Parses an expression at the current cursor position, which is followed
    /// by some other expression.
    #[libftrace::traced(level = Trace, err)]
    fn parse_following_expression(&mut self, left: Expression) -> Result<Expression> {
        // If the expression is followed by two dots ('..'), it's a range expression.
        if self.peek(TokenType::DotDot) {
            libftrace::trace!("member expr is range");

            return self.parse_range_expression(left);
        }

        // If the next token is a '.', it's a chained expression and we should parse it
        // as a member access expression.
        //
        // In essence, this statement is handling cases where:
        //
        //   let c = new Foo().bar()
        //
        // would be parsed as an operator expression, such as `c = Call(New('Foo'), '.',
        // [Call('bar')])`, where it really should be something like `c =
        // Member(New('Foo'), 'bar')`.
        if self.peek(TokenType::Dot) {
            libftrace::trace!("expression is member");

            return self.parse_member(left);
        }

        if self.peek(TokenType::Assign) {
            libftrace::trace!("expression is assignment");

            return self.parse_assignment(left);
        }

        if self.peek(TokenType::As) {
            libftrace::trace!("expression is cast");

            return self.parse_cast(left);
        }

        if self.peek(TokenType::Is) {
            libftrace::trace!("expression is instance checking");

            return self.parse_is(left);
        }

        let operator = self.consume_any();

        if operator.is_postfix() {
            Ok(self.parse_postfix_expression(left, operator))
        } else if operator.is_infix() {
            self.parse_infix_expression(left, operator)
        } else {
            Err(InvalidExpression {
                source: self.source.clone(),
                range: self.token().index,
                actual: operator.kind.as_type(),
            }
            .into())
        }
    }

    /// Parses a infix expression at the current cursor position.
    ///
    /// Infix expressions are expressions which appear in the middle of an
    /// expression, such as arithmetic operators, binary operators, etc. In
    /// Pratt Parsing, this is also called "Led" or "Left Denotation".
    #[libftrace::traced(level = Trace, err)]
    fn parse_infix_expression(&mut self, lhs: Expression, operator: Token) -> Result<Expression> {
        let rhs = self.parse_expression_with_precedence(operator.precedence())?;

        let start = lhs.location().start();
        let end = rhs.location().end();

        let intrinsic = match operator.kind {
            // Binary operators
            TokenKind::BinaryAnd => IntrinsicKind::BinaryAnd {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            TokenKind::BinaryOr => IntrinsicKind::BinaryOr {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            TokenKind::BinaryXor => IntrinsicKind::BinaryXor {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },

            // Arithmetic intrinsics
            TokenKind::Add => IntrinsicKind::Add {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            TokenKind::Sub => IntrinsicKind::Sub {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            TokenKind::Mul => IntrinsicKind::Mul {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            TokenKind::Div => IntrinsicKind::Div {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            TokenKind::And => IntrinsicKind::And {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            TokenKind::Or => IntrinsicKind::Or {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },

            // Comparison operators
            TokenKind::Equal => IntrinsicKind::Equal {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            TokenKind::NotEqual => IntrinsicKind::NotEqual {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            TokenKind::Less => IntrinsicKind::Less {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            TokenKind::LessEqual => IntrinsicKind::LessEqual {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            TokenKind::Greater => IntrinsicKind::Greater {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            TokenKind::GreaterEqual => IntrinsicKind::GreaterEqual {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            _ => unimplemented!("bug!: unsupported or unimplemented operation: {}", operator.as_type()),
        };

        Ok(Expression::IntrinsicCall(Box::new(IntrinsicCall {
            kind: intrinsic,
            location: (start..end).into(),
        })))
    }

    /// Parses a postfix expression at the current cursor position.
    ///
    /// Postfix expressions are expressions which appear at the end of an
    /// expression, such as increment or decrement operators.
    #[expect(clippy::unused_self, reason = "this is more consistent")]
    #[libftrace::traced(level = Trace, fields(operator))]
    fn parse_postfix_expression(&self, target: Expression, operator: Token) -> Expression {
        let start = target.location().start();
        let end = operator.end();

        let intrinsic = match operator.kind {
            TokenKind::Increment => IntrinsicKind::Increment {
                target: Box::new(target),
            },
            TokenKind::Decrement => IntrinsicKind::Decrement {
                target: Box::new(target),
            },
            _ => unreachable!(),
        };

        Expression::IntrinsicCall(Box::new(IntrinsicCall {
            kind: intrinsic,
            location: (start..end).into(),
        }))
    }

    /// Parses an expression on the current cursor position, which is nested
    /// within parentheses.
    #[libftrace::traced(level = Trace, err)]
    fn parse_nested_expression(&mut self) -> Result<Expression> {
        self.consume(TokenType::LeftParen)?;

        let expression = self.parse_expression_with_precedence(0)?;

        self.consume(TokenType::RightParen)?;

        Ok(expression)
    }

    /// Parses an array expression on the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_array_expression(&mut self) -> Result<Expression> {
        let (values, location) = self.consume_with_loc(|parser| {
            parser.consume_comma_seq(TokenType::LeftBracket, TokenType::RightBracket, |p| {
                p.parse_expression()
            })
        })?;

        Ok(Expression::Array(Box::new(Array { values, location })))
    }

    /// Parses a scope expression on the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_scope_expression(&mut self) -> Result<Expression> {
        let (body, location) = self.consume_with_loc(|p| p.consume_curly_seq(Parser::parse_statement))?;

        Ok(Expression::Scope(Box::new(Scope { body, location })))
    }

    /// Parses a switch expression on the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    pub(super) fn parse_switch_expression(&mut self) -> Result<Expression> {
        let start = self.consume(TokenType::Switch)?.start();
        let operand = self.parse_expression()?;

        let (cases, loc) = self.consume_with_loc(|p| {
            p.consume_comma_seq(TokenType::LeftCurly, TokenType::RightCurly, Parser::parse_switch_case)
        })?;

        let end = loc.end();

        Ok(Expression::Switch(Box::new(Switch {
            operand,
            cases,
            location: (start..end).into(),
        })))
    }

    /// Parses a switch case on the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_switch_case(&mut self) -> Result<SwitchCase> {
        let pattern = self.parse_pattern()?;

        self.consume(TokenType::ArrowBig)?;

        let branch = self.parse_expression()?;

        let start = pattern.location().start();
        let end = branch.location().end();

        Ok(SwitchCase {
            pattern,
            branch,
            location: (start..end).into(),
        })
    }

    /// Parses a range expression on the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_range_expression(&mut self, lower: Expression) -> Result<Expression> {
        self.consume(TokenType::DotDot)?;

        let inclusive = self.check(TokenType::Assign);
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

    /// Parses an expression on the current cursor position, which is preceded
    /// by some identifier.
    #[libftrace::traced(level = Trace, err)]
    fn parse_named_expression(&mut self) -> Result<Expression> {
        let mut identifier = self.parse_identifier()?;

        match self.token().kind {
            // If the next token is an opening parenthesis, it's a method invocation
            TokenKind::LeftParen => self.parse_call(None, identifier),

            // If the next token is a question mark, it's a method invocation
            TokenKind::Question => {
                let tok = self.consume(TokenType::Question)?;

                identifier.name.push_str(&tok.kind.as_type().to_string());
                identifier.location.0.end += tok.len();

                self.parse_call(None, identifier)
            }

            // If the next token is a dot, it's some form of member access
            TokenKind::Dot => self.parse_member(identifier.as_var()),

            // If the next token is an equal sign, it's an assignment expression
            TokenKind::Assign => self.parse_assignment(identifier.as_var()),

            // If the identifier is following by a separator, it's refering to a namespaced identifier
            TokenKind::PathSeparator => self.parse_path_expression(identifier),

            // If the identifier is followed by a left curly brace, it's a struct construction.
            // We only assume this if the identifier is actually camel-case, as it might otherwise not
            // refer to a type.
            TokenKind::LeftCurly if !identifier.is_lower() => self.parse_construction_expression(identifier),

            // If the identifier is following by a lesser sign, it might be referring to a generic call expression
            TokenKind::Less => {
                let current_idx = self.index;

                // If we fail to parse the type arguments, move back to the original position
                // and continue parsing at the fallthrough case.
                let has_type_args = self.parse_type_arguments().is_ok();
                let next_token = self.token().kind.as_type();

                self.move_to(current_idx);

                match (has_type_args, next_token) {
                    (_, TokenType::LeftParen) => self.parse_call(None, identifier),
                    (_, TokenType::LeftCurly) => self.parse_construction_expression(identifier),
                    (true, _) => self.parse_path_expression(identifier),
                    (false, _) => Ok(identifier.as_var()),
                }
            }

            // If the identifier is all upper case, we'll parse it as a const variable reference
            _ if identifier.is_all_upper() => Ok(identifier.as_var()),

            // If the identifier is not lower case, it might be a path expression
            _ if !identifier.is_lower() => self.parse_path_expression(identifier),

            // If the name stands alone, it's likely a variable reference
            _ => Ok(identifier.as_var()),
        }
    }

    /// Parses a call expression on the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_call(&mut self, callee: Option<Expression>, name: Identifier) -> Result<Expression> {
        let bound_types = self.parse_type_arguments()?;
        let bound_types_end = self.previous_token().end();

        let arguments = self.parse_call_arguments()?;

        let start = name.location.start();
        let end = self.previous_token().end();

        let call = Call {
            callee,
            name: Path::rooted(PathSegment::Callable {
                name,
                bound_types,
                location: (start..bound_types_end).into(),
            }),
            arguments,
            location: (start..end).into(),
        };

        Ok(Expression::Call(Box::new(call)))
    }

    /// Parses zero-or-more call arguments at the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>> {
        self.consume_paren_seq(Parser::parse_expression)
    }

    /// Parses an "if" conditional statement at the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_if_conditional(&mut self) -> Result<Expression> {
        let start = self.consume(TokenType::If)?.start();
        let mut cases = Vec::new();

        // Append the primary case
        self.parse_conditional_case(&mut cases)?;

        // Append the `else if` case
        self.parse_else_if_conditional_cases(&mut cases)?;

        // Append the `else` case
        self.parse_else_conditional_case(&mut cases)?;

        let end = cases.last().unwrap().location.end();

        let conditional = IfCondition {
            cases,
            location: (start..end).into(),
        };

        Ok(Expression::If(Box::new(conditional)))
    }

    /// Parses a case within a conditional expression at the current cursor
    /// position.
    #[libftrace::traced(level = Trace, err)]
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

    /// Parses zero-or-more `else-if` cases within a conditional expression at
    /// the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_else_if_conditional_cases(&mut self, cases: &mut Vec<Condition>) -> Result<()> {
        loop {
            if !self.peek(TokenType::Else) || !self.peek_next(TokenType::If) {
                break;
            }

            self.consume(TokenType::Else)?;
            self.consume(TokenType::If)?;

            self.parse_conditional_case(cases)?;
        }

        Ok(())
    }

    /// Parses zero-or-one `else` cases within a conditional expression at the
    /// current cursor position.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the parser hits an unexpected token.
    #[libftrace::traced(level = Trace, err)]
    pub fn parse_else_conditional_case(&mut self, cases: &mut Vec<Condition>) -> Result<()> {
        let start = match self.consume_if(TokenType::Else) {
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

    /// Parses a member expression on the current cursor position, which is
    /// preceded by some identifier.
    #[libftrace::traced(level = Trace, err)]
    fn parse_member(&mut self, target: Expression) -> Result<Expression> {
        // Consume the dot token
        self.consume(TokenType::Dot)?;

        let name = self.parse_callable_name()?;

        let is_method_call = self.peek(TokenType::LeftParen);
        let is_generic_method_call = if self.peek(TokenType::Less)
            && let TokenKind::Identifier(ident) = self.next_token().kind
            && ident.starts_with(|c: char| c.is_ascii_uppercase())
        {
            true
        } else {
            false
        };

        // If the next token is an opening parenthesis, it's a method invocation
        if is_method_call || is_generic_method_call {
            libftrace::trace!("member expr is method invocation");

            let identifier = Identifier {
                name: name.name,
                location: name.location,
            };

            return self.parse_call(Some(target), identifier);
        }

        let start = target.location().start();
        let end = name.location.end();

        let expression = Expression::Member(Box::new(Member {
            callee: target,
            name,
            location: (start..end).into(),
        }));

        // If there is yet another dot, it's part of a longer expression.
        if self.peek(TokenType::Dot) {
            libftrace::trace!("member expr is being chained");

            return self.parse_member(expression);
        }

        // Otherwise, return the expression, as-is.
        Ok(expression)
    }

    /// Parses a cast expression on the current cursor position, which is
    /// preceded by some identifier.
    #[libftrace::traced(level = Trace, err)]
    fn parse_cast(&mut self, source: Expression) -> Result<Expression> {
        // Consume the `as` token
        self.consume(TokenType::As)?;

        let ty = self.parse_type()?;

        let start = source.location().start();
        let end = ty.location().end();

        Ok(Expression::Cast(Box::new(Cast {
            source,
            target_type: ty,
            location: (start..end).into(),
        })))
    }

    /// Parses an instance checking expression on the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_is(&mut self, target: Expression) -> Result<Expression> {
        // Consume the `is` token
        self.consume(TokenType::Is)?;

        let pattern = self.parse_pattern()?;

        let start = target.location().start();
        let end = self.token().end();

        Ok(Expression::Is(Box::new(Is {
            target,
            pattern,
            location: (start..end).into(),
        })))
    }

    /// Parses an assignment expression on the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_assignment(&mut self, target: Expression) -> Result<Expression> {
        // Consume the equal sign
        self.consume(TokenType::Assign)?;

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
    #[libftrace::traced(level = Trace, err)]
    fn parse_path_expression(&mut self, name: Identifier) -> Result<Expression> {
        // Move the cursor back to the start of the given identifier.
        self.move_to_pos(name.location.start());

        let path = self.parse_path()?;

        let subtyped = path
            .root
            .last()
            .is_some_and(|seg| matches!(seg, PathSegment::Type { .. }));

        match &path.name {
            PathSegment::Namespace { name } => Err(crate::errors::ExpectedValueNamespace {
                source: self.source.clone(),
                range: name.location.0.clone(),
                actual: name.to_string(),
            }
            .into()),
            PathSegment::Type { .. } if subtyped => Ok(Expression::Variant(Box::new(Variant {
                location: path.location.clone(),
                name: path,
                arguments: Vec::new(),
            }))),
            PathSegment::Type { location, .. } => Err(crate::errors::ExpectedValueType {
                source: self.source.clone(),
                range: location.0.clone(),
                actual: path.name.to_string(),
            }
            .into()),
            PathSegment::Callable { location, .. } => {
                let arguments = self.parse_call_arguments()?;

                Ok(Expression::Call(Box::new(Call {
                    location: (location.start()..location.end()).into(),
                    callee: None,
                    name: path,
                    arguments,
                })))
            }
            PathSegment::Variant { location, .. } => {
                let arguments = if self.peek(TokenType::LeftParen) {
                    self.parse_call_arguments()?
                } else {
                    Vec::new()
                };

                Ok(Expression::Variant(Box::new(Variant {
                    location: (location.start()..location.end()).into(),
                    name: path,
                    arguments,
                })))
            }
        }
    }

    /// Parses a struct construction expression on the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_construction_expression(&mut self, name: Identifier) -> Result<Expression> {
        // Move the cursor back to the start of the given identifier.
        self.move_to_pos(name.location.start());

        let path = self.parse_path()?;
        let fields = self.consume_comma_seq(TokenType::LeftCurly, TokenType::RightCurly, Parser::parse_field)?;

        let start = name.location().start();
        let end = self.token().end();
        let location = (start..end).into();

        Ok(Expression::Construct(Box::new(Construct { location, path, fields })))
    }

    /// Parses a field expression on the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_field(&mut self) -> Result<ConstructorField> {
        let name = self.parse_identifier()?;

        let value = if self.check(TokenType::Colon) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let start = name.location().start();
        let end = self.previous_token().end();
        let location = (start..end).into();

        Ok(ConstructorField { name, value, location })
    }

    /// Parses a literal value expression on the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    pub(super) fn parse_literal(&mut self) -> Result<Expression> {
        let literal = self.parse_literal_inner()?;

        Ok(Expression::Literal(Box::new(literal)))
    }

    /// Parses a literal value expression on the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    pub(super) fn parse_literal_inner(&mut self) -> Result<Literal> {
        let token = self.token();
        let location: Location = token.index.into();

        let literal = match token.kind {
            TokenKind::Integer((radix, kind)) => {
                let slice = self.source.content.get(location.0.clone()).unwrap();
                let mut value = slice.to_string();

                // Remove all underscores from the literal
                while let Some(idx) = value.find('_') {
                    value.remove(idx);
                }

                // Remove radix prefixes, as `from_str_radix` does not support them
                // being included.
                if value.len() >= 2 && value.starts_with('0') {
                    let c = value.as_bytes()[1] as char;

                    if matches!(c.to_ascii_lowercase(), 'b' | 'o' | 'd' | 'x') {
                        let mut value_chars = value.chars();

                        value_chars.next();
                        value_chars.next();

                        value = value_chars.collect();
                    }
                }

                // Remove the literal kind from the value, since they cannot be parsed.
                match kind {
                    Some(lume_lexer::IntegerKind::I8 | lume_lexer::IntegerKind::U8) => value.truncate(value.len() - 2),
                    Some(_) => value.truncate(value.len() - 3),
                    None => {}
                }

                let Ok(int_value) = i64::from_str_radix(&value, radix.base()) else {
                    return Err(InvalidLiteral {
                        source: self.source.clone(),
                        range: self.token().index,
                        value: slice.to_string(),
                        target: token.kind.as_type(),
                    }
                    .into());
                };

                let kind = match kind {
                    Some(lume_lexer::IntegerKind::I8) => Some(IntKind::I8),
                    Some(lume_lexer::IntegerKind::U8) => Some(IntKind::U8),
                    Some(lume_lexer::IntegerKind::I16) => Some(IntKind::I16),
                    Some(lume_lexer::IntegerKind::U16) => Some(IntKind::U16),
                    Some(lume_lexer::IntegerKind::I32) => Some(IntKind::I32),
                    Some(lume_lexer::IntegerKind::U32) => Some(IntKind::U32),
                    Some(lume_lexer::IntegerKind::I64) => Some(IntKind::I64),
                    Some(lume_lexer::IntegerKind::U64) => Some(IntKind::U64),
                    None => None,
                };

                Literal::Int(Box::new(IntLiteral {
                    value: int_value,
                    location,
                    kind,
                }))
            }
            TokenKind::Float(kind) => {
                let slice = self.source.content.get(location.0.clone()).unwrap();
                let mut value = slice.to_string();

                // Remove all underscores from the literal
                while let Some(idx) = value.find('_') {
                    value.remove(idx);
                }

                // Remove the literal kind from the value, since they cannot be parsed.
                if kind.is_some() {
                    value.truncate(value.len() - 3);
                }

                let Ok(float_value) = value.parse::<f64>() else {
                    return Err(InvalidLiteral {
                        source: self.source.clone(),
                        range: self.token().index,
                        value: slice.to_string(),
                        target: token.kind.as_type(),
                    }
                    .into());
                };

                let kind = match kind {
                    Some(lume_lexer::FloatKind::F32) => Some(FloatKind::F32),
                    Some(lume_lexer::FloatKind::F64) => Some(FloatKind::F64),
                    None => None,
                };

                Literal::Float(Box::new(FloatLiteral {
                    value: float_value,
                    location,
                    kind,
                }))
            }
            TokenKind::String(value) => Literal::String(Box::new(StringLiteral {
                value: value.to_string(),
                location,
            })),
            TokenKind::True => Literal::Boolean(Box::new(BooleanLiteral { value: true, location })),
            TokenKind::False => Literal::Boolean(Box::new(BooleanLiteral { value: false, location })),
            k => {
                return Err(Unimplemented {
                    source: self.source.clone(),
                    range: self.token().index,
                    desc: format!("{} literals", k.as_type()),
                }
                .into());
            }
        };

        self.skip();

        Ok(literal)
    }

    /// Parses a unary expression on the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_unary(&mut self) -> Result<Expression> {
        let operator = match self.consume_any() {
            t if t.kind.is_unary() => t,
            t => {
                return Err(InvalidExpression {
                    source: self.source.clone(),
                    range: self.token().index,
                    actual: t.kind.as_type(),
                }
                .into());
            }
        };

        let target = self.parse_expression_with_precedence(UNARY_PRECEDENCE)?;

        let start = operator.start();
        let end = target.location().end();

        let intrinsic = match operator.kind {
            TokenKind::Exclamation => IntrinsicKind::Not {
                target: Box::new(target),
            },
            TokenKind::Sub => IntrinsicKind::Negate {
                target: Box::new(target),
            },
            _ => unreachable!(),
        };

        Ok(Expression::IntrinsicCall(Box::new(IntrinsicCall {
            kind: intrinsic,
            location: (start..end).into(),
        })))
    }

    /// Parses some expression, if an equal sign is consumed. Otherwise, returns
    /// `None`.
    #[libftrace::traced(level = Trace, err)]
    pub(super) fn parse_opt_assignment(&mut self) -> Result<Option<Expression>> {
        if self.check(TokenType::Assign) {
            Ok(Some(self.parse_expression()?))
        } else {
            Ok(None)
        }
    }
}
