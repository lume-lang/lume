use error_snippet::Result;
use lume_ast::*;
use lume_lexer::{IDENTIFIER_SEPARATOR, Token, TokenKind, UNARY_PRECEDENCE};

use crate::{Parser, err, errors::*};

impl Parser {
    /// Parses an expression on the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub(super) fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_expression_with_precedence(0)
    }

    /// Parses an expression on the current cursor position, if one is defined.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub(super) fn parse_opt_expression(&mut self) -> Result<Option<Expression>> {
        if self.peek(TokenKind::Semicolon) {
            Ok(None)
        } else {
            Ok(Some(self.parse_expression()?))
        }
    }

    /// Parses an expression on the current cursor position, with a minimum precedence.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_expression_with_precedence(&mut self, precedence: u8) -> Result<Expression> {
        let mut left = self.parse_prefix_expression()?;

        // If a semicolon is present, consider the expression finished.
        if self.peek(TokenKind::Semicolon) {
            return Ok(left);
        }

        while precedence < self.token().precedence() {
            left = self.parse_following_expression(left)?;
        }

        Ok(left)
    }

    /// Parses a prefix expression at the current cursor position.
    ///
    /// Prefix expressions are expressions which appear at the start of an expression,
    /// such as literals of prefix operators. In Pratt Parsing, this is also called "Nud" or "Null Denotation".
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_prefix_expression(&mut self) -> Result<Expression> {
        let kind = self.token().kind;

        tracing::trace!("expression kind: {kind}");

        match kind {
            TokenKind::LeftParen => self.parse_nested_expression(),
            TokenKind::LeftBracket => self.parse_array_expression(),
            TokenKind::LeftCurly => self.parse_scope_expression(),
            TokenKind::If => self.parse_if_conditional(),
            TokenKind::Switch => self.parse_switch_expression(),
            TokenKind::SelfRef => self.parse_self_reference(),
            TokenKind::Identifier => self.parse_named_expression(),

            k if k.is_literal() => self.parse_literal(),
            k if k.is_unary() => self.parse_unary(),

            _ => Err(err!(self, InvalidExpression, actual, kind)),
        }
    }

    /// Parses an expression at the current cursor position, which is followed by some other
    /// expression.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_following_expression(&mut self, left: Expression) -> Result<Expression> {
        // If the expression is followed by two dots ('..'), it's a range expression.
        if self.peek(TokenKind::DotDot) {
            tracing::trace!("member expr is range");

            return self.parse_range_expression(left);
        }

        // If the next token is a '.', it's a chained expression and we should parse it as a member access expression.
        //
        // In essence, this statement is handling cases where:
        //
        //   let c = new Foo().bar()
        //
        // would be parsed as an operator expression, such as `c = Call(New('Foo'), '.', [Call('bar')])`, where
        // it really should be something like `c = Member(New('Foo'), 'bar')`.
        if self.peek(TokenKind::Dot) {
            tracing::trace!("expression is member");

            return self.parse_member(left);
        }

        if self.peek(TokenKind::Assign) {
            tracing::trace!("expression is assignment");

            return self.parse_assignment(left);
        }

        if self.peek(TokenKind::As) {
            tracing::trace!("expression is cast");

            return self.parse_cast(left);
        }

        let operator = match self.consume_any() {
            t if t.kind.is_operator() => t,
            t => return Err(err!(self, InvalidExpression, actual, t.kind)),
        };

        if operator.is_postfix() {
            Ok(self.parse_postfix_expression(left, operator))
        } else {
            self.parse_infix_expression(left, operator)
        }
    }

    /// Parses a infix expression at the current cursor position.
    ///
    /// Infix expressions are expressions which appear in the middle of an expression,
    /// such as infix of postfix operators. In Pratt Parsing, this is also called "Led" or "Left Denotation".
    /// such as increment or decrement operators.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_infix_expression(&mut self, lhs: Expression, operator: Token) -> Result<Expression> {
        let rhs = self.parse_expression_with_precedence(operator.precedence())?;

        if operator.is_binary() {
            return self.parse_binary_operator_expression(lhs, operator, rhs);
        }

        if operator.is_boolean() {
            return self.parse_boolean_operator_expression(lhs, operator, rhs);
        }

        self.parse_other_operator_expression(lhs, operator, rhs)
    }

    /// Parses a postfix expression at the current cursor position.
    ///
    /// Postfix expressions are expressions which appear at the end of an expression,
    /// such as increment or decrement operators.
    #[tracing::instrument(level = "TRACE", skip(self, left))]
    fn parse_postfix_expression(&mut self, left: Expression, operator: Token) -> Expression {
        let operator_loc = operator.index.clone();

        let start = left.location().start();
        let end = operator.end();

        Expression::IntrinsicCall(Box::new(IntrinsicCall {
            callee: left,
            name: Path {
                name: PathSegment::callable(Identifier {
                    name: operator.into(),
                    location: operator_loc.clone().into(),
                }),
                root: Vec::new(),
                location: operator_loc.into(),
            },
            arguments: vec![],
            location: (start..end).into(),
        }))
    }

    /// Parses the given left-hand- and right-hand-side expressions as a binary
    /// operator expression call.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_binary_operator_expression(
        &mut self,
        lhs: Expression,
        operator: Token,
        rhs: Expression,
    ) -> Result<Expression> {
        debug_assert!(operator.is_binary());

        let operator_kind = match operator.kind {
            TokenKind::BinaryAnd => BinaryOperatorKind::And,
            TokenKind::BinaryOr => BinaryOperatorKind::Or,
            TokenKind::BinaryXor => BinaryOperatorKind::Xor,
            kind => panic!("bug!: invalid binary operator token ({kind:?})"),
        };

        let start = lhs.location().start();
        let end = rhs.location().end();

        Ok(Expression::Binary(Box::new(Binary {
            lhs,
            op: BinaryOperator {
                kind: operator_kind,
                location: operator.index.clone().into(),
            },
            rhs,
            location: (start..end).into(),
        })))
    }

    /// Parses the given left-hand- and right-hand-side expressions as an
    /// operator expression call.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_other_operator_expression(
        &mut self,
        lhs: Expression,
        operator: Token,
        rhs: Expression,
    ) -> Result<Expression> {
        let operator_loc = operator.index.clone();

        let start = lhs.location().start();
        let end = rhs.location().end();

        Ok(Expression::IntrinsicCall(Box::new(IntrinsicCall {
            callee: lhs,
            name: Path {
                name: PathSegment::callable(Identifier {
                    name: operator.into(),
                    location: operator_loc.clone().into(),
                }),
                root: Vec::new(),
                location: operator_loc.into(),
            },
            arguments: vec![rhs],
            location: (start..end).into(),
        })))
    }

    /// Parses the given left-hand- and right-hand-side expressions as a boolean
    /// operator expression call.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_boolean_operator_expression(
        &mut self,
        lhs: Expression,
        operator: Token,
        rhs: Expression,
    ) -> Result<Expression> {
        debug_assert!(operator.is_boolean());

        let operator_kind = match operator.kind {
            TokenKind::And => LogicalOperatorKind::And,
            TokenKind::Or => LogicalOperatorKind::Or,
            kind => panic!("bug!: invalid boolean operator token ({kind:?})"),
        };

        let start = lhs.location().start();
        let end = rhs.location().end();

        Ok(Expression::Logical(Box::new(Logical {
            lhs,
            op: LogicalOperator {
                kind: operator_kind,
                location: operator.index.clone().into(),
            },
            rhs,
            location: (start..end).into(),
        })))
    }

    /// Parses an expression on the current cursor position, which is nested within parentheses.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_nested_expression(&mut self) -> Result<Expression> {
        self.consume(TokenKind::LeftParen)?;

        let expression = self.parse_expression_with_precedence(0)?;

        self.consume(TokenKind::RightParen)?;

        Ok(expression)
    }

    /// Parses an array expression on the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_array_expression(&mut self) -> Result<Expression> {
        let token = self.token();
        let location: Location = token.index.into();

        let values = self.consume_comma_seq(TokenKind::LeftBracket, TokenKind::RightBracket, |p| {
            p.parse_expression()
        })?;

        Ok(Expression::Array(Box::new(Array { values, location })))
    }

    /// Parses a scope expression on the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_scope_expression(&mut self) -> Result<Expression> {
        let (body, location) = self.consume_with_loc(|p| p.consume_curly_seq(Parser::parse_statement))?;

        Ok(Expression::Scope(Box::new(Scope { body, location })))
    }

    /// Parses a switch expression on the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub(super) fn parse_switch_expression(&mut self) -> Result<Expression> {
        let start = self.consume(TokenKind::Switch)?.start();
        let operand = self.parse_expression()?;
        let cases = self.consume_comma_seq(TokenKind::LeftCurly, TokenKind::RightCurly, Parser::parse_switch_case)?;

        let end = self.token().end();

        Ok(Expression::Switch(Box::new(Switch {
            operand,
            cases,
            location: (start..end).into(),
        })))
    }

    /// Parses a switch case on the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_switch_case(&mut self) -> Result<SwitchCase> {
        let pattern = self.parse_pattern()?;

        self.consume(TokenKind::ArrowBig)?;

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
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_range_expression(&mut self, lower: Expression) -> Result<Expression> {
        self.consume(TokenKind::DotDot)?;

        let inclusive = self.check(TokenKind::Assign);
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
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_self_reference(&mut self) -> Result<Expression> {
        let location = self.consume(TokenKind::SelfRef)?.index;
        let identifier = Identifier {
            name: "self".to_string(),
            location: location.into(),
        };

        if self.peek(IDENTIFIER_SEPARATOR) {
            return self.parse_path_expression(identifier);
        }

        Ok(Expression::Variable(Box::new(Variable { name: identifier })))
    }

    /// Parses an expression on the current cursor position, which is preceded by some identifier.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_named_expression(&mut self) -> Result<Expression> {
        let mut identifier = self.parse_identifier()?;

        match self.token().kind {
            // If the next token is an opening parenthesis, it's a method invocation
            TokenKind::LeftParen => self.parse_call(None, identifier),

            // If the next token is a question mark, it's a method invocation
            TokenKind::Question => {
                let tok = self.consume(TokenKind::Question)?;

                identifier.name.push_str(&tok.kind.to_string());
                identifier.location.0.end += tok.len();

                self.parse_call(None, identifier)
            }

            // If the next token is a dot, it's some form of member access
            TokenKind::Dot => self.parse_member(identifier.as_var()),

            // If the next token is an equal sign, it's an assignment expression
            TokenKind::Assign => self.parse_assignment(identifier.as_var()),

            // If the identifier is following by a separator, it's refering to a namespaced identifier
            IDENTIFIER_SEPARATOR => self.parse_path_expression(identifier),

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
                let next_token = self.token().kind;

                self.move_to(current_idx);

                match (has_type_args, next_token) {
                    (_, TokenKind::LeftParen) => self.parse_call(None, identifier),
                    (_, TokenKind::LeftCurly) => self.parse_construction_expression(identifier),
                    (true, _) => self.parse_path_expression(identifier),
                    (false, _) => Ok(self.parse_variable(identifier)),
                }
            }

            // If the identifier is all upper case, we'll parse it as a const variable reference
            _ if identifier.is_all_upper() => Ok(self.parse_variable(identifier)),

            // If the identifier is not lower case, it might be a path expression
            _ if !identifier.is_lower() => self.parse_path_expression(identifier),

            // If the name stands alone, it's likely a variable reference
            _ => Ok(self.parse_variable(identifier)),
        }
    }

    /// Parses a call expression on the current cursor position.
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn parse_call(&mut self, callee: Option<Expression>, name: Identifier) -> Result<Expression> {
        let type_arguments = self.parse_type_arguments()?;
        let type_arguments_end = self.token().end();

        let arguments = self.parse_call_arguments()?;

        let start = name.location.start();
        let end = self.token().end();

        let call = Call {
            callee,
            name: Path::rooted(PathSegment::Callable {
                name,
                type_arguments,
                location: (start..type_arguments_end).into(),
            }),
            arguments,
            location: (start..end).into(),
        };

        Ok(Expression::Call(Box::new(call)))
    }

    /// Parses zero-or-more call arguments at the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>> {
        self.consume_paren_seq(Parser::parse_expression)
    }

    /// Parses an "if" conditional statement at the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_if_conditional(&mut self) -> Result<Expression> {
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
            cases,
            location: (start..end).into(),
        };

        Ok(Expression::If(Box::new(conditional)))
    }

    /// Parses a case within a conditional expression at the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
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

    /// Parses zero-or-more `else-if` cases within a conditional expression at the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_else_if_conditional_cases(&mut self, cases: &mut Vec<Condition>) -> Result<()> {
        loop {
            if !self.peek(TokenKind::Else) || !self.peek_next(TokenKind::If) {
                break;
            }

            self.consume(TokenKind::Else)?;
            self.consume(TokenKind::If)?;

            self.parse_conditional_case(cases)?;
        }

        Ok(())
    }

    /// Parses zero-or-one `else` cases within a conditional expression at the current cursor position.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the parser hits an unexpected token.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn parse_else_conditional_case(&mut self, cases: &mut Vec<Condition>) -> Result<()> {
        let start = match self.consume_if(TokenKind::Else) {
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

    /// Parses a member expression on the current cursor position, which is preceded by some identifier.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_member(&mut self, target: Expression) -> Result<Expression> {
        // Consume the dot token
        self.consume(TokenKind::Dot)?;

        let name = self.parse_callable_name()?;

        // If the next token is an opening parenthesis, it's a method invocation
        if self.peek(TokenKind::LeftParen) || self.peek(TokenKind::Less) {
            tracing::trace!("member expr is method invocation");

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
            name: name.name,
            location: (start..end).into(),
        }));

        // If there is yet another dot, it's part of a longer expression.
        if self.peek(TokenKind::Dot) {
            tracing::trace!("member expr is being chained");

            return self.parse_member(expression);
        }

        // Otherwise, return the expression, as-is.
        Ok(expression)
    }

    /// Parses a cast expression on the current cursor position, which is preceded by some identifier.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_cast(&mut self, source: Expression) -> Result<Expression> {
        // Consume the `as` token
        self.consume(TokenKind::As)?;

        let ty = self.parse_type()?;

        let start = source.location().start();
        let end = ty.location().end();

        Ok(Expression::Cast(Box::new(Cast {
            source,
            target_type: ty,
            location: (start..end).into(),
        })))
    }

    /// Parses an assignment expression on the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
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
    #[tracing::instrument(level = "TRACE", skip(self), err)]
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
                let arguments = if self.peek(TokenKind::LeftParen) {
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
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_construction_expression(&mut self, name: Identifier) -> Result<Expression> {
        // Move the cursor back to the start of the given identifier.
        self.move_to_pos(name.location.start());

        let path = self.parse_path()?;
        let fields = self.consume_comma_seq(TokenKind::LeftCurly, TokenKind::RightCurly, Parser::parse_field)?;

        let start = name.location().start();
        let end = self.token().end();
        let location = (start..end).into();

        Ok(Expression::Construct(Box::new(Construct { location, path, fields })))
    }

    /// Parses a field expression on the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_field(&mut self) -> Result<ConstructorField> {
        let name = self.parse_identifier()?;

        let value = if self.check(TokenKind::Colon) {
            self.parse_expression()?
        } else {
            name.clone().as_var()
        };

        let start = name.location().start();
        let end = value.location().end();
        let location = (start..end).into();

        Ok(ConstructorField { name, value, location })
    }

    /// Parses a variable reference expression on the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn parse_variable(&mut self, target: Identifier) -> Expression {
        let variable = Variable { name: target };

        Expression::Variable(Box::new(variable))
    }

    /// Parses a literal value expression on the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub(super) fn parse_literal(&mut self) -> Result<Expression> {
        let literal = self.parse_literal_inner()?;

        Ok(Expression::Literal(Box::new(literal)))
    }

    /// Parses a literal value expression on the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub(super) fn parse_literal_inner(&mut self) -> Result<Literal> {
        let token = self.token();
        let location: Location = token.index.into();

        let literal = match token.kind {
            TokenKind::Integer(radix) => {
                let mut value = token.value.unwrap();

                // Remove all underscores from the literal
                while let Some(idx) = value.find('"') {
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

                let Ok(int_value) = i64::from_str_radix(&value, radix) else {
                    return Err(err!(self, InvalidLiteral, value, value, target, token.kind));
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
                let Ok(float_value) = value.parse::<f64>() else {
                    return Err(err!(self, InvalidLiteral, value, value, target, token.kind));
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

        self.skip();

        Ok(literal)
    }

    /// Parses a unary expression on the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_unary(&mut self) -> Result<Expression> {
        let operator = match self.consume_any() {
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
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub(super) fn parse_opt_assignment(&mut self) -> Result<Option<Expression>> {
        if self.check(TokenKind::Assign) {
            Ok(Some(self.parse_expression()?))
        } else {
            Ok(None)
        }
    }
}
