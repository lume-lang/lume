use crate::*;

const EXPR_RECOVERY_SET: &[SyntaxKind] = &[Token![;], SyntaxKind::RIGHT_BRACE];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParseBoundTypes {
    Include,
    Ignore,
}

impl Parser {
    /// Parses an expression on the current cursor position.
    pub(super) fn parse_expression(&mut self, c: Option<Checkpoint>) -> Option<SyntaxKind> {
        self.parse_expression_with_precedence(c, 0)
    }

    /// Parses an expression on the current cursor position, if one is defined.
    pub(super) fn parse_opt_expression(&mut self, c: Option<Checkpoint>) -> Option<SyntaxKind> {
        if !self.peek(Token![;]) {
            return self.parse_expression(c);
        }

        None
    }

    /// Parses an expression on the current cursor position, with a minimum
    /// precedence.
    fn parse_expression_with_precedence(&mut self, c: Option<Checkpoint>, precedence: u8) -> Option<SyntaxKind> {
        let c = c.unwrap_or_else(|| self.checkpoint());

        let mut kind = self.parse_prefix_expression(c);

        // If a semicolon is present, consider the expression finished.
        if self.peek(Token![;]) {
            return kind;
        }

        while precedence < self.token().precedence() {
            kind = self.parse_following_expression(c);
        }

        kind
    }

    /// Parses a prefix expression at the current cursor position.
    ///
    /// Prefix expressions are expressions which appear at the start of an
    /// expression, such as literals of prefix operators. In Pratt Parsing,
    /// this is also called "Nud" or "Null Denotation".
    fn parse_prefix_expression(&mut self, c: Checkpoint) -> Option<SyntaxKind> {
        let kind = self.token();

        tracing::trace!("expression kind: {kind:?}");

        let kind = match kind {
            SyntaxKind::LEFT_PAREN => self.parse_nested_expression(),
            SyntaxKind::LEFT_BRACKET => self.parse_array_expression(),
            SyntaxKind::LEFT_BRACE => self.parse_scope_expression(),
            Token![if] => self.parse_if_conditional(),
            Token![switch] => self.parse_switch_expression(),
            SyntaxKind::IDENT | Token![self] => self.parse_named_expression(),

            k if k.is_literal() => self.parse_literal(),
            k if k.is_unary() => self.parse_unary(c),

            _ => {
                self.error_and_skip("invalid error");
                return None;
            }
        };

        Some(kind)
    }

    /// Parses an expression at the current cursor position, which is followed
    /// by some other expression.
    fn parse_following_expression(&mut self, c: Checkpoint) -> Option<SyntaxKind> {
        // If the expression is followed by two dots ('..'), it's a range expression.
        if self.peek(Token![..]) {
            tracing::trace!("member expr is range");

            return Some(self.parse_range_expression(c));
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
        if self.peek(Token![.]) {
            tracing::trace!("expression is member");

            return Some(self.parse_member(c));
        }

        if self.peek(Token![=]) {
            tracing::trace!("expression is assignment");

            return Some(self.parse_assignment(c));
        }

        if self.peek(Token![as]) {
            tracing::trace!("expression is cast");

            return Some(self.parse_cast(c));
        }

        if self.peek(Token![is]) {
            tracing::trace!("expression is instance checking");

            return Some(self.parse_is(c));
        }

        let operator = self.token();

        if self.check_any(lume_syntax::POSTFIX_OPERATORS) {
            Some(self.complete_node(SyntaxKind::POSTFIX_EXPR, c))
        } else if self.check_any(lume_syntax::INFIX_OPERATORS) {
            self.start_node_at(SyntaxKind::BIN_EXPR, c);
            self.parse_expression_with_precedence(None, operator.precedence());
            self.finish_node();

            Some(SyntaxKind::BIN_EXPR)
        } else {
            self.error_and_recover("invalid expression", EXPR_RECOVERY_SET);
            None
        }
    }

    /// Parses an expression on the current cursor position, which is nested
    /// within parentheses.
    fn parse_nested_expression(&mut self) -> SyntaxKind {
        self.start_node(SyntaxKind::PAREN_EXPR);
        self.consume(SyntaxKind::LEFT_PAREN);

        self.parse_expression_with_precedence(None, 0);

        self.consume(SyntaxKind::RIGHT_PAREN);
        self.finish_node();

        SyntaxKind::PAREN_EXPR
    }

    /// Parses an array expression on the current cursor position.
    fn parse_array_expression(&mut self) -> SyntaxKind {
        self.start_node(SyntaxKind::ARRAY_EXPR);

        let finished = self.consume_comma_seq(SyntaxKind::LEFT_BRACKET, SyntaxKind::RIGHT_BRACKET, |parser| {
            parser.parse_expression(None);
        });

        if !finished {
            self.recover_with_set(&[Token![;]]);
        }

        self.finish_node();

        SyntaxKind::ARRAY_EXPR
    }

    /// Parses a scope expression on the current cursor position.
    fn parse_scope_expression(&mut self) -> SyntaxKind {
        self.consume(SyntaxKind::LEFT_BRACE);
        self.start_node(SyntaxKind::STMT_LIST);

        while !self.peek(SyntaxKind::RIGHT_BRACE) {
            self.parse_statement();
        }

        self.finish_node();

        self.consume(SyntaxKind::RIGHT_BRACE);

        SyntaxKind::SCOPE_EXPR
    }

    /// Parses a switch expression on the current cursor position.
    pub(super) fn parse_switch_expression(&mut self) -> SyntaxKind {
        self.start_node(SyntaxKind::SWITCH_EXPR);

        self.consume(Token![switch]);
        self.parse_expression(None);

        let finished = self.consume_comma_seq(SyntaxKind::LEFT_BRACE, SyntaxKind::RIGHT_BRACE, |parser| {
            parser.start_node(SyntaxKind::SWITCH_ARM);

            parser.parse_pattern();
            parser.consume(Token![=>]);
            parser.parse_expression(None);

            parser.finish_node();
        });

        if !finished {
            self.recover_with_set(&[Token![;]]);
        }

        self.finish_node();

        SyntaxKind::SWITCH_EXPR
    }

    /// Parses a range expression on the current cursor position.
    fn parse_range_expression(&mut self, c: Checkpoint) -> SyntaxKind {
        self.start_node_at(SyntaxKind::RANGE_EXPR, c);

        self.consume(Token![..]);

        self.check(Token![=]);
        self.parse_expression(None);

        self.finish_node();

        SyntaxKind::RANGE_EXPR
    }

    /// Parses an expression on the current cursor position, which is preceded
    /// by some identifier.
    fn parse_named_expression(&mut self) -> SyntaxKind {
        assert!(
            matches!(self.token(), SyntaxKind::IDENT | SyntaxKind::SELF_EXPR),
            "found {:?}",
            self.token()
        );

        let c = self.checkpoint();
        let ident_slice = self.content_at(self.span()).to_string();

        match self.token_at(1) {
            // If the next token is an opening parenthesis, it's a method invocation
            SyntaxKind::LEFT_PAREN => self.parse_static_call(c),

            // If the next token is a dot, it's some form of member access
            Token![.] => self.parse_member(c),

            // If the next token is an equal sign, it's an assignment expression
            Token![=] => self.parse_assignment(c),

            // If the identifier is following by a separator, it's refering to a namespaced identifier
            Token![::] => self.parse_path_expression(c),

            // If the identifier is followed by a left curly brace, it's a struct construction.
            // We only assume this if the identifier is actually camel-case, as it might otherwise not
            // refer to a type.
            SyntaxKind::LEFT_BRACE if !ident_slice.starts_with(|c: char| c.is_ascii_lowercase()) => {
                self.parse_path();
                self.parse_construction_expression(c)
            }

            // If the identifier is following by a lesser sign, it might be referring to a generic call expression
            Token![<] => {
                // If we fail to parse the type arguments, move back to the original position
                // and continue parsing at the fallthrough case.
                let has_type_args = self.parse_type_arguments();
                let next_token = self.token();

                match (has_type_args, next_token) {
                    (_, SyntaxKind::LEFT_PAREN) => {
                        self.parse_name();
                        self.parse_instance_call(c, ParseBoundTypes::Ignore)
                    }
                    (_, SyntaxKind::LEFT_BRACE) => self.parse_construction_expression(c),
                    (true, _) => self.parse_path_expression(c),
                    (false, _) => self.parse_variable_reference(c),
                }
            }

            // If the identifier is all upper case, we'll parse it as a const variable reference
            _ if ident_slice.chars().all(|c: char| c == '_' || c.is_ascii_uppercase()) => {
                self.parse_variable_reference(c)
            }

            // If the identifier is not lower case, it might be a path expression
            _ if !ident_slice.starts_with(|c: char| c.is_ascii_lowercase()) => self.parse_path_expression(c),

            // If the name stands alone, it's likely a variable reference
            _ => self.parse_variable_reference(c),
        }
    }

    /// Parses a call expression on the current cursor position.
    fn parse_instance_call(&mut self, c: Checkpoint, bound_types: ParseBoundTypes) -> SyntaxKind {
        self.start_node_at(SyntaxKind::INSTANCE_CALL_EXPR, c);

        if bound_types == ParseBoundTypes::Include {
            self.parse_type_arguments();
        }

        self.parse_call_arguments();
        self.finish_node();

        SyntaxKind::INSTANCE_CALL_EXPR
    }

    /// Parses a static call expression on the current cursor position.
    fn parse_static_call(&mut self, c: Checkpoint) -> SyntaxKind {
        self.start_node_at(SyntaxKind::STATIC_CALL_EXPR, c);

        self.parse_name();

        self.parse_type_arguments();
        self.parse_call_arguments();

        self.finish_node();

        SyntaxKind::STATIC_CALL_EXPR
    }

    /// Parses zero-or-more call arguments at the current cursor position.
    fn parse_call_arguments(&mut self) {
        self.start_node(SyntaxKind::ARG_LIST);
        self.consume_paren_seq(|parser| {
            parser.parse_expression(None);
        });
        self.finish_node();
    }

    /// Parses an "if" conditional statement at the current cursor position.
    fn parse_if_conditional(&mut self) -> SyntaxKind {
        self.start_node(SyntaxKind::IF_EXPR);

        self.consume(Token![if]);
        self.parse_expression(None);
        self.parse_block();

        if self.check(Token![else]) {
            if self.peek(Token![if]) {
                self.parse_if_conditional();
            } else {
                self.parse_block();
            }
        }

        self.finish_node();

        SyntaxKind::IF_EXPR
    }

    /// Parses a member expression on the current cursor position, which is
    /// preceded by some identifier.
    fn parse_member(&mut self, c: Checkpoint) -> SyntaxKind {
        self.parse_identifier();
        self.consume(Token![.]);
        self.parse_name();

        let is_method_call = self.peek(SyntaxKind::LEFT_PAREN);

        let is_generic_method_call = if self.peek(Token![<])
            && let SyntaxKind::IDENT = self.token_at(1)
            && self
                .content_at(self.span_at(1))
                .starts_with(|c: char| c.is_ascii_uppercase())
        {
            true
        } else {
            false
        };

        if is_method_call || is_generic_method_call {
            tracing::trace!("member expr is method invocation");

            return self.parse_instance_call(c, ParseBoundTypes::Include);
        }

        self.start_node_at(SyntaxKind::MEMBER_EXPR, c);

        // If there is yet another dot, it's part of a longer expression.
        if self.peek(Token![.]) {
            tracing::trace!("member expr is being chained");

            self.parse_member(c);
        }

        SyntaxKind::MEMBER_EXPR
    }

    /// Parses a cast expression on the current cursor position, which is
    /// preceded by some identifier.
    fn parse_cast(&mut self, c: Checkpoint) -> SyntaxKind {
        self.start_node_at(SyntaxKind::CAST_EXPR, c);

        self.consume(Token![as]);
        self.parse_type();

        self.finish_node();

        SyntaxKind::CAST_EXPR
    }

    /// Parses an instance checking expression on the current cursor position.
    fn parse_is(&mut self, c: Checkpoint) -> SyntaxKind {
        self.start_node_at(SyntaxKind::IS_EXPR, c);

        self.consume(Token![is]);
        self.parse_pattern();

        self.finish_node();

        SyntaxKind::IS_EXPR
    }

    /// Parses an assignment expression on the current cursor position.
    fn parse_assignment(&mut self, c: Checkpoint) -> SyntaxKind {
        self.start_node_at(SyntaxKind::ASSIGNMENT_EXPR, c);

        self.parse_identifier();
        self.consume(Token![=]);
        self.parse_expression(None);

        self.finish_node();

        SyntaxKind::ASSIGNMENT_EXPR
    }

    /// Parses a path expression on the current cursor position.
    fn parse_path_expression(&mut self, c: Checkpoint) -> SyntaxKind {
        let path_kind = self.parse_path();

        match path_kind {
            SyntaxKind::PATH_NAMESPACE => {
                self.error_and_skip("unexpected namespace path segment");
            }
            SyntaxKind::PATH_TYPE => {
                return self.parse_construction_expression(c);
            }
            SyntaxKind::PATH_CALLABLE => {
                self.start_node_at(SyntaxKind::STATIC_CALL_EXPR, c);
                self.parse_call_arguments();

                self.finish_node();
            }
            SyntaxKind::PATH_VARIANT => {
                self.start_node_at(SyntaxKind::VARIABLE_EXPR, c);
                if self.peek(SyntaxKind::LEFT_PAREN) {
                    self.parse_call_arguments();
                }

                self.finish_node();
            }
            kind => unreachable!("bug!: unexpected syntax kind, {kind:?}"),
        }

        SyntaxKind::PATH
    }

    /// Parses a struct construction expression on the current cursor position.
    fn parse_construction_expression(&mut self, c: Checkpoint) -> SyntaxKind {
        self.start_node_at(SyntaxKind::CONSTRUCT_EXPR, c);

        let finished = self.consume_comma_seq(SyntaxKind::LEFT_BRACE, SyntaxKind::RIGHT_BRACE, |parser| {
            parser.start_node(SyntaxKind::FIELD);
            parser.parse_name();

            if parser.check(Token![:]) {
                parser.start_node(SyntaxKind::VALUE);
                parser.parse_expression(None);
                parser.finish_node();
            }

            parser.finish_node();
        });

        if !finished {
            self.recover_with_set(&[Token![;]]);
        }

        self.finish_node();

        SyntaxKind::CONSTRUCT_EXPR
    }

    /// Parses a literal value expression on the current cursor position.
    pub(crate) fn parse_literal(&mut self) -> SyntaxKind {
        self.start_node(SyntaxKind::LITERAL);

        match self.token() {
            SyntaxKind::INTEGER_LIT => {
                self.node_token(SyntaxKind::INTEGER_LIT, self.span());
            }
            SyntaxKind::FLOAT_LIT => {
                self.node_token(SyntaxKind::FLOAT_LIT, self.span());
            }
            SyntaxKind::STRING_LIT => {
                self.node_token(SyntaxKind::STRING_LIT, self.span());
            }
            Token![true] | Token![false] => {
                self.node_token(SyntaxKind::BOOLEAN_LIT, self.span());
            }
            _ => {
                self.error("expected literal");
            }
        }

        self.skip();
        self.finish_node();

        SyntaxKind::LITERAL
    }

    /// Parses a unary expression on the current cursor position.
    fn parse_unary(&mut self, c: Checkpoint) -> SyntaxKind {
        self.start_node_at(SyntaxKind::UNARY_EXPR, c);

        if !self.check_any(&[Token![!], Token![-]]) {
            self.error("expected unary expression");
        }

        self.parse_expression_with_precedence(None, lume_syntax::UNARY_PRECEDENCE);
        self.finish_node();

        SyntaxKind::UNARY_EXPR
    }

    /// Parses a variable reference expression on the current cursor position.
    fn parse_variable_reference(&mut self, c: Checkpoint) -> SyntaxKind {
        self.start_node_at(SyntaxKind::VARIABLE_EXPR, c);
        self.parse_identifier();
        self.finish_node();

        SyntaxKind::VARIABLE_EXPR
    }
}
