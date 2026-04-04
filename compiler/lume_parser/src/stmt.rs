use crate::*;

impl Parser {
    /// Returns a block for functions or methods.
    pub(crate) fn parse_block(&mut self) {
        self.start_node(SyntaxKind::BLOCK);
        self.consume(SyntaxKind::LEFT_BRACE);

        while !self.peek(SyntaxKind::RIGHT_BRACE) && !self.eof() {
            self.parse_statement();
        }

        self.consume(SyntaxKind::RIGHT_BRACE);
        self.finish_node();
    }

    /// Parses zero-or-more abstract statements at the current cursor position.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the parser hits an unexpected token.
    #[allow(dead_code, reason = "used in tests and fuzzing")]
    pub fn parse_statements(&mut self) {
        while !self.eof() {
            self.parse_statement();
        }
    }

    /// Parses some abstract statement at the current cursor position.
    pub(super) fn parse_statement(&mut self) {
        match self.token() {
            Token![let] => self.parse_variable_declaration(),
            Token![break] => self.parse_break(),
            Token![continue] => self.parse_continue(),
            Token![return] => self.parse_return(),
            Token![loop] => self.parse_infinite_loop(),
            Token![for] => self.parse_iterator_loop(),
            Token![while] => self.parse_predicate_loop(),
            Token![;] => {
                self.consume(Token![;]);
            }
            _ => self.parse_expression_stmt(),
        }
    }

    /// Parses a variable declaration statement at the current cursor position.
    fn parse_variable_declaration(&mut self) {
        self.start_node(SyntaxKind::LET_STMT);
        self.consume(Token![let]);

        self.parse_ident();
        self.parse_opt_type();

        self.consume(Token![=]);

        self.parse_expression(None);
        self.consume(Token![;]);
        self.finish_node();
    }

    /// Parses an infinite loop statement at the current cursor position.
    fn parse_infinite_loop(&mut self) {
        self.start_node(SyntaxKind::LOOP_STMT);

        self.consume(Token![loop]);
        self.parse_block();

        self.finish_node();
    }

    /// Parses an iterator loop statement at the current cursor position.
    fn parse_iterator_loop(&mut self) {
        self.start_node(SyntaxKind::FOR_STMT);
        self.consume(Token![for]);

        self.parse_ident();

        self.consume(Token![in]);

        self.parse_expression(None);
        self.parse_block();

        self.finish_node();
    }

    /// Parses a predicate loop statement at the current cursor position.
    fn parse_predicate_loop(&mut self) {
        self.start_node(SyntaxKind::WHILE_STMT);
        self.consume(Token![while]);

        self.parse_expression(None);
        self.parse_block();

        self.finish_node();
    }

    /// Parses an expression statement at the current cursor position.
    fn parse_expression_stmt(&mut self) {
        let c = self.checkpoint();

        let kind = self.parse_expression(None);

        if kind.is_none() {
            return;
        }

        // If we match the end of the current body, assume it's a final.
        if self.peek(SyntaxKind::RIGHT_BRACE) {
            self.complete_node(SyntaxKind::FINAL_STMT, c);
            return;
        }

        self.start_node_at(SyntaxKind::EXPR_STMT, c);

        if !matches!(kind, Some(SyntaxKind::SWITCH_EXPR | SyntaxKind::IF_EXPR)) {
            self.consume(Token![;]);
        }

        self.finish_node();
    }

    /// Parses a `break` statement at the current cursor position.
    fn parse_break(&mut self) {
        self.start_node(SyntaxKind::BREAK_STMT);

        self.consume(Token![break]);
        self.consume(Token![;]);

        self.finish_node();
    }

    /// Parses a `continue` statement at the current cursor position.
    fn parse_continue(&mut self) {
        self.start_node(SyntaxKind::CONTINUE_STMT);

        self.consume(Token![continue]);
        self.consume(Token![;]);

        self.finish_node();
    }

    /// Parses a return statement at the current cursor position.
    fn parse_return(&mut self) {
        self.start_node(SyntaxKind::RETURN_STMT);

        self.consume(Token![return]);
        self.parse_opt_expression(None);
        self.consume(Token![;]);

        self.finish_node();
    }
}
