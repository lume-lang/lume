use error_snippet::Result;
use lume_ast::*;
use lume_lexer::{TokenKind, TokenType};

use crate::Parser;

impl Parser<'_> {
    /// Parses zero-or-more abstract statements at the current cursor position.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the parser hits an unexpected token.
    #[allow(dead_code, reason = "used in tests and fuzzing")]
    #[libftrace::traced(level = Trace, err)]
    pub fn parse_statements(&mut self) -> Result<Vec<Statement>> {
        let mut statements = Vec::new();

        while !self.eof() {
            statements.push(self.parse_statement()?);
        }

        Ok(statements)
    }

    /// Parses some abstract statement at the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    pub(super) fn parse_statement(&mut self) -> Result<Statement> {
        match self.token().kind {
            TokenKind::Let => self.parse_variable_declaration(),
            TokenKind::Break => self.parse_break(),
            TokenKind::Continue => self.parse_continue(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Loop => self.parse_infinite_loop(),
            TokenKind::For => self.parse_iterator_loop(),
            TokenKind::While => self.parse_predicate_loop(),
            _ => self.parse_expression_stmt(),
        }
    }

    /// Attempts to recover from a parsing error, whilst attempting to parse a
    /// statement.
    ///
    /// The recovery for statements is the move the cursor to the next
    /// semicolon. So, given the following Lume statement:
    ///
    /// ```lm
    /// let a: = 1;
    ///        ^ error occurs here...
    ///            ^ ...so we move the cursor to here
    /// ```
    ///
    /// # Returns
    ///
    /// Returns whether the parser sufficiently recovered from the error.
    #[libftrace::traced(level = Trace)]
    pub(super) fn recover_statement(&mut self) -> bool {
        let mut brace_depth = 0;
        let mut bracket_depth = 0;

        loop {
            match self.token().kind {
                TokenKind::LeftCurly => {
                    self.skip();
                    brace_depth += 1;
                }
                TokenKind::LeftBracket => {
                    self.skip();
                    bracket_depth += 1;
                }
                TokenKind::RightCurly => {
                    self.skip();
                    brace_depth -= 1;
                }
                TokenKind::RightBracket => {
                    self.skip();
                    bracket_depth -= 1;
                }
                TokenKind::Semicolon => {
                    self.skip();

                    if brace_depth == 0 && bracket_depth == 0 {
                        return true;
                    }
                }
                TokenKind::Eof => {
                    return false;
                }
                _ => self.skip(),
            }
        }
    }

    /// Parses a variable declaration statement at the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_variable_declaration(&mut self) -> Result<Statement> {
        // Whatever the token is, consume it.
        let start = self.consume_any().start();

        let name = self.parse_identifier()?;
        let variable_type = self.parse_opt_type()?;

        self.consume(TokenType::Assign)?;

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

    /// Parses an infinite loop statement at the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_infinite_loop(&mut self) -> Result<Statement> {
        let start = self.consume(TokenType::Loop)?.start();
        let block = self.parse_block()?;

        let location = start..block.location.end();

        Ok(Statement::InfiniteLoop(Box::new(InfiniteLoop {
            block,
            location: location.into(),
        })))
    }

    /// Parses an iterator loop statement at the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_iterator_loop(&mut self) -> Result<Statement> {
        let start = self.consume(TokenType::For)?.start();

        let pattern = self.parse_identifier()?;

        self.consume(TokenType::In)?;

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
    #[libftrace::traced(level = Trace, err)]
    fn parse_predicate_loop(&mut self) -> Result<Statement> {
        let start = self.consume(TokenType::While)?.start();

        let condition = self.parse_expression()?;
        let block = self.parse_block()?;

        let location = start..block.location.end();

        Ok(Statement::PredicateLoop(Box::new(PredicateLoop {
            condition,
            block,
            location: location.into(),
        })))
    }

    /// Parses an expression statement at the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_expression_stmt(&mut self) -> Result<Statement> {
        let expression = self.parse_expression()?;

        if self.peek(TokenType::RightCurly) {
            return Ok(Statement::Final(Box::new(Final { value: expression })));
        }

        if !matches!(expression, Expression::Switch(_) | Expression::If(_)) {
            self.consume(TokenType::Semicolon)?;
        }

        Ok(Statement::Expression(Box::new(expression)))
    }

    /// Parses a `break` statement at the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_break(&mut self) -> Result<Statement> {
        let start = self.consume(TokenType::Break)?.start();
        let end = self.expect_semi()?.end();

        Ok(Statement::Break(Box::new(Break {
            location: (start..end).into(),
        })))
    }

    /// Parses a `continue` statement at the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_continue(&mut self) -> Result<Statement> {
        let start = self.consume(TokenType::Continue)?.start();
        let end = self.expect_semi()?.end();

        Ok(Statement::Continue(Box::new(Continue {
            location: (start..end).into(),
        })))
    }

    /// Parses a return statement at the current cursor position.
    #[libftrace::traced(level = Trace, err)]
    fn parse_return(&mut self) -> Result<Statement> {
        let start = self.consume(TokenType::Return)?.start();

        let value = self.parse_opt_expression()?;
        let end = self.expect_semi()?.end();

        let statement = Statement::Return(Box::new(Return {
            value,
            location: (start..end).into(),
        }));

        Ok(statement)
    }
}
