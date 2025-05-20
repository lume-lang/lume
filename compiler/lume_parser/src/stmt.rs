use error_snippet::Result;
use lume_ast::*;
use lume_lexer::TokenKind;

use crate::{Parser, err, errors::*};

impl Parser {
    /// Parses zero-or-more abstract statements at the current cursor position.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the parser hits an unexpected token.
    #[allow(dead_code)]
    pub fn parse_statements(&mut self) -> Result<Vec<Statement>> {
        let mut statements = Vec::new();

        while !self.eof() {
            statements.push(self.parse_statement()?);
        }

        Ok(statements)
    }

    /// Parses some abstract statement at the current cursor position.
    pub(super) fn parse_statement(&mut self) -> Result<Statement> {
        match self.token().kind {
            TokenKind::Let => self.parse_variable_declaration(),
            TokenKind::Break => self.parse_break(),
            TokenKind::Continue => self.parse_continue(),
            TokenKind::Return => self.parse_return(),
            TokenKind::If | TokenKind::Unless => self.parse_conditional(),
            TokenKind::Loop => self.parse_infinite_loop(),
            TokenKind::For => self.parse_iterator_loop(),
            TokenKind::While => self.parse_predicate_loop(),
            _ => self.parse_expression_stmt(),
        }
    }

    /// Attempts to recover from a parsing error, whilst attempting to parse a statement.
    ///
    /// The recovery for statements is the move the cursor to the next semicolon. So, given the
    /// following Lume statement:
    ///
    /// ```lm
    /// let a: = 1;
    ///        ^ error occurs here...
    ///            ^ ...so we move the cursor to here
    /// ```
    pub(super) fn recover_statement(&mut self) {
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
                        break;
                    }
                }
                _ => self.skip(),
            }
        }
    }

    /// Parses a variable declaration statement at the current cursor position.
    fn parse_variable_declaration(&mut self) -> Result<Statement> {
        // Whatever the token is, consume it.
        let start = self.consume_any().start();

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
        match self.token().kind {
            TokenKind::If => self.parse_if_conditional(),
            TokenKind::Unless => self.parse_unless_conditional(),
            k => panic!("invalid conditional token given: {k}"),
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
            cases,
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
        if self.peek(TokenKind::Else) && self.peek_next(TokenKind::If) {
            return Err(err!(self, UnlessElseIfClause));
        }

        // Append the `else` case
        self.parse_else_conditional_case(&mut cases)?;

        let end = cases.last().unwrap().location.end();

        let conditional = UnlessCondition {
            cases,
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
            if !self.peek(TokenKind::Else) || !self.peek_next(TokenKind::If) {
                break;
            }

            self.consume(TokenKind::Else)?;
            self.consume(TokenKind::If)?;

            self.parse_conditional_case(cases)?;
        }

        Ok(())
    }

    /// Parses zero-or-one `else` cases within a conditional statement at the current cursor position..
    ///
    /// # Errors
    ///
    /// Returns `Err` if the parser hits an unexpected token.
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

    /// Parses an expression statement at the current cursor position.
    fn parse_expression_stmt(&mut self) -> Result<Statement> {
        let expression = self.parse_expression()?;

        self.consume(TokenKind::Semicolon)?;

        Ok(Statement::Expression(Box::new(expression)))
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
}
