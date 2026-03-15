use crate::*;

impl Parser {
    /// Attempts to recover from a parsing error, whilst attempting
    /// to parse a token.
    ///
    /// This method will simply continue to bump the parser until a token from
    /// the given set is reached.
    ///
    /// # Returns
    ///
    /// Returns whether the parser sufficiently recovered from the error.
    pub(crate) fn recover_with_set(&mut self, set: &[SyntaxKind]) -> bool {
        loop {
            if self.eof() {
                return false;
            }

            if self.peek_any(set) {
                return true;
            }

            self.consume_any();
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
    pub(crate) fn recover_statement(&mut self) -> bool {
        let mut brace_depth = 0;
        let mut bracket_depth = 0;

        loop {
            let tok = self.token();

            match tok {
                SyntaxKind::LEFT_BRACE => {
                    self.consume(tok);
                    brace_depth += 1;
                }
                SyntaxKind::LEFT_BRACKET => {
                    self.consume(tok);
                    bracket_depth += 1;
                }
                SyntaxKind::RIGHT_BRACE => {
                    self.consume(tok);
                    brace_depth -= 1;
                }
                SyntaxKind::RIGHT_BRACKET => {
                    self.consume(tok);
                    bracket_depth -= 1;
                }
                SyntaxKind::SEMICOLON => {
                    self.consume(tok);

                    if brace_depth == 0 && bracket_depth == 0 {
                        return true;
                    }
                }
                SyntaxKind::EOF => {
                    return false;
                }
                _ => {
                    self.consume(tok);
                }
            }
        }
    }
}
