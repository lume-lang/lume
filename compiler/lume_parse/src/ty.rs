use crate::*;

impl Parser {
    /// Parses some abstract type at the current cursor position.
    pub(super) fn parse_type(&mut self) {
        match self.token() {
            SyntaxKind::IDENT => self.parse_named_type(),
            SyntaxKind::LEFT_BRACKET => self.parse_array_type(),
            _ => {
                self.error_and_skip("expected type");
            }
        }
    }

    /// Parses either a scalar- or generic-type at the current cursor position.
    fn parse_named_type(&mut self) {
        self.parse_path();
    }

    /// Parses an array type at the current cursor position.
    fn parse_array_type(&mut self) {
        self.start_node(SyntaxKind::ARRAY_TYPE);

        self.consume(SyntaxKind::LEFT_BRACKET);
        self.parse_type();
        self.consume(SyntaxKind::RIGHT_BRACKET);

        self.finish_node();
    }

    /// Parses some abstract type at the current cursor position.
    pub(super) fn parse_opt_type(&mut self) {
        if self.check(Token![:]) {
            self.parse_type();
        }
    }
}
