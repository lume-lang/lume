use crate::*;

impl Parser {
    /// Parses some abstract type at the current cursor position.
    pub(super) fn parse_type(&mut self) {
        match self.token() {
            SyntaxKind::IDENT => self.parse_identd_type(),
            SyntaxKind::LEFT_BRACKET => self.parse_array_type(),
            SyntaxKind::SELF_TYPE => self.parse_self_type(),
            _ => {
                self.error_and_skip("expected type");
            }
        }
    }

    /// Parses either a scalar- or generic-type at the current cursor position.
    fn parse_identd_type(&mut self) {
        self.start_node(SyntaxKind::NAMED_TYPE);
        self.parse_path();
        self.finish_node();
    }

    /// Parses an array type at the current cursor position.
    fn parse_array_type(&mut self) {
        self.start_node(SyntaxKind::ARRAY_TYPE);

        self.consume(SyntaxKind::LEFT_BRACKET);
        self.parse_type();
        self.consume(SyntaxKind::RIGHT_BRACKET);

        self.finish_node();
    }

    /// Parses a `Self` type at the current cursor position.
    fn parse_self_type(&mut self) {
        self.start_node(SyntaxKind::SELF_TYPE);
        self.consume(SyntaxKind::SELF_TYPE);
        self.finish_node();
    }

    /// Parses some abstract type at the current cursor position.
    pub(super) fn parse_opt_type(&mut self) {
        if self.check(Token![:]) {
            self.parse_type();
        }
    }
}
