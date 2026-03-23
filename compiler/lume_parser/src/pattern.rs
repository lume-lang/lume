use crate::*;

impl Parser {
    pub(super) fn parse_pattern(&mut self) {
        match self.token() {
            SyntaxKind::IDENT => self.parse_ident_pattern(),
            Token![..] => self.parse_wildcard_pattern(),

            k if k.is_literal() => self.parse_literal_pattern(),

            _ => {
                self.error("invalid pattern");
                self.skip();
            }
        }
    }

    fn parse_literal_pattern(&mut self) {
        self.start_node(SyntaxKind::PAT_LITERAL);
        self.parse_literal();
        self.finish_node();
    }

    fn parse_ident_pattern(&mut self) {
        let c = self.checkpoint();

        // Only a single ident found - likely not a variant.
        if self.peek(SyntaxKind::IDENT) && !self.peek_at(1, Token![::]) && !self.peek_at(1, Token![<]) {
            self.start_node_at(SyntaxKind::PAT_IDENT, c);
            self.parse_ident();
            self.finish_node();

            return;
        }

        self.start_node_at(SyntaxKind::PAT_VARIANT, c);
        self.parse_path();

        if self.peek(SyntaxKind::LEFT_PAREN) {
            self.consume_paren_seq(Parser::parse_pattern);
        }

        self.finish_node();
    }

    fn parse_wildcard_pattern(&mut self) {
        self.start_node(SyntaxKind::PAT_WILDCARD);
        self.consume(Token![..]);
        self.finish_node();
    }
}
