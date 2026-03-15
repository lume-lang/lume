use crate::*;

impl Parser {
    /// Parses zero-or-more type parameters.
    pub(super) fn parse_type_parameters(&mut self) {
        if !self.peek(Token![<]) {
            return;
        }

        self.start_node(SyntaxKind::BOUND_TYPES);

        self.consume_comma_seq(Token![<], Token![>], |p| {
            p.start_node(SyntaxKind::BOUND_TYPE);

            p.parse_identifier();

            if p.peek(Token![:]) {
                p.start_node(SyntaxKind::CONSTRAINTS);

                p.consume(Token![:]);
                p.parse_type();

                while p.check(Token![+]) {
                    p.parse_type();
                }

                p.finish_node();
            }

            p.finish_node();
        });

        self.finish_node();
    }

    /// Parses zero-or-more type arguments, boxed as [`Box<Type>`].
    pub(super) fn parse_type_arguments(&mut self) -> bool {
        if !self.peek(Token![<]) {
            return false;
        }

        self.start_node(SyntaxKind::BOUND_TYPES);
        self.consume_comma_seq(Token![<], Token![>], Parser::parse_type);
        self.finish_node();

        true
    }
}
