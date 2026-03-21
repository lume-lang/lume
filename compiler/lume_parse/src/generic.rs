use crate::*;

impl Parser {
    /// Parses zero-or-more type parameters.
    pub(super) fn parse_type_parameters(&mut self) -> bool {
        if !self.peek(Token![<]) {
            return false;
        }

        self.start_node(SyntaxKind::BOUND_TYPES);

        let finished = self.consume_comma_seq(Token![<], Token![>], |p| {
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

        if !finished {
            self.recover_with_set(&[Token![>]]);
            self.check(Token![>]);
        }

        self.finish_node();

        finished
    }

    /// Attempts to determine whether the next token(s) are type arguments.
    pub(super) fn is_type_arguments(&mut self, offset: usize) -> (usize, bool) {
        if !self.peek_at(offset, Token![<]) {
            return (0, false);
        }

        let mut idx = offset;
        let mut angles_count = 0_usize;

        for (tok, _span) in self.tokens.iter().rev().skip(offset + 1) {
            idx += 1;

            match tok {
                Token![<] => angles_count += 1,
                Token![>] => angles_count -= 1,
                Token![,]
                | Token![::]
                | SyntaxKind::IDENT
                | SyntaxKind::LEFT_BRACKET
                | SyntaxKind::RIGHT_BRACKET
                | SyntaxKind::WHITESPACE
                | SyntaxKind::NEWLINE => {}
                _ => {
                    return (idx, false);
                }
            }

            if angles_count == 0 {
                break;
            }
        }

        (idx, true)
    }

    /// Parses zero-or-more type arguments, boxed as [`Box<Type>`].
    pub(super) fn parse_type_arguments(&mut self) -> bool {
        if !self.peek(Token![<]) {
            return false;
        }

        self.start_node(SyntaxKind::GENERIC_ARGS);
        let finished = self.consume_comma_seq(Token![<], Token![>], Parser::parse_type);
        self.finish_node();

        finished
    }
}
