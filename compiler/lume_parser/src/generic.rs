use error_snippet::Result;
use lume_ast::*;
use lume_lexer::TokenKind;

use crate::Parser;

impl Parser {
    /// Parses zero-or-more type parameters.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub(super) fn parse_type_parameters(&mut self) -> Result<Vec<TypeParameter>> {
        if !self.peek(TokenKind::Less) {
            return Ok(Vec::new());
        }

        self.consume_comma_seq(TokenKind::Less, TokenKind::Greater, |p| {
            let name = p.parse_identifier()?;
            let mut constraints = Vec::new();

            if p.check(TokenKind::Colon) {
                constraints.push(Box::new(p.parse_type()?));

                while p.check(TokenKind::Add) {
                    constraints.push(Box::new(p.parse_type()?));
                }
            }

            Ok(TypeParameter { name, constraints })
        })
    }

    /// Parses zero-or-more type arguments, boxed as [`Box<Type>`].
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub(super) fn parse_type_arguments(&mut self) -> Result<Vec<Type>> {
        if !self.peek(TokenKind::Less) {
            return Ok(Vec::new());
        }

        self.consume_comma_seq(TokenKind::Less, TokenKind::Greater, Parser::parse_type)
    }
}
