use error_snippet::Result;
use lume_ast::*;
use lume_lexer::TokenKind;

use crate::Parser;

impl Parser {
    /// Parses zero-or-more type parameters.
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

    /// Parses zero-or-more type arguments.
    pub(super) fn parse_type_arguments(&mut self) -> Result<Vec<TypeArgument>> {
        if !self.peek(TokenKind::Less) {
            return Ok(Vec::new());
        }

        self.consume_comma_seq(TokenKind::Less, TokenKind::Greater, |p| {
            let ty = p.parse_type()?;

            Ok(TypeArgument { ty })
        })
    }
}
