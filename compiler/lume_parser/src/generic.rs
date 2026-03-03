use error_snippet::Result;
use lume_ast::*;
use lume_lexer::TokenType;

use crate::Parser;

impl<'ast> Parser<'_, 'ast> {
    /// Parses zero-or-more type parameters.
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub(super) fn parse_type_parameters(&mut self) -> Result<Vec<TypeParameter<'ast>>> {
        if !self.peek(TokenType::Less) {
            return Ok(Vec::new());
        }

        self.consume_comma_seq(TokenType::Less, TokenType::Greater, |p| {
            let name = p.parse_identifier()?;
            let mut constraints = Vec::new();

            if p.check(TokenType::Colon) {
                constraints.push(p.parse_type()?);

                while p.check(TokenType::Add) {
                    constraints.push(p.parse_type()?);
                }
            }

            Ok(TypeParameter { name, constraints })
        })
    }

    /// Parses zero-or-more type arguments, boxed as [`Box<Type>`].
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub(super) fn parse_type_arguments(&mut self) -> Result<Vec<Type<'ast>>> {
        if !self.peek(TokenType::Less) {
            return Ok(Vec::new());
        }

        self.consume_comma_seq(TokenType::Less, TokenType::Greater, Parser::parse_type)
    }
}
