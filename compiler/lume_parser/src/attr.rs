use error_snippet::Result;
use lume_ast::*;
use lume_lexer::TokenType;

use crate::Parser;

impl<'ast> Parser<'_, 'ast> {
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub(super) fn parse_attributes(&mut self) -> Result<Vec<Attribute<'ast>>> {
        self.consume_any_seq(TokenType::Exclamation, Parser::parse_attribute)
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    pub(super) fn parse_attribute(&mut self) -> Result<Attribute<'ast>> {
        let start = self.consume(TokenType::Exclamation)?.start();
        self.consume(TokenType::LeftBracket)?;

        let name = self.parse_identifier()?;
        let arguments = self.parse_attribute_arguments()?;

        let end = self.consume(TokenType::RightBracket)?.end();

        Ok(Attribute {
            name,
            arguments,
            location: (start..end).into(),
        })
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn parse_attribute_arguments(&mut self) -> Result<Vec<AttributeArgument<'ast>>> {
        self.consume_comma_seq(
            TokenType::LeftParen,
            TokenType::RightParen,
            Parser::parse_attribute_argument,
        )
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn parse_attribute_argument(&mut self) -> Result<AttributeArgument<'ast>> {
        let key = self.parse_identifier()?;

        self.consume(TokenType::Assign)?;

        let value = self.parse_literal_inner()?;

        let start = key.location.start();
        let end = value.location().start();

        Ok(AttributeArgument {
            key,
            value,
            location: (start..end).into(),
        })
    }
}
