use error_snippet::Result;
use lume_ast::*;
use lume_lexer::TokenType;

use crate::Parser;
use crate::errors::*;

impl Parser<'_> {
    /// Parses some abstract type at the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub(super) fn parse_type(&mut self) -> Result<Type> {
        let token = self.token();

        match token.kind.as_type() {
            TokenType::Identifier => self.parse_named_type(),
            TokenType::LeftBracket => self.parse_array_type(),
            ty => Err(UnexpectedType {
                source: self.source.clone(),
                range: token.index.clone(),
                actual: ty,
            }
            .into()),
        }
    }

    /// Parses either a scalar- or generic-type at the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_named_type(&mut self) -> Result<Type> {
        let name = self.parse_path()?;

        Ok(Type::Named(Box::new(NamedType { name })))
    }

    /// Parses an array type at the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_array_type(&mut self) -> Result<Type> {
        let start = self.consume(TokenType::LeftBracket)?.start();

        let element_type = Box::new(self.parse_type()?);

        let end = self.consume(TokenType::RightBracket)?.end();

        let array_type = ArrayType {
            element_type,
            location: (start..end).into(),
        };

        Ok(Type::Array(Box::new(array_type)))
    }

    /// Parses some abstract type at the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub(super) fn parse_opt_type(&mut self) -> Result<Option<Type>> {
        if self.check(TokenType::Colon) {
            Ok(Some(self.parse_type()?))
        } else {
            Ok(None)
        }
    }
}
