use error_snippet::Result;
use lume_ast::*;
use lume_lexer::TokenKind;

use crate::errors::*;
use crate::{Parser, err};

impl Parser {
    /// Parses some abstract type at the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub(super) fn parse_type(&mut self) -> Result<Type> {
        let token = self.token();

        match token.kind {
            TokenKind::Identifier => self.parse_named_type(),
            TokenKind::LeftBracket => self.parse_array_type(),
            _ => Err(err!(self, UnexpectedType, actual, token.kind)),
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
        let start = self.consume(TokenKind::LeftBracket)?.start();

        let element_type = Box::new(self.parse_type()?);

        let end = self.consume(TokenKind::RightBracket)?.end();

        let array_type = ArrayType {
            element_type,
            location: (start..end).into(),
        };

        Ok(Type::Array(Box::new(array_type)))
    }

    /// Parses some abstract type at the current cursor position.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub(super) fn parse_opt_type(&mut self) -> Result<Option<Type>> {
        if self.check(TokenKind::Colon) {
            Ok(Some(self.parse_type()?))
        } else {
            Ok(None)
        }
    }
}
