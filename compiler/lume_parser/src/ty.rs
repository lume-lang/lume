use error_snippet::Result;
use lume_ast::*;
use lume_lexer::TokenKind;

use crate::{Parser, err, errors::*};

impl Parser {
    /// Parses some abstract type at the current cursor position.
    pub(super) fn parse_type(&mut self) -> Result<Type> {
        let token = self.token()?;

        match token.kind {
            TokenKind::Identifier => self.parse_scalar_or_generic_type(),
            TokenKind::LeftBracket => self.parse_array_type(),
            _ => Err(err!(self, UnexpectedType, actual, token.kind)),
        }
    }

    /// Parses either a scalar- or generic-type at the current cursor position.
    fn parse_scalar_or_generic_type(&mut self) -> Result<Type> {
        let name = self.parse_path()?;

        if self.peek(TokenKind::Less)? {
            self.parse_generic_type_arguments(name)
        } else {
            Ok(Type::Scalar(Box::new(ScalarType { name })))
        }
    }

    /// Parses an array type at the current cursor position.
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

    /// Parses a generic type, with zero-or-more type parameters at the current cursor position.
    fn parse_generic_type_arguments(&mut self, name: Path) -> Result<Type> {
        let (type_params, location) = self.consume_with_loc(|p| {
            p.consume_comma_seq(TokenKind::Less, TokenKind::Greater, |p| Ok(Box::new(p.parse_type()?)))
        })?;

        Ok(Type::Generic(Box::new(GenericType {
            name,
            type_params,
            location,
        })))
    }

    /// Parses some abstract type at the current cursor position.
    pub(super) fn parse_opt_type(&mut self) -> Result<Option<Type>> {
        if self.consume_if(TokenKind::Colon)?.is_none() {
            Ok(None)
        } else {
            Ok(Some(self.parse_type()?))
        }
    }
}
