use error_snippet::Result;
use lume_ast::*;
use lume_lexer::TokenKind;

use crate::{Parser, errors::*};

impl Parser {
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub(super) fn parse_pattern(&mut self) -> Result<Pattern> {
        match self.token().kind {
            TokenKind::Identifier => self.parse_named_pattern(),
            TokenKind::DotDot => self.parse_wildcard_pattern(),

            k if k.is_literal() => self.parse_literal_pattern(),

            k => Err(InvalidPattern {
                source: self.source.clone(),
                range: self.token().index,
                found: k,
            }
            .into()),
        }
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_literal_pattern(&mut self) -> Result<Pattern> {
        let literal_expr = self.parse_literal_inner()?;

        Ok(Pattern::Literal(literal_expr))
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_named_pattern(&mut self) -> Result<Pattern> {
        let name = self.expect(TokenKind::Identifier)?.value.unwrap();

        if name.starts_with(|c: char| c.is_ascii_lowercase()) {
            let name = self.parse_identifier()?;

            Ok(Pattern::Identifier(name))
        } else {
            let path = self.parse_path()?;
            let fields = if self.peek(TokenKind::LeftParen) {
                self.consume_paren_seq(Parser::parse_pattern)?
            } else {
                Vec::new()
            };

            let start = path.location.start();
            let end = self.token().end();

            Ok(Pattern::Variant(VariantPattern {
                name: path,
                fields,
                location: (start..end).into(),
            }))
        }
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_wildcard_pattern(&mut self) -> Result<Pattern> {
        let location = self.consume(TokenKind::DotDot)?.index;

        Ok(Pattern::Wildcard(WildcardPattern {
            location: location.into(),
        }))
    }
}
