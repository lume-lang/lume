use error_snippet::Result;
use lume_ast::*;
use lume_lexer::{TokenKind, TokenType};

use crate::Parser;
use crate::errors::*;

impl Parser<'_> {
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub(super) fn parse_pattern(&mut self) -> Result<Pattern> {
        match self.token().kind {
            TokenKind::Identifier(_) => self.parse_named_pattern(),
            TokenKind::DotDot => self.parse_wildcard_pattern(),

            k if k.is_literal() => self.parse_literal_pattern(),

            k => Err(InvalidPattern {
                source: self.source.clone(),
                range: self.token().index,
                found: k.as_type(),
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
        let path = self.parse_path()?;

        if path.root.is_empty() && path.name.name().is_lower() {
            Ok(Pattern::Identifier(path.name.name().clone()))
        } else {
            let fields = if self.peek(TokenType::LeftParen) {
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
        let location = self.consume(TokenType::DotDot)?.index;

        Ok(Pattern::Wildcard(WildcardPattern {
            location: location.into(),
        }))
    }
}
