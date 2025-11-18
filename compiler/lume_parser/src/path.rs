use error_snippet::Result;
use lume_ast::*;
use lume_lexer::TokenType;

use crate::Parser;

const LOWERCASE_PATH_TYPES: &[&str] = &["void", "self"];

impl Parser<'_> {
    /// Parses the next token as a symbol path.
    #[libftrace::traced(level = Trace)]
    pub(crate) fn parse_path(&mut self) -> Result<Path> {
        let mut segments = Vec::new();

        loop {
            segments.push(self.parse_path_segment(segments.last())?);

            if self.consume_if(TokenType::PathSeparator).is_none() {
                break;
            }
        }

        let (name, root) = segments.split_last().unwrap();

        Ok(Path {
            name: name.to_owned(),
            root: root.to_owned(),
            location: name.location().clone(),
        })
    }

    /// Parses the next token as a single segment of a symbol path.
    #[libftrace::traced(level = Trace)]
    fn parse_path_segment(&mut self, prev_segment: Option<&PathSegment>) -> Result<PathSegment> {
        let name = self.parse_identifier()?;
        let start = name.location.start();

        // If the name starts with a lower case, it can refer to either
        // a namespace or a function call.
        //
        // We also make sure the name is not referring to a scalar type, such as `void`,
        // since it would parse that as a namespace segment, while it's meant to
        // be a type segment.
        if name.is_lower() && !LOWERCASE_PATH_TYPES.contains(&name.as_str()) {
            // If we spot a `<` token, we know it's the start of a type argument list,
            // which can only be defined on function calls.
            if self.peek(TokenType::Less) || self.peek(TokenType::LeftParen) {
                let type_arguments = self.parse_type_arguments()?;
                let end = self.previous_token().end();

                return Ok(PathSegment::Callable {
                    name,
                    type_arguments,
                    location: (start..end).into(),
                });
            }

            // Otherwise, it must be a namespace.
            Ok(PathSegment::namespace(name))
        }
        // If the name starts with an upper case, it refers to a type.
        else {
            let is_after_type_segment = matches!(prev_segment, Some(PathSegment::Type { .. }));

            // If we spot a `(` token, we know it's the start of an argument list,
            // which can only be defined on variant expressions.
            if self.peek(TokenType::LeftParen) || is_after_type_segment {
                let end = self.previous_token().end();

                return Ok(PathSegment::Variant {
                    name,
                    location: (start..end).into(),
                });
            }

            let type_arguments = self.parse_type_arguments()?;
            let end = self.previous_token().end();

            Ok(PathSegment::Type {
                name,
                type_arguments,
                location: (start..end).into(),
            })
        }
    }
}
