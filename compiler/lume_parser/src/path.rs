use crate::Parser;
use error_snippet::Result;
use lume_ast::*;
use lume_lexer::{IDENTIFIER_SEPARATOR, TokenKind};

const LOWERCASE_PATH_TYPES: &[&str] = &["void", "self"];

impl Parser {
    /// Parses the next token as a symbol path.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub(crate) fn parse_path(&mut self) -> Result<Path> {
        let segments = self.consume_delim(IDENTIFIER_SEPARATOR, Parser::parse_path_segment)?;
        let (name, root) = segments.split_last().unwrap();

        Ok(Path {
            name: name.to_owned(),
            root: root.to_owned(),
            location: name.location().clone(),
        })
    }

    /// Parses the next token as a single segment of a symbol path.
    #[tracing::instrument(level = "TRACE", skip(self))]
    fn parse_path_segment(&mut self) -> Result<PathSegment> {
        let name = self.parse_identifier()?;
        let start = name.location.start();

        // If the name starts with a lower case, it can refer to either
        // a namespace or a function call.
        //
        // We also make sure the name is not referring to a scalar type, such as `void`, since
        // it would parse that as a namespace segment, while it's meant to be a type segment.
        if name.is_lower() && !LOWERCASE_PATH_TYPES.contains(&name.as_str()) {
            // If we spot a `<` token, we know it's the start of a type argument list,
            // which can only be defined on function calls.
            if self.peek(TokenKind::Less) || self.peek(TokenKind::LeftParen) {
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
