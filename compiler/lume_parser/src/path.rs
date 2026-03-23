use crate::*;

const LOWERCASE_PATH_TYPES: &[&str] = &["void", "self"];

impl Parser {
    /// Parses the next token as a symbol path.
    pub(crate) fn parse_path(&mut self) -> SyntaxKind {
        let mut prev_kind = None;

        self.start_node(SyntaxKind::PATH);

        loop {
            prev_kind = Some(self.parse_path_segment(prev_kind));

            if !self.check(Token![::]) {
                break;
            }
        }

        self.finish_node();

        prev_kind.unwrap()
    }

    /// Parses the next token as a single segment of a symbol path.
    fn parse_path_segment(&mut self, prev_kind: Option<SyntaxKind>) -> SyntaxKind {
        let c = self.checkpoint();
        let ident_slice = self.content_at(self.span()).to_string();

        self.parse_ident();

        // If the name starts with a lower case, it can refer to either
        // a namespace or a function call.
        //
        // We also make sure the name is not referring to a scalar type, such as `void`,
        // since it would parse that as a namespace segment, while it's meant to
        // be a type segment.
        if ident_slice.starts_with(|c: char| c.is_ascii_lowercase())
            && !LOWERCASE_PATH_TYPES.contains(&ident_slice.as_str())
        {
            // If we spot a `<` token, we know it's the start of a type argument list,
            // which can only be defined on function calls.
            if self.peek(Token![<]) || self.peek(SyntaxKind::LEFT_PAREN) {
                self.start_node_at(SyntaxKind::PATH_CALLABLE, c);
                self.parse_type_arguments();
                self.finish_node();

                return SyntaxKind::PATH_CALLABLE;
            }

            // Otherwise, it must be a namespace.
            self.complete_node(SyntaxKind::PATH_NAMESPACE, c)
        }
        // If the name starts with an upper case, it refers to a type.
        else {
            // If we spot a `(` token, we know it's the start of an argument list,
            // which can only be defined on variant expressions.
            if self.peek(SyntaxKind::LEFT_PAREN) || prev_kind == Some(SyntaxKind::PATH_TYPE) {
                return self.complete_node(SyntaxKind::PATH_VARIANT, c);
            }

            self.start_node_at(SyntaxKind::PATH_TYPE, c);
            self.parse_type_arguments();
            self.finish_node();

            SyntaxKind::PATH_TYPE
        }
    }

    /// Parses the next token(s) as a namespace path.
    ///
    /// Identifier paths are much like regular identifiers, but can be joined
    /// together with periods, to form longer chains of them. They can be as
    /// short as a single link, such as `std`, but they can also be longer,
    /// such as `std::fmt::error`.
    pub(crate) fn parse_import_path(&mut self) {
        self.start_node(SyntaxKind::IMPORT_PATH);
        self.consume_delim(Token![::], Parser::parse_ident);
        self.finish_node();
    }
}
