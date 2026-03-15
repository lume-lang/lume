use crate::*;

const ITEM_RECOVERY_SET: &[SyntaxKind] = &[
    Token![namespace],
    Token![import],
    Token![impl],
    Token![fn],
    Token![struct],
    Token![trait],
    Token![enum],
    Token![pub],
    Token![priv],
    Token![use],
];

impl Parser {
    pub(crate) fn parse_item(&mut self) {
        let c = self.checkpoint();

        self.parse_doc_comment();
        self.parse_attributes();

        self.parse_visibility();

        match self.token() {
            Token![import] => self.parse_import(),
            Token![namespace] => self.parse_namespace(),
            Token![impl] => self.parse_implementation(c),
            Token![fn] => self.parse_function(c),
            Token![struct] => self.parse_struct_definition(c),
            Token![trait] => self.parse_trait_definition(c),
            Token![enum] => self.parse_enum_definition(c),
            Token![use] => self.parse_trait_implementation(c),
            _ => {
                self.error_and_recover("invalid top-level item", ITEM_RECOVERY_SET);
            }
        }
    }

    fn parse_visibility(&mut self) {
        match self.token() {
            Token![pub] => {
                self.start_node(SyntaxKind::VISIBILITY);
                self.consume(Token![pub]);

                if self.check(SyntaxKind::LEFT_PAREN) {
                    self.consume(Token![internal]);
                    self.consume(SyntaxKind::RIGHT_PAREN);
                }

                self.finish_node();
            }
            Token![priv] => {
                self.start_node(SyntaxKind::VISIBILITY);
                self.consume(Token![priv]);
                self.finish_node();
            }
            _ => {}
        }
    }

    fn parse_import(&mut self) {
        self.start_node(SyntaxKind::IMPORT);

        self.consume(Token![import]);

        self.start_node(SyntaxKind::IMPORT_PATH);
        self.parse_import_path();
        self.finish_node();

        if !self.peek(SyntaxKind::LEFT_PAREN) {
            self.error_and_recover("invalid import path", ITEM_RECOVERY_SET);
            return;
        }

        self.start_node(SyntaxKind::IMPORT_LIST);
        self.consume_paren_seq(Parser::parse_identifier);
        self.finish_node();

        self.finish_node();
    }

    fn parse_namespace(&mut self) {
        self.start_node(SyntaxKind::NAMESPACE);

        self.consume(Token![namespace]);
        self.parse_import_path();

        self.finish_node();
    }

    fn parse_signature(&mut self) {
        self.consume(Token![fn]);
        let is_external = self.check(Token![external]);

        self.parse_name();

        self.parse_type_parameters();
        self.parse_parameters();
        self.parse_return_type();

        if is_external && self.peek(SyntaxKind::LEFT_BRACE) {
            self.error("external items cannot have bodies");
            self.recover_statement();
        }
    }

    fn parse_parameters(&mut self) {
        self.start_node(SyntaxKind::PARAM_LIST);
        self.consume_paren_seq(Parser::parse_parameter);
        self.finish_node();
    }

    fn parse_parameter(&mut self) {
        self.start_node(SyntaxKind::PARAM);

        if self.check(Token![self]) {
            return;
        }

        self.check(Token![...]);

        self.parse_name();
        self.check(Token![:]);

        self.start_node(SyntaxKind::PARAM_TYPE);
        self.parse_type();
        self.finish_node();

        self.finish_node();
    }

    /// Parses the return type of the current function definition.
    fn parse_return_type(&mut self) {
        if !self.peek(Token![->]) {
            return;
        }

        self.start_node(SyntaxKind::RETURN_TYPE);

        self.consume(Token![->]);
        self.parse_type();

        self.finish_node();
    }

    fn parse_function(&mut self, c: Checkpoint) {
        self.start_node_at(SyntaxKind::FN, c);

        self.parse_signature();
        if self.peek(SyntaxKind::LEFT_BRACE) {
            self.parse_block();
        }

        self.finish_node();
    }

    fn parse_struct_definition(&mut self, c: Checkpoint) {
        self.start_node_at(SyntaxKind::STRUCT, c);

        self.consume(Token![struct]);
        self.parse_identifier();
        self.parse_type_parameters();

        self.consume_curly_seq(Parser::parse_struct_field);

        self.finish_node();
    }

    fn parse_struct_field(&mut self) {
        self.start_node(SyntaxKind::FIELD);

        self.parse_doc_comment();
        self.parse_attributes();

        self.parse_visibility();
        self.parse_identifier();

        // Report a special error if we found an identifier, such as
        // a method declaration, which isn't allowed within a `struct` block.
        if !self.check(Token![:]) && self.peek(SyntaxKind::IDENT) {
            self.error("methods can only be defined in `impl` blocks");
            self.recover_statement();
            self.finish_node();

            return;
        }

        self.parse_type();

        if self.check(Token![=]) {
            self.parse_expression(None);
        }

        self.consume(Token![;]);
        self.finish_node();
    }

    fn parse_implementation(&mut self, c: Checkpoint) {
        self.start_node_at(SyntaxKind::IMPL, c);

        self.consume(Token![impl]);
        self.parse_type_parameters();

        self.parse_type();
        self.consume_curly_seq(Parser::parse_method_definition);

        self.finish_node();
    }

    fn parse_method_definition(&mut self) {
        self.start_node(SyntaxKind::METHOD);

        self.parse_doc_comment();
        self.parse_attributes();

        self.parse_visibility();

        self.parse_signature();
        if self.peek(SyntaxKind::LEFT_BRACE) {
            self.parse_block();
        }

        self.finish_node();
    }

    fn parse_trait_definition(&mut self, c: Checkpoint) {
        self.start_node_at(SyntaxKind::TRAIT, c);

        self.consume(Token![trait]);
        self.parse_identifier();

        self.parse_type_parameters();
        self.consume_curly_seq(Parser::parse_trait_method);

        self.finish_node();
    }

    fn parse_trait_method(&mut self) {
        self.start_node(SyntaxKind::METHOD);

        self.parse_doc_comment();
        self.parse_attributes();

        self.parse_signature();

        if !self.check(Token![;]) {
            self.parse_block();
        }

        self.finish_node();
    }

    fn parse_trait_implementation(&mut self, c: Checkpoint) {
        self.start_node_at(SyntaxKind::TRAIT_IMPL, c);

        self.consume(Token![use]);
        self.parse_type_parameters();

        self.parse_type();

        self.consume(Token![:]);
        self.parse_type();

        self.consume_curly_seq(Parser::parse_trait_method_implementation);

        self.finish_node();
    }

    fn parse_trait_method_implementation(&mut self) {
        self.start_node(SyntaxKind::METHOD);

        self.parse_doc_comment();
        self.parse_attributes();

        self.parse_signature();

        if self.peek(SyntaxKind::LEFT_BRACE) {
            self.parse_block();
        }

        self.finish_node();
    }

    /// Parses a single enum type definition, such as:
    ///
    /// ```lm
    /// enum IpAddrKind {
    ///   V4,
    ///   V6,
    /// }
    /// ```
    fn parse_enum_definition(&mut self, c: Checkpoint) {
        self.start_node_at(SyntaxKind::ENUM, c);

        self.consume(Token![enum]);

        self.parse_name();
        self.parse_type_parameters();
        self.consume_comma_seq(SyntaxKind::LEFT_BRACE, SyntaxKind::RIGHT_BRACE, Parser::parse_enum_case);

        self.finish_node();
    }

    /// Parses a single enum type case, such as `V4` or `V4(String)`.
    fn parse_enum_case(&mut self) {
        self.start_node(SyntaxKind::CASE);

        self.parse_doc_comment();
        self.parse_attributes();

        self.parse_identifier();

        if self.peek(SyntaxKind::LEFT_PAREN) {
            self.start_node(SyntaxKind::PARAM_LIST);
            self.consume_paren_seq(Parser::parse_type);
            self.finish_node();
        }

        self.finish_node();
    }
}
