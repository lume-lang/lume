use crate::*;

impl Parser {
    pub(crate) fn parse_attributes(&mut self) -> bool {
        self.consume_any_seq(Token![!], Parser::parse_attribute)
    }

    fn parse_attribute(&mut self) {
        self.start_node(SyntaxKind::ATTR);

        self.consume(Token![!]);
        self.consume(SyntaxKind::LEFT_BRACKET);

        self.parse_name();
        self.parse_attribute_arguments();

        self.consume(SyntaxKind::RIGHT_BRACKET);

        self.finish_node();
    }

    fn parse_attribute_arguments(&mut self) {
        self.start_node(SyntaxKind::ATTR_ARG_LIST);

        self.consume_comma_seq(
            SyntaxKind::LEFT_PAREN,
            SyntaxKind::RIGHT_PAREN,
            Parser::parse_attribute_argument,
        );

        self.finish_node();
    }

    fn parse_attribute_argument(&mut self) {
        self.start_node(SyntaxKind::ATTR_ARG);

        self.parse_name();
        self.consume(Token![=]);
        self.parse_literal();

        self.finish_node();
    }
}
