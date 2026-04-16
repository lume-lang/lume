#[macro_export]
macro_rules! Token {
    [+] => { $crate::SyntaxKind::ADD };
    [+=] => { $crate::SyntaxKind::ADDASSIGN };
    [-] => { $crate::SyntaxKind::SUB };
    [-=] => { $crate::SyntaxKind::SUBASSIGN };
    [*] => { $crate::SyntaxKind::MUL };
    [*=] => { $crate::SyntaxKind::MULASSIGN };
    [/] => { $crate::SyntaxKind::DIV };
    [/=] => { $crate::SyntaxKind::DIVASSIGN };
    [++] => { $crate::SyntaxKind::INCREMENT };
    [--] => { $crate::SyntaxKind::DECREMENT };
    [>] => { $crate::SyntaxKind::GREATER };
    [>=] => { $crate::SyntaxKind::GEQUAL };
    [<] => { $crate::SyntaxKind::LESS };
    [<=] => { $crate::SyntaxKind::LEQUAL };
    [==] => { $crate::SyntaxKind::EQUAL };
    [!=] => { $crate::SyntaxKind::NEQUAL };
    [=] => { $crate::SyntaxKind::ASSIGN };
    [!] => { $crate::SyntaxKind::NOT };
    [&&] => { $crate::SyntaxKind::AND };
    [||] => { $crate::SyntaxKind::OR };
    [&] => { $crate::SyntaxKind::BINARY_AND };
    [|] => { $crate::SyntaxKind::BINARY_OR };
    [^] => { $crate::SyntaxKind::BINARY_XOR };
    [_] => { $crate::SyntaxKind::UNDERSCORE };

    // Punctuation
    [:] => { $crate::SyntaxKind::COLON };
    [;] => { $crate::SyntaxKind::SEMICOLON };
    [.] => { $crate::SyntaxKind::DOT };
    [..] => { $crate::SyntaxKind::DOT_DOT };
    [...] => { $crate::SyntaxKind::DOT_DOT_DOT };
    [,] => { $crate::SyntaxKind::COMMA };
    [->] => { $crate::SyntaxKind::ARROW };
    [=>] => { $crate::SyntaxKind::BIG_ARROW };
    [::] => { $crate::SyntaxKind::PATH_SEP };
    [?] => { $crate::SyntaxKind::QUESTION };

    // Keywords
    [as] => { $crate::SyntaxKind::AS_KW };
    [break] => { $crate::SyntaxKind::BREAK_KW };
    [continue] => { $crate::SyntaxKind::CONTINUE_KW };
    [else] => { $crate::SyntaxKind::ELSE_KW };
    [enum] => { $crate::SyntaxKind::ENUM_KW };
    [external] => { $crate::SyntaxKind::EXTERN_KW };
    [false] => { $crate::SyntaxKind::FALSE_KW };
    [fn] => { $crate::SyntaxKind::FN_KW };
    [for] => { $crate::SyntaxKind::FOR_KW };
    [if] => { $crate::SyntaxKind::IF_KW };
    [is] => { $crate::SyntaxKind::IS_KW };
    [impl] => { $crate::SyntaxKind::IMPL_KW };
    [import] => { $crate::SyntaxKind::IMPORT_KW };
    [in] => { $crate::SyntaxKind::IN_KW };
    [internal] => { $crate::SyntaxKind::INTERNAL_KW };
    [let] => { $crate::SyntaxKind::LET_KW };
    [loop] => { $crate::SyntaxKind::LOOP_KW };
    [namespace] => { $crate::SyntaxKind::NAMESPACE_KW };
    [priv] => { $crate::SyntaxKind::PRIV_KW };
    [pub] => { $crate::SyntaxKind::PUB_KW };
    [return] => { $crate::SyntaxKind::RETURN_KW };
    [self] => { $crate::SyntaxKind::SELF_EXPR };
    [Self] => { $crate::SyntaxKind::SELF_TYPE };
    [struct] => { $crate::SyntaxKind::STRUCT_KW };
    [switch] => { $crate::SyntaxKind::SWITCH_KW };
    [trait] => { $crate::SyntaxKind::TRAIT_KW };
    [true] => { $crate::SyntaxKind::TRUE_KW };
    [unsafe] => { $crate::SyntaxKind::UNSAFE_KW };
    [use] => { $crate::SyntaxKind::USE_KW };
    [while] => { $crate::SyntaxKind::WHILE_KW };
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub enum SyntaxKind {
    WHITESPACE = 0,
    NEWLINE,

    SOURCE_FILE,
    IDENT,
    VISIBILITY,
    NAME,
    SIG,
    PARAM_LIST,
    PARAM,
    RETURN_TYPE,
    ARG_LIST,
    IMPORT_PATH,
    IMPORT_LIST,
    ATTR,
    ATTR_ARG_LIST,
    ATTR_ARG,

    CONSTRUCTOR_FIELD,
    CONDITION,
    SWITCH_ARM,

    CASE,
    CASE_PARAM_LIST,

    // Items
    FN,
    IMPL,
    NAMESPACE,
    IMPORT,
    STRUCT,
    TRAIT,
    ENUM,
    TRAIT_IMPL,
    METHOD,
    FIELD,
    BLOCK,

    // Comments
    LINE_COMMENT,
    DOC_COMMENTS,
    DOC_COMMENT,

    // Statements
    LET_STMT,
    BREAK_STMT,
    CONTINUE_STMT,
    RETURN_STMT,
    LOOP_STMT,
    FOR_STMT,
    WHILE_STMT,
    FINAL_STMT,
    EXPR_STMT,

    // Expressions
    ARRAY_EXPR,
    ASSIGNMENT_EXPR,
    REF_EXPR,
    DEREF_EXPR,
    INSTANCE_CALL_EXPR,
    STATIC_CALL_EXPR,
    CAST_EXPR,
    CONSTRUCT_EXPR,
    IF_EXPR,
    IS_EXPR,
    LIT_EXPR,
    MEMBER_EXPR,
    RANGE_EXPR,
    SCOPE_EXPR,
    SWITCH_EXPR,
    VARIABLE_EXPR,
    VARIANT_EXPR,
    PAREN_EXPR,
    SELF_EXPR,
    UNSAFE_EXPR,

    BIN_EXPR,
    POSTFIX_EXPR,
    UNARY_EXPR,

    // Literals
    BOOLEAN_LIT,
    INTEGER_LIT,
    FLOAT_LIT,
    STRING_LIT,

    // Types
    NAMED_TYPE,
    ARRAY_TYPE,
    SELF_TYPE,
    POINTER_TYPE,

    // Symbols
    ADD,
    ADDASSIGN,
    SUB,
    SUBASSIGN,
    MUL,
    MULASSIGN,
    DIV,
    DIVASSIGN,
    INCREMENT,
    DECREMENT,
    GREATER,
    GEQUAL,
    LESS,
    LEQUAL,
    EQUAL,
    NEQUAL,
    ASSIGN,
    NOT,
    AND,
    OR,
    BINARY_AND,
    BINARY_OR,
    BINARY_XOR,
    UNDERSCORE,

    // Brackets
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    LEFT_BRACKET,
    RIGHT_BRACKET,

    // Path
    PATH,
    PATH_SEGMENT,
    PATH_NAMESPACE,
    PATH_VARIANT,
    PATH_CALLABLE,
    PATH_TYPE,

    // Pattern
    PAT_IDENT,
    PAT_LITERAL,
    PAT_VARIANT,
    PAT_WILDCARD,

    // Punctuation
    COLON,
    SEMICOLON,
    DOT,
    DOT_DOT,
    DOT_DOT_DOT,
    COMMA,
    ARROW,
    BIG_ARROW,
    PATH_SEP,
    QUESTION,

    // Generics
    BOUND_TYPES,
    BOUND_TYPE,
    CONSTRAINTS,

    GENERIC_ARGS,

    // Keywords
    AS_KW,
    BREAK_KW,
    CONTINUE_KW,
    ELSE_KW,
    ENUM_KW,
    EXTERN_KW,
    FALSE_KW,
    FN_KW,
    FOR_KW,
    IF_KW,
    IS_KW,
    IMPL_KW,
    IMPORT_KW,
    IN_KW,
    INTERNAL_KW,
    LET_KW,
    LOOP_KW,
    NAMESPACE_KW,
    PRIV_KW,
    PUB_KW,
    RETURN_KW,
    STRUCT_KW,
    SWITCH_KW,
    TRAIT_KW,
    TRUE_KW,
    UNSAFE_KW,
    USE_KW,
    WHILE_KW,

    EOF,
}

impl SyntaxKind {
    #[inline]
    pub fn is_trivia(self) -> bool {
        matches!(self, Self::WHITESPACE | Self::LINE_COMMENT)
    }

    #[inline]
    pub fn is_keyword(self) -> bool {
        matches!(
            self,
            SyntaxKind::AS_KW
                | SyntaxKind::BREAK_KW
                | SyntaxKind::CONTINUE_KW
                | SyntaxKind::ELSE_KW
                | SyntaxKind::EXTERN_KW
                | SyntaxKind::FALSE_KW
                | SyntaxKind::FN_KW
                | SyntaxKind::FOR_KW
                | SyntaxKind::IF_KW
                | SyntaxKind::IMPL_KW
                | SyntaxKind::IMPORT_KW
                | SyntaxKind::IN_KW
                | SyntaxKind::INTERNAL_KW
                | SyntaxKind::IS_KW
                | SyntaxKind::LOOP_KW
                | SyntaxKind::NAMESPACE_KW
                | SyntaxKind::PRIV_KW
                | SyntaxKind::PUB_KW
                | SyntaxKind::RETURN_KW
                | SyntaxKind::SELF_EXPR
                | SyntaxKind::SELF_TYPE
                | SyntaxKind::STRUCT_KW
                | SyntaxKind::SWITCH_KW
                | SyntaxKind::TRAIT_KW
                | SyntaxKind::TRUE_KW
                | SyntaxKind::USE_KW
                | SyntaxKind::WHILE_KW
        )
    }

    /// Gets the precedence of the token kind.
    ///
    /// Returns the precedence of the token kind, or 0 if the token kind is not
    /// an operator.
    #[inline]
    pub fn precedence(&self) -> u8 {
        OPERATOR_PRECEDENCE
            .iter()
            .find(|(k, _)| k == self)
            .map_or(0, |(_, p)| *p)
    }

    #[inline]
    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            SyntaxKind::INTEGER_LIT
                | SyntaxKind::FLOAT_LIT
                | SyntaxKind::STRING_LIT
                | SyntaxKind::BOOLEAN_LIT
                | SyntaxKind::TRUE_KW
                | SyntaxKind::FALSE_KW
        )
    }

    #[inline]
    pub fn is_unary(&self) -> bool {
        matches!(self, SyntaxKind::NOT | SyntaxKind::SUB)
    }
}

/// Defines the precedence for unary operators, such as `-` or `!`.
///
/// They cannot be defined within [`OPERATOR_PRECEDENCE`], as unary operators
/// share the same operators as other operators, but have different meanings.
///
/// For example, `-` can mean both "minus some amount", but when it's within
/// it's own expression before a number expression, it'd mean "the negative of
/// the following expression".
pub const UNARY_PRECEDENCE: u8 = 3;

/// Defines the precedence for the dereference operator, `*`.
pub const DEREF_PRECEDENCE: u8 = 9;

pub const OPERATOR_PRECEDENCE: &[(SyntaxKind, u8)] = &[
    (Token![=], 1),
    (Token![+=], 1),
    (Token![-=], 1),
    (Token![*=], 1),
    (Token![/=], 1),
    (Token![as], 2),
    (Token![is], 2),
    (Token![&], 3),
    (Token![^], 3),
    (Token![|], 3),
    (Token![&&], 3),
    (Token![||], 3),
    (Token![==], 4),
    (Token![!=], 4),
    (Token![<], 5),
    (Token![>], 5),
    (Token![<=], 6),
    (Token![>=], 6),
    (Token![+], 7),
    (Token![-], 7),
    (Token![*], 8),
    (Token![/], 8),
    (Token![!], 9),
    (Token![++], 9),
    (Token![--], 9),
    (Token![.], 10),
    (Token![..], 10),
    (Token![::], 11),
];

/// Defines all the operators which are notated as postfix, as opposed to infix.
pub const POSTFIX_OPERATORS: &[SyntaxKind] = &[Token![++], Token![--]];

/// Defines all the operators which are notated as infix, as opposed to postfix.
pub const INFIX_OPERATORS: &[SyntaxKind] = &[
    Token![&],
    Token![|],
    Token![^],
    Token![+],
    Token![-],
    Token![*],
    Token![/],
    Token![&&],
    Token![||],
    Token![==],
    Token![!=],
    Token![<],
    Token![<=],
    Token![>],
    Token![>=],
];

/// Defines all the operators which are used in binary contexts.
pub const BINARY_OPERATORS: &[SyntaxKind] = &[Token![&], Token![|], Token![^]];

/// Defines all the operators which are used in boolean contexts.
pub const BOOLEAN_OPERATORS: &[SyntaxKind] = &[Token![&&], Token![||]];

/// Defines all the operators which are used in comparison contexts.
pub const COMPARISON_OPERATORS: &[SyntaxKind] = &[Token![==], Token![!=], Token![<], Token![<=], Token![>], Token![>=]];

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}
