use lume_syntax::Token;

use crate::*;

#[inline]
pub(crate) fn child<N: AstNode>(parent: &lume_syntax::SyntaxNode) -> Option<N> {
    parent.children().find_map(N::cast)
}

#[inline]
pub(crate) fn children<N: AstNode>(parent: &lume_syntax::SyntaxNode) -> AstChildren<N> {
    AstChildren::new(parent)
}

#[inline]
pub(crate) fn token(
    parent: &lume_syntax::SyntaxNode,
    kind: lume_syntax::SyntaxKind,
) -> Option<lume_syntax::SyntaxToken> {
    parent
        .children_with_tokens()
        .filter_map(|it| it.into_token())
        .find(|it| it.kind() == kind)
}

#[inline]
pub(crate) fn tokens(
    parent: &lume_syntax::SyntaxNode,
    kind: lume_syntax::SyntaxKind,
) -> impl Iterator<Item = lume_syntax::SyntaxToken> {
    parent
        .children_with_tokens()
        .filter_map(|it| it.into_token())
        .filter(move |it| it.kind() == kind)
}

pub trait WithDocumentation {
    fn documentation(&self) -> impl Iterator<Item = String>;
}

macro_rules! with_doc_types {
    ($($ty:ty),*) => {
        $(
            impl WithDocumentation for $ty {
                fn documentation(&self) -> impl Iterator<Item = String> {
                    tokens(self.syntax(), lume_syntax::SyntaxKind::DOC_COMMENT).map(|tok| tok.text().to_string())
                }
            }
        )*
    };
}

with_doc_types! {
    Fn, Struct, Field, Trait, Enum, Case, Impl, TraitImpl, Method
}

impl crate::Name {
    pub fn is_lower(&self) -> bool {
        self.syntax()
            .text()
            .char_at(lume_syntax::TextSize::new(0))
            .is_none_or(|c| c.is_ascii_lowercase())
    }
}

impl crate::TraitImpl {
    pub fn trait_type(&self) -> Option<crate::Type> {
        children(self.syntax()).nth(0)
    }

    pub fn target_type(&self) -> Option<crate::Type> {
        children(self.syntax()).nth(1)
    }
}

impl crate::Param {
    pub fn is_self(&self) -> bool {
        self.name().is_some_and(|name| name.syntax().text() == "self")
    }
}

impl crate::AssignmentExpr {
    pub fn lhs(&self) -> Option<crate::Expr> {
        children(self.syntax()).nth(0)
    }

    pub fn rhs(&self) -> Option<crate::Expr> {
        children(self.syntax()).nth(1)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic intrinsics
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Negate,

    // Logical intrinsics
    BinaryAnd,
    BinaryOr,
    BinaryXor,
    Not,

    // Comparison intrinsics
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

impl crate::BinExpr {
    pub fn lhs(&self) -> Option<crate::Expr> {
        children(self.syntax()).nth(0)
    }

    pub fn rhs(&self) -> Option<crate::Expr> {
        children(self.syntax()).nth(1)
    }

    pub fn op(&self) -> Option<BinaryOp> {
        let op = self
            .syntax()
            .children_with_tokens()
            .filter_map(|child| child.into_token())
            .filter(|tok| !tok.kind().is_trivia())
            .collect::<Vec<_>>();

        let tok1 = op.first().map(|tok| tok.kind());
        let tok2 = op.get(1).map(|tok| tok.kind());

        match (tok1, tok2) {
            // Arithmetic intrinsics
            (Some(Token![+]), _) => Some(BinaryOp::Add),
            (Some(Token![-]), _) => Some(BinaryOp::Sub),
            (Some(Token![*]), _) => Some(BinaryOp::Mul),
            (Some(Token![/]), _) => Some(BinaryOp::Div),
            (Some(Token![&]), Some(Token![&])) => Some(BinaryOp::And),
            (Some(Token![|]), Some(Token![|])) => Some(BinaryOp::Or),

            // Logical intrinsics
            (Some(Token![&]), _) => Some(BinaryOp::BinaryAnd),
            (Some(Token![|]), _) => Some(BinaryOp::BinaryOr),
            (Some(Token![^]), _) => Some(BinaryOp::BinaryXor),

            // Comparison intrinsics
            (Some(Token![==]), _) => Some(BinaryOp::Equal),
            (Some(Token![!=]), _) => Some(BinaryOp::NotEqual),
            (Some(Token![<=]), _) => Some(BinaryOp::LessEqual),
            (Some(Token![<]), _) => Some(BinaryOp::Less),
            (Some(Token![>=]), _) => Some(BinaryOp::GreaterEqual),
            (Some(Token![>]), _) => Some(BinaryOp::Greater),

            _ => None,
        }
    }
}

impl crate::RangeExpr {
    pub fn lower(&self) -> Option<crate::Expr> {
        children(self.syntax()).nth(0)
    }

    pub fn upper(&self) -> Option<crate::Expr> {
        children(self.syntax()).nth(1)
    }
}

#[derive(Default, Debug, PartialEq, Eq, Clone, Copy)]
pub enum Radix {
    Binary = 2,
    Octal = 8,
    #[default]
    Decimal = 10,
    Hexadecimal = 16,
}

impl crate::IntegerLit {
    pub fn as_parts(&self) -> (Radix, String, Option<IntKind>) {
        let text = self.syntax().text().to_string();

        let radix = match text.get(..2) {
            Some("0b" | "0B") => Some(Radix::Binary),
            Some("0o" | "0O") => Some(Radix::Octal),
            Some("0x" | "0X") => Some(Radix::Hexadecimal),
            Some("0d" | "0D") => Some(Radix::Decimal),
            Some(_) | None => None,
        };

        let (_radix_str, mut text) = text.split_at(if radix.is_some() { 2 } else { 0 });

        let is_suffix_start: fn(&(usize, char)) -> bool = match radix {
            Some(Radix::Hexadecimal) => |(_, c)| matches!(c, 'g'..='z' | 'G'..='Z'),
            _ => |(_, c)| c.is_ascii_alphabetic(),
        };

        let mut suffix = "";
        if let Some((suffix_start, _)) = text.char_indices().find(is_suffix_start) {
            let (text2, suffix2) = text.split_at(suffix_start);
            text = text2;
            suffix = suffix2;
        }

        let kind = match suffix {
            "u8" => Some(IntKind::U8),
            "u16" => Some(IntKind::U16),
            "u32" => Some(IntKind::U32),
            "u64" => Some(IntKind::U64),
            "i8" => Some(IntKind::I8),
            "i16" => Some(IntKind::I16),
            "i32" => Some(IntKind::I32),
            "i64" => Some(IntKind::I64),
            _ => None,
        };

        (radix.unwrap_or_default(), text.to_string(), kind)
    }
}

impl crate::FloatLit {
    pub fn as_parts(&self) -> (String, Option<FloatKind>) {
        let mut value = self.syntax().text().to_string();

        // Remove all underscores from the literal
        while let Some(idx) = value.find('_') {
            value.remove(idx);
        }

        let kind = if value.ends_with("f32") {
            Some(FloatKind::F32)
        } else if value.ends_with("f64") {
            Some(FloatKind::F64)
        } else {
            None
        };

        // Remove the literal kind from the value, since they cannot be parsed.
        if kind.is_some() {
            value.truncate(value.len() - 3);
        }

        (value, kind)
    }
}

impl crate::Param {
    #[inline]
    pub fn is_self_type(&self) -> bool {
        self.ty().is_some_and(|ty| ty.is_self())
    }
}

impl crate::Type {
    #[inline]
    pub fn is_self(&self) -> bool {
        matches!(self, crate::Type::SelfType(_))
    }
}

impl crate::PathSegment {
    #[inline]
    pub fn is_self_type(&self) -> bool {
        if let crate::PathSegment::PathType(seg) = self {
            seg.name().is_some_and(|ident| ident.syntax().text() == "Self")
        } else {
            false
        }
    }

    pub fn name(&self) -> String {
        match self {
            PathSegment::PathNamespace(_) | PathSegment::PathVariant(_) => self.as_text(),
            PathSegment::PathCallable(p) => p
                .name()
                .map_or_else(|| String::from("[missing name]"), |name| name.as_text()),
            PathSegment::PathType(p) => p
                .name()
                .map_or_else(|| String::from("[missing name]"), |name| name.as_text()),
        }
    }
}

impl crate::Path {
    /// Returns whether the current path has any root segments.
    pub fn has_root(&self) -> bool {
        self.path_segment().count() > 1
    }

    pub fn split_root(&self) -> Option<(Vec<crate::PathSegment>, crate::PathSegment)> {
        let segments = self.path_segment().collect::<Vec<_>>();
        let (name, root) = segments.split_last()?;

        Some((root.to_vec(), name.to_owned()))
    }
}
