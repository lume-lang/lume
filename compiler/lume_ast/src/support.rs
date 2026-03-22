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

impl crate::Name {
    pub fn is_lower(&self) -> bool {
        self.syntax()
            .text()
            .char_at(lume_syntax::TextSize::new(0))
            .is_none_or(|c| c.is_ascii_lowercase())
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
        let text = self.syntax().text().to_string();
        let len = text.len().saturating_sub(1);

        let kind = match text.get(len - 3..).unwrap_or_default() {
            "f32" => Some(FloatKind::F32),
            "f64" => Some(FloatKind::F64),
            _ => None,
        };

        let text = text[..len - 3].to_string();
        (text, kind)
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
