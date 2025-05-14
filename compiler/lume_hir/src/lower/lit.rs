use error_snippet::Result;

use crate::{self as hir, lower::LowerModule};
use lume_ast::{self as ast};

impl<'a> LowerModule<'a> {
    pub(super) fn literal(&mut self, expr: ast::Literal) -> Result<hir::Literal> {
        match expr {
            ast::Literal::Int(t) => self.lit_int(*t),
            ast::Literal::Float(t) => self.lit_float(*t),
            ast::Literal::String(t) => self.lit_string(*t),
            ast::Literal::Boolean(t) => self.lit_boolean(*t),
        }
    }

    fn lit_int(&mut self, expr: ast::IntLiteral) -> Result<hir::Literal> {
        let id = self.next_expr_id();
        let value = expr.value;
        let kind = expr.kind.into();
        let location = self.location(expr.location);

        Ok(hir::Literal {
            id,
            location,
            kind: hir::LiteralKind::Int(Box::new(hir::IntLiteral { id, value, kind })),
        })
    }

    fn lit_float(&mut self, expr: ast::FloatLiteral) -> Result<hir::Literal> {
        let id = self.next_expr_id();
        let value = expr.value;
        let kind = expr.kind.into();
        let location = self.location(expr.location);

        Ok(hir::Literal {
            id,
            location,
            kind: hir::LiteralKind::Float(Box::new(hir::FloatLiteral { id, value, kind })),
        })
    }

    fn lit_string(&mut self, expr: ast::StringLiteral) -> Result<hir::Literal> {
        let id = self.next_expr_id();
        let value = expr.value.clone();
        let location = self.location(expr.location);

        Ok(hir::Literal {
            id,
            location,
            kind: hir::LiteralKind::String(Box::new(hir::StringLiteral { id, value })),
        })
    }

    fn lit_boolean(&mut self, expr: ast::BooleanLiteral) -> Result<hir::Literal> {
        let id = self.next_expr_id();
        let value = expr.value;
        let location = self.location(expr.location);

        Ok(hir::Literal {
            id,
            location,
            kind: hir::LiteralKind::Boolean(Box::new(hir::BooleanLiteral { id, value })),
        })
    }
}
