use crate::LowerModule;

use lume_ast::{self as ast};
use lume_hir::{self as hir};

impl LowerModule<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn literal(&mut self, expr: ast::Literal) -> hir::Literal {
        match expr {
            ast::Literal::Int(t) => self.lit_int(*t),
            ast::Literal::Float(t) => self.lit_float(*t),
            ast::Literal::String(t) => self.lit_string(*t),
            ast::Literal::Boolean(t) => self.lit_boolean(*t),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn lit_int(&mut self, expr: ast::IntLiteral) -> hir::Literal {
        let id = self.next_expr_id();
        let value = expr.value;
        let kind = expr.kind.into();
        let location = self.location(expr.location);

        hir::Literal {
            id,
            location,
            kind: hir::LiteralKind::Int(Box::new(hir::IntLiteral { id, value, kind })),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn lit_float(&mut self, expr: ast::FloatLiteral) -> hir::Literal {
        let id = self.next_expr_id();
        let value = expr.value;
        let kind = expr.kind.into();
        let location = self.location(expr.location);

        hir::Literal {
            id,
            location,
            kind: hir::LiteralKind::Float(Box::new(hir::FloatLiteral { id, value, kind })),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn lit_string(&mut self, expr: ast::StringLiteral) -> hir::Literal {
        let id = self.next_expr_id();
        let value = expr.value.clone();
        let location = self.location(expr.location);

        hir::Literal {
            id,
            location,
            kind: hir::LiteralKind::String(Box::new(hir::StringLiteral { id, value })),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn lit_boolean(&mut self, expr: ast::BooleanLiteral) -> hir::Literal {
        let id = self.next_expr_id();
        let value = expr.value;
        let location = self.location(expr.location);

        hir::Literal {
            id,
            location,
            kind: hir::LiteralKind::Boolean(Box::new(hir::BooleanLiteral { id, value })),
        }
    }
}
