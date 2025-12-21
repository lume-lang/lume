use crate::LowerModule;

impl LowerModule {
    #[libftrace::traced(level = Debug)]
    pub(super) fn literal(&mut self, expr: lume_ast::Literal) -> lume_hir::Literal {
        match expr {
            lume_ast::Literal::Int(t) => self.lit_int(*t),
            lume_ast::Literal::Float(t) => self.lit_float(*t),
            lume_ast::Literal::String(t) => self.lit_string(*t),
            lume_ast::Literal::Boolean(t) => self.lit_boolean(*t),
        }
    }

    #[libftrace::traced(level = Debug)]
    fn lit_int(&mut self, expr: lume_ast::IntLiteral) -> lume_hir::Literal {
        let id = self.next_node_id();
        let value = expr.value;
        let kind = expr.kind.map(std::convert::Into::into);
        let location = self.location(expr.location);

        lume_hir::Literal {
            id,
            location,
            kind: lume_hir::LiteralKind::Int(Box::new(lume_hir::IntLiteral { id, value, kind })),
        }
    }

    #[libftrace::traced(level = Debug)]
    fn lit_float(&mut self, expr: lume_ast::FloatLiteral) -> lume_hir::Literal {
        let id = self.next_node_id();
        let value = expr.value;
        let kind = expr.kind.map(std::convert::Into::into);
        let location = self.location(expr.location);

        lume_hir::Literal {
            id,
            location,
            kind: lume_hir::LiteralKind::Float(Box::new(lume_hir::FloatLiteral { id, value, kind })),
        }
    }

    #[libftrace::traced(level = Debug)]
    fn lit_string(&mut self, expr: lume_ast::StringLiteral) -> lume_hir::Literal {
        let id = self.next_node_id();
        let value = expr.value.clone();
        let location = self.location(expr.location);

        lume_hir::Literal {
            id,
            location,
            kind: lume_hir::LiteralKind::String(Box::new(lume_hir::StringLiteral { id, value })),
        }
    }

    #[libftrace::traced(level = Debug)]
    fn lit_boolean(&mut self, expr: lume_ast::BooleanLiteral) -> lume_hir::Literal {
        let id = self.next_node_id();
        let value = expr.value;
        let location = self.location(expr.location);

        lume_hir::Literal {
            id,
            location,
            kind: lume_hir::LiteralKind::Boolean(Box::new(lume_hir::BooleanLiteral { id, value })),
        }
    }
}
