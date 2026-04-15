use crate::*;

impl LoweringContext<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn literal(&mut self, expr: lume_ast::Literal) -> lume_hir::Literal {
        match expr {
            lume_ast::Literal::IntegerLit(t) => self.lit_int(t),
            lume_ast::Literal::FloatLit(t) => self.lit_float(t),
            lume_ast::Literal::StringLit(t) => self.lit_string(t),
            lume_ast::Literal::BooleanLit(t) => self.lit_boolean(t),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn literal_opt(&mut self, expr: Option<lume_ast::Literal>) -> lume_hir::Literal {
        match expr {
            Some(lit) => self.literal(lit),
            None => lume_hir::Literal::missing(),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn lit_int(&mut self, expr: lume_ast::IntegerLit) -> lume_hir::Literal {
        let (radix, mut value_str, kind) = expr.as_parts();
        value_str = value_str.replace('_', "");

        let Ok(value) = i128::from_str_radix(&value_str, radix as u32) else {
            self.dcx.emit_and_push(
                crate::errors::InvalidLiteral {
                    source: self.current_file().clone(),
                    range: expr.range(),
                    value: value_str,
                }
                .into(),
            );

            return lume_hir::Literal::missing();
        };

        let id = self.next_node_id();
        let kind = kind.map(std::convert::Into::into);
        let location = self.location(expr.location());

        lume_hir::Literal {
            id,
            location,
            kind: lume_hir::LiteralKind::Int(lume_hir::IntLiteral { id, value, kind }),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn lit_float(&mut self, expr: lume_ast::FloatLit) -> lume_hir::Literal {
        let (mut value_str, kind) = expr.as_parts();
        value_str = value_str.replace('_', "");

        let Ok(value) = value_str.parse::<f64>() else {
            self.dcx.emit_and_push(
                crate::errors::InvalidLiteral {
                    source: self.current_file().clone(),
                    range: expr.range(),
                    value: value_str,
                }
                .into(),
            );

            return lume_hir::Literal::missing();
        };

        let id = self.next_node_id();
        let kind = kind.map(std::convert::Into::into);
        let location = self.location(expr.location());

        lume_hir::Literal {
            id,
            location,
            kind: lume_hir::LiteralKind::Float(lume_hir::FloatLiteral { id, value, kind }),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn lit_string(&mut self, expr: lume_ast::StringLit) -> lume_hir::Literal {
        let id = self.next_node_id();
        let value = expr.syntax().text().to_string().trim_matches('"').to_string();
        let location = self.location(expr.location());

        lume_hir::Literal {
            id,
            location,
            kind: lume_hir::LiteralKind::String(lume_hir::StringLiteral { id, value }),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn lit_boolean(&mut self, expr: lume_ast::BooleanLit) -> lume_hir::Literal {
        let id = self.next_node_id();
        let value = expr.syntax().text() == "true";
        let location = self.location(expr.location());

        lume_hir::Literal {
            id,
            location,
            kind: lume_hir::LiteralKind::Boolean(lume_hir::BooleanLiteral { id, value }),
        }
    }
}
