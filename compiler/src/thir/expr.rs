use diag::Result;

use crate::{hir, thir::ir};

use super::ThirBuildCtx;

impl ThirBuildCtx {
    pub(super) fn expr(&self, expr: &hir::Expression) -> Result<ir::Expr> {
        let kind = match &expr.kind {
            hir::ExpressionKind::Assignment(a) => self.expr_assignment(&a)?,
            hir::ExpressionKind::New(a) => self.expr_new(&a)?,
            hir::ExpressionKind::FunctionCall(a) => self.expr_function_call(&a)?,
            hir::ExpressionKind::MethodCall(a) => self.expr_method_call(&a)?,
            hir::ExpressionKind::Literal(a) => self.expr_literal(&a)?,
            hir::ExpressionKind::Member(a) => self.expr_member(&a)?,
            hir::ExpressionKind::Range(a) => self.expr_range(&a)?,
            hir::ExpressionKind::Variable(a) => self.expr_variable(&a)?,
        };

        Ok(ir::Expr {
            kind,
            ty: self.type_of(expr.id)?,
        })
    }

    fn expr_assignment<'a>(&self, expr: &hir::Assignment) -> Result<ir::ExprKind> {
        Ok(ir::ExprKind::Assignment {
            target: expr.target.id,
            value: expr.value.id,
        })
    }

    fn expr_new<'a>(&self, _expr: &hir::New) -> Result<ir::ExprKind> {
        todo!()
    }

    fn expr_function_call<'a>(&self, expr: &hir::FunctionCall) -> Result<ir::ExprKind> {
        let fun = expr.reference;
        let args = expr
            .arguments
            .iter()
            .map(|arg| arg.id)
            .collect::<Vec<_>>()
            .into_boxed_slice();

        Ok(ir::ExprKind::Call { fun, args })
    }

    fn expr_method_call<'a>(&self, _expr: &hir::MethodCall) -> Result<ir::ExprKind> {
        todo!()
    }

    fn expr_literal<'a>(&self, expr: &hir::Literal) -> Result<ir::ExprKind> {
        let kind = match &expr.kind {
            hir::LiteralKind::Int(i) => self.lit_int(&*i)?,
            hir::LiteralKind::Boolean(i) => self.lit_bool(&*i)?,
            hir::LiteralKind::Float(i) => self.lit_float(&*i)?,
            hir::LiteralKind::String(i) => self.lit_string(&*i)?,
        };

        Ok(ir::ExprKind::Literal { kind })
    }

    fn expr_member<'a>(&self, _expr: &hir::Member) -> Result<ir::ExprKind> {
        todo!()
    }

    fn expr_variable<'a>(&self, expr: &hir::Variable) -> Result<ir::ExprKind> {
        Ok(ir::ExprKind::Var { id: expr.id })
    }

    fn expr_range<'a>(&self, _expr: &hir::Range) -> Result<ir::ExprKind> {
        todo!()
    }

    fn lit_int<'a>(&self, expr: &hir::IntLiteral) -> Result<ir::LiteralKind> {
        Ok(ir::LiteralKind::Int(expr.value as i64, ir::IntKind::I32))
    }

    fn lit_bool<'a>(&self, expr: &hir::BooleanLiteral) -> Result<ir::LiteralKind> {
        Ok(ir::LiteralKind::Bool(expr.value))
    }

    fn lit_float<'a>(&self, expr: &hir::FloatLiteral) -> Result<ir::LiteralKind> {
        Ok(ir::LiteralKind::Float(expr.value as f64, ir::FloatKind::F64))
    }

    fn lit_string<'a>(&self, expr: &hir::StringLiteral) -> Result<ir::LiteralKind> {
        Ok(ir::LiteralKind::Str(expr.value.clone()))
    }
}
