use diag::Result;

use crate::{
    hir::{self, ItemId, NodeId},
    thir::ir,
};

use super::ThirBuildCtx;

impl ThirBuildCtx {
    /// Returns the *type* of the definition with the given `NodeId`.
    ///
    /// ### Panics
    ///
    /// This method will panic if no definition with the given ID exists
    /// within it's declared module. This also applies to any recursive calls this
    /// method makes, in the case of some expressions, such as assignments.
    pub(crate) fn type_of(&self, def: NodeId) -> Result<ir::Ty> {
        let expr = self.hir_expr(def);

        let kind = match &expr.kind {
            hir::ExpressionKind::Assignment(e) => self.type_of_id(e.value.id)?,
            hir::ExpressionKind::FunctionCall(e) => self.type_of_item(e.reference)?,
            hir::ExpressionKind::Literal(e) => match &e.kind {
                hir::LiteralKind::Int(k) => ir::TyKind::Int(k.kind.into()),
                hir::LiteralKind::Float(k) => ir::TyKind::Float(k.kind.into()),
                hir::LiteralKind::String(_) => ir::TyKind::Str,
                hir::LiteralKind::Boolean(_) => ir::TyKind::Bool,
            },
            hir::ExpressionKind::Variable(var) => {
                let decl = self.hir_expect_var_stmt(var.reference);

                self.type_of_id(decl.value.id)?
            }
            k => todo!("unhandled node type in type_of(): {:?}", k),
        };

        Ok(ir::Ty(Box::new(kind)))
    }

    fn type_of_id(&self, id: NodeId) -> Result<ir::TyKind> {
        let ir::Ty(kind, ..) = self.type_of(id)?;

        Ok(*kind)
    }

    fn type_of_item(&self, id: ItemId) -> Result<ir::TyKind> {
        match self.hir_item(id) {
            hir::Symbol::Function(e) => self.lower_type_ref(&*e.return_type),
            _ => todo!("item kind: {:?}", id),
        }
    }

    fn lower_type_ref(&self, ty: &hir::Type) -> Result<ir::TyKind> {
        match &ty {
            hir::Type::Scalar(t) => Ok(ir::TyKind::Class(t.reference)),
            _ => todo!(),
        }
    }
}
