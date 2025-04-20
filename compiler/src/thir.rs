use diag::Result;

use crate::hir::{self, ExpressionId, ItemId};

mod define;
// mod errors;
// pub mod expr;
// pub mod ir;
pub mod ty;

// pub(crate) type Map = ir::Map;

#[derive(serde::Serialize, Debug)]
pub(crate) struct ThirBuildCtx {
    // The Typed High-level Intermediate Representation (THIR) map.
    // thir: Map,
    /// The HIR maps to typecheck over.
    hir: hir::map::Map,

    /// Defines the type database context.
    tcx: ty::TypeDatabaseContext,
}

#[allow(dead_code)]
impl ThirBuildCtx {
    /// Creates a new THIR build context from the given HIR map.
    pub(crate) fn new(hir: hir::map::Map) -> Self {
        ThirBuildCtx {
            // thir: Map::new(),
            hir,
            tcx: ty::TypeDatabaseContext::new(),
        }
    }

    /// Retrieves the HIR map from the build context.
    pub(super) fn hir(&self) -> &hir::map::Map {
        &self.hir
    }

    /// Retrieves the type context from the build context.
    pub(super) fn tcx(&self) -> &ty::TypeDatabaseContext {
        &self.tcx
    }

    /// Attempts to infer the types of all expressions within the HIR maps.
    pub(crate) fn infer(&mut self) -> Result<()> {
        self.define_types()?;

        self.infer_exprs()?;

        println!("{:#?}", self.tcx());

        Ok(())
    }

    /// Gets the HIR item with the given ID within the source file.
    pub(crate) fn hir_item(&self, id: ItemId) -> &hir::Symbol {
        match self.hir().items.get(&id) {
            Some(item) => item,
            None => bug!("no item with given ID found: {:?}", id),
        }
    }

    /// Gets the HIR expression with the given ID within the source file.
    pub(crate) fn hir_stmt(&self, id: hir::StatementId) -> &hir::Statement {
        match self.hir().statements.get(&id) {
            Some(expr) => expr,
            None => bug!("no statement with given ID found: {:?}", id),
        }
    }

    /// Gets the HIR expression with the given ID within the source file.
    pub(crate) fn hir_expr(&self, id: ExpressionId) -> &hir::Expression {
        match self.hir().expressions.get(&id) {
            Some(expr) => expr,
            None => bug!("no expression with given ID found: {:?}", id),
        }
    }

    /// Gets the HIR statement with the given ID and assert that it's a variable declaration statement.
    pub(crate) fn hir_expect_var_stmt(&self, id: hir::StatementId) -> &hir::VariableDeclaration {
        let stmt = self.hir_stmt(id);

        match &stmt.kind {
            hir::StatementKind::Variable(decl) => decl,
            t => bug!("invalid variable reference type: {:?}", t),
        }
    }
}
