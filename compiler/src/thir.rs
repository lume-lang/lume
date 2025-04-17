use std::collections::HashMap;

use diag::Result;

use crate::{
    driver::{ModuleFileId, Sources},
    hir::{self, IntoOwner, ItemId, NodeId},
};

mod errors;
pub mod expr;
pub mod ir;
pub mod ty;

pub(crate) type Map = ir::Map;

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub(crate) struct ThirBuildCtx {
    /// The Typed High-level Intermediate Representation (THIR) map.
    thir: Map,

    /// The HIR maps to typecheck over.
    pub(super) hir: HashMap<ModuleFileId, hir::Map>,
}

impl ThirBuildCtx {
    /// Creates an empty THIR build context.
    pub(crate) fn empty() -> Self {
        ThirBuildCtx {
            thir: Map::new(),
            hir: HashMap::new(),
        }
    }

    /// Creates a new THIR build context from the given sources.
    pub(crate) fn from_sources(sources: Sources) -> Self {
        let mut thir_ctx = Self::empty();

        for (_, source) in sources.files {
            thir_ctx.push_hir(source.hir);
        }

        thir_ctx
    }

    /// Pushes a new parsed HIR map onto the THIR build context.
    pub(crate) fn push_hir(&mut self, hir: hir::Map) {
        self.hir.insert(hir.file, hir);
    }

    /// Attempts to evaluate the types of all expressions within the HIR maps.
    pub(crate) fn evaluate(&mut self) -> Result<()> {
        for (_, hir) in &self.hir {
            for (id, expr) in &hir.expressions {
                let ir_expr = self.expr(expr)?;

                self.thir.exprs.insert(*id, ir_expr);
            }
        }

        Ok(())
    }

    /// Gets the HIR map for the given source file ID.
    pub(crate) fn hir_find(&self, id: impl IntoOwner) -> &hir::Map {
        let file_id = id.owner();

        match self.hir.get(&file_id) {
            Some(hir) => hir,
            None => bug!("no HIR source with given ID found: {:?}", file_id),
        }
    }

    /// Gets the HIR item with the given ID within the source file.
    pub(crate) fn hir_item(&self, id: ItemId) -> &hir::Symbol {
        let hir = self.hir_find(id);

        match hir.items.get(&id) {
            Some(item) => item,
            None => bug!("no item with given ID found: {:?}", id),
        }
    }

    /// Gets the HIR expression with the given ID within the source file.
    pub(crate) fn hir_stmt(&self, id: NodeId) -> &hir::Statement {
        let hir = self.hir_find(id);

        match hir.statements.get(&id) {
            Some(expr) => expr,
            None => bug!("no statement with given ID found: {:?}", id),
        }
    }

    /// Gets the HIR expression with the given ID within the source file.
    pub(crate) fn hir_expr(&self, id: NodeId) -> &hir::Expression {
        let hir = self.hir_find(id);

        match hir.expressions.get(&id) {
            Some(expr) => expr,
            None => bug!("no expression with given ID found: {:?}", id),
        }
    }

    /// Gets the HIR statement with the given ID and assert that it's a variable declaration statement.
    pub(crate) fn hir_expect_var_stmt(&self, id: NodeId) -> &hir::VariableDeclaration {
        let stmt = self.hir_stmt(id);

        match &stmt.kind {
            hir::StatementKind::Variable(decl) => decl,
            t => bug!("invalid variable reference type: {:?}", t),
        }
    }
}
