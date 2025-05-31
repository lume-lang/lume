use crate::query::CallReference;
use indexmap::IndexMap;
use lume_errors::DiagCtxHandle;
use lume_hir::{SymbolName, TypeParameter};
use lume_span::{ExpressionId, StatementId};
use lume_types::{TypeDatabaseContext, TypeRef};

mod check;
mod errors;
mod infer;
pub(crate) mod query;
#[cfg(test)]
mod tests;

pub struct ThirBuildCtx {
    /// Defines the type context from the build context.
    tcx: TypeDatabaseContext,

    /// Defines the HIR map which contains the input expressions.
    hir: lume_hir::map::Map,

    /// Defines the diagnostics handler.
    dcx: DiagCtxHandle,

    /// Defines a mapping between statements and their resolved types.
    pub resolved_stmts: IndexMap<StatementId, TypeRef>,

    /// Defines a mapping between calls and the corresponding symbol being called.
    pub resolved_calls: IndexMap<ExpressionId, CallReference>,
}

#[allow(dead_code)]
impl ThirBuildCtx {
    /// Creates a new empty THIR build context.
    pub fn new(hir: lume_hir::map::Map, dcx: DiagCtxHandle) -> ThirBuildCtx {
        ThirBuildCtx {
            tcx: TypeDatabaseContext::default(),
            hir,
            dcx,
            resolved_stmts: IndexMap::new(),
            resolved_calls: IndexMap::new(),
        }
    }

    /// Retrieves the type context from the build context.
    pub fn tcx(&self) -> &TypeDatabaseContext {
        &self.tcx
    }

    /// Retrieves the type context from the build context.
    pub fn tcx_mut(&mut self) -> &mut TypeDatabaseContext {
        &mut self.tcx
    }

    /// Retrieves the diagnostics handler from the build context.
    pub fn dcx(&mut self) -> &mut DiagCtxHandle {
        &mut self.dcx
    }

    /// Gets the HIR expression with the given ID within the source file.
    #[allow(clippy::unused_self)]
    pub(crate) fn hir_stmt(&self, id: StatementId) -> &lume_hir::Statement {
        match self.hir.statements().get(&id) {
            Some(expr) => expr,
            None => panic!("no statement with given ID found: {id:?}"),
        }
    }

    /// Gets the HIR expression with the given ID within the source file.
    #[allow(clippy::unused_self)]
    pub(crate) fn hir_expr(&self, id: ExpressionId) -> &lume_hir::Expression {
        match self.hir.expressions().get(&id) {
            Some(expr) => expr,
            None => panic!("no expression with given ID found: {id:?}"),
        }
    }
}
