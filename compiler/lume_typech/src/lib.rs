use error_snippet::{GraphicalRenderer, handler::DiagnosticHandler};
use indexmap::IndexMap;
use lume_hir::{ExpressionId, StatementId, TypeParameter};
use lume_types::{SymbolName, TypeDatabaseContext, TypeId, TypeRef};
use symbol::CallReference;

mod check;
mod errors;
mod infer;
mod method;
mod symbol;

pub struct ThirBuildCtx<'a> {
    /// Defines the parent state.
    state: &'a mut lume_state::State,

    /// Defines the diagnostics handler.
    dcx: DiagnosticHandler,

    /// Defines a mapping between expressions and their resolved types.
    pub resolved_exprs: IndexMap<ExpressionId, TypeRef>,

    /// Defines a mapping between statements and their resolved types.
    pub resolved_stmts: IndexMap<StatementId, TypeRef>,

    /// Defines a mapping between calls and the corresponding symbol being called.
    pub resolved_calls: IndexMap<ExpressionId, CallReference>,
}

#[allow(dead_code)]
impl<'a> ThirBuildCtx<'a> {
    /// Creates a new empty THIR build context.
    pub fn new<'tcx>(state: &'tcx mut lume_state::State) -> ThirBuildCtx<'tcx> {
        ThirBuildCtx {
            state,
            dcx: DiagnosticHandler::with_renderer(Box::new(GraphicalRenderer::new())),
            resolved_exprs: IndexMap::new(),
            resolved_stmts: IndexMap::new(),
            resolved_calls: IndexMap::new(),
        }
    }

    /// Retrieves the type context from the build context.
    pub fn tcx(&self) -> &TypeDatabaseContext {
        self.state.tcx()
    }

    /// Retrieves the type context from the build context.
    pub fn tcx_mut(&mut self) -> &mut TypeDatabaseContext {
        self.state.tcx_mut()
    }

    /// Retrieves the diagnostics handler from the build context.
    pub fn dcx(&'a mut self) -> &'a mut DiagnosticHandler {
        &mut self.dcx
    }

    /// Gets the HIR expression with the given ID within the source file.
    pub(crate) fn hir_stmt(
        &'a self,
        hir: &'a lume_hir::map::Map,
        id: lume_hir::StatementId,
    ) -> &'a lume_hir::Statement {
        match hir.statements().get(&id) {
            Some(expr) => expr,
            None => panic!("no statement with given ID found: {:?}", id),
        }
    }

    /// Gets the HIR expression with the given ID within the source file.
    pub(crate) fn hir_expr(&self, hir: &'a lume_hir::map::Map, id: ExpressionId) -> &'a lume_hir::Expression {
        match hir.expressions().get(&id) {
            Some(expr) => expr,
            None => panic!("no expression with given ID found: {:?}", id),
        }
    }
}
