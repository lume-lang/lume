use indexmap::IndexMap;
use lume_hir::{ExpressionId, StatementId, TypeParameter, map::SourceMap};
use lume_types::{SymbolName, TypeDatabaseContext, TypeId, TypeRef};

mod check;
mod errors;
mod infer;

#[derive(serde::Serialize, Debug)]
pub struct ThirBuildCtx {
    /// Defines the type database context.
    tcx: TypeDatabaseContext,

    /// Defines the sources currently being processed.
    sources: SourceMap,

    /// Defines a mapping between expressions and their resolved types.
    pub resolved_exprs: IndexMap<ExpressionId, TypeRef>,

    /// Defines a mapping between statements and their resolved types.
    pub resolved_stmts: IndexMap<StatementId, TypeRef>,
}

#[allow(dead_code)]
impl ThirBuildCtx {
    /// Creates a new empty THIR build context.
    pub fn new(sources: SourceMap) -> Self {
        ThirBuildCtx {
            tcx: TypeDatabaseContext::new(),
            sources,
            resolved_exprs: IndexMap::new(),
            resolved_stmts: IndexMap::new(),
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

    /// Gets the type of the expression with the given ID within the source file.
    ///
    /// ## Panics
    ///
    /// If the given ID does not exist in the resolved expressions map, this
    /// method will panic. This also applies if the given ID (somehow) refers to
    /// a non-expression node, such as a statement.
    pub(crate) fn type_of_expr(&self, id: ExpressionId) -> &TypeRef {
        self.resolved_exprs.get(&id).unwrap()
    }

    /// Gets the type of the statement with the given ID within the source file.
    ///
    /// ## Panics
    ///
    /// If the given ID does not exist in the resolved statements map, this
    /// method will panic. This also applies if the given ID (somehow) refers to
    /// a non-statement node, such as an expression.
    pub(crate) fn type_of_stmt(&self, id: StatementId) -> &TypeRef {
        self.resolved_stmts.get(&id).unwrap()
    }

    /// Gets the HIR expression with the given ID within the source file.
    pub(crate) fn hir_stmt<'a>(
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
    pub(crate) fn hir_expr<'a>(&self, hir: &'a lume_hir::map::Map, id: ExpressionId) -> &'a lume_hir::Expression {
        match hir.expressions().get(&id) {
            Some(expr) => expr,
            None => panic!("no expression with given ID found: {:?}", id),
        }
    }
}
