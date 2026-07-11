use std::ops::ControlFlow;

use lume_architect::cached_query;
use lume_errors::{Error, SimpleDiagnostic};
use lume_hir::Visitor;
use lume_span::{Location, NodeId};

use crate::TyInferCtx;
use crate::query::Callable;

impl TyInferCtx {
    /// Gets the constness of the given HIR node.
    #[cached_query]
    #[tracing::instrument(level = "TRACE", skip_all)]
    pub fn hir_constness_of(&self, id: NodeId) -> Option<lume_hir::Constness> {
        if let lume_hir::Node::Function(expr) = self.hir_node(id)? {
            Some(expr.constness)
        } else {
            None
        }
    }

    /// Determines whether the given node is a child of a `const` block, either
    /// as part of a const function or otherwise.
    #[cached_query]
    #[tracing::instrument(level = "TRACE", skip_all)]
    pub fn hir_in_const_block(&self, id: NodeId) -> bool {
        self.hir_parent_id_iter(id)
            .any(|id| self.hir_constness_of(id) == Some(lume_hir::Constness::Const))
    }
}

struct ConstNodeVisitor<'tcx> {
    tcx: &'tcx TyInferCtx,
    errors: Vec<Error>,
}

impl ConstNodeVisitor<'_> {
    fn raise_incompatible_call(&mut self, expr_span: Location, callable: Callable<'_>) {
        let callable_span = callable.name().location();

        self.errors.push(
            SimpleDiagnostic::new("cannot call non-const callable in const context")
                .with_label(lume_errors::Label::error(
                    Some(expr_span.file.clone()),
                    expr_span.index.clone(),
                    format!(
                        "cannot call non-const callable {:+} in a const context",
                        callable.name()
                    ),
                ))
                .with_label(lume_errors::Label::note(
                    Some(callable_span.file.clone()),
                    callable_span.index.clone(),
                    "callable declared here",
                ))
                .into(),
        );
    }
}

impl Visitor for ConstNodeVisitor<'_> {
    type Break = bool;

    fn visit_expr(&mut self, expr: &lume_hir::Expression) -> ControlFlow<Self::Break> {
        match &expr.kind {
            lume_hir::ExpressionKind::Assignment(_)
            | lume_hir::ExpressionKind::Cast(_)
            | lume_hir::ExpressionKind::Construct(_)
            | lume_hir::ExpressionKind::If(_)
            | lume_hir::ExpressionKind::Is(_)
            | lume_hir::ExpressionKind::Literal(_)
            | lume_hir::ExpressionKind::Member(_)
            | lume_hir::ExpressionKind::Scope(_)
            | lume_hir::ExpressionKind::Switch(_)
            | lume_hir::ExpressionKind::Variable(_)
            | lume_hir::ExpressionKind::Variant(_) => ControlFlow::Continue(()),

            lume_hir::ExpressionKind::Ref(_)
            | lume_hir::ExpressionKind::Deref(_)
            | lume_hir::ExpressionKind::Missing => ControlFlow::Break(false),

            lume_hir::ExpressionKind::InstanceCall(call) => {
                let Ok(callable) = self.tcx.probe_callable_instance(call) else {
                    return ControlFlow::Break(false);
                };

                if self.tcx.hir_constness_of(callable.id()) == Some(lume_hir::Constness::Const) {
                    ControlFlow::Continue(())
                } else {
                    self.raise_incompatible_call(call.location, callable);

                    ControlFlow::Break(false)
                }
            }
            lume_hir::ExpressionKind::IntrinsicCall(call) => {
                let Ok(callable) = self.tcx.probe_callable_intrinsic(call) else {
                    return ControlFlow::Break(false);
                };

                // Compiler-inserted intrinsics are allowed, since they mostly evaluate to
                // single, idempotent instructions.
                if let Ok(true) = self.tcx.is_compiler_intrinsic(call) {
                    return ControlFlow::Continue(());
                }

                if self.tcx.hir_constness_of(callable.id()) == Some(lume_hir::Constness::Const) {
                    ControlFlow::Continue(())
                } else {
                    self.raise_incompatible_call(call.location, callable);

                    ControlFlow::Break(false)
                }
            }
            lume_hir::ExpressionKind::StaticCall(call) => {
                let Ok(callable) = self.tcx.probe_callable_static(call) else {
                    return ControlFlow::Break(false);
                };

                if self.tcx.hir_constness_of(callable.id()) == Some(lume_hir::Constness::Const) {
                    ControlFlow::Continue(())
                } else {
                    self.raise_incompatible_call(call.location, callable);

                    ControlFlow::Break(false)
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CompatibleResult {
    Compatible,
    Incompatible(Vec<Error>),
}

impl TyInferCtx {
    /// Determines whether the given HIR node is const compatible, ie. if it can
    /// exist within a const context.
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn is_const_compatible(&self, id: NodeId) -> CompatibleResult {
        let node = self.hir_expect_node(id);
        let mut visitor = ConstNodeVisitor {
            tcx: self,
            errors: Vec::new(),
        };

        if lume_hir::visitor::traverse_node(self.hir(), &mut visitor, node) == ControlFlow::Break(false) {
            CompatibleResult::Incompatible(visitor.errors)
        } else {
            CompatibleResult::Compatible
        }
    }

    /// Ensures that the given node is const compatible, ie. if it can
    /// exist within a const context.
    ///
    /// If not compatible, raises an error directly into the diagnostics
    /// context.
    #[tracing::instrument(level = "Trace", skip_all)]
    pub fn ensure_const_compatible(&self, id: NodeId) {
        if let CompatibleResult::Incompatible(errors) = self.is_const_compatible(id) {
            for err in errors {
                self.dcx().emit(err);
            }
        }
    }
}
