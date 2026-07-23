use indexmap::IndexSet;
use lume_errors::Result;
use lume_hir::CallExpression;
use lume_mir_queries::MirQueryCtx;
use lume_span::NodeId;
use lume_typech::TyCheckCtx;

use crate::{Instance, MonoItems};

pub fn collect(mcx: &MirQueryCtx<'_>) -> Result<MonoItems> {
    let mut items = MonoItems::default();
    items.extend(collect_roots(mcx)?);

    Ok(items)
}

#[tracing::instrument(level = "INFO", skip_all, fields(package = mcx.tcx().current_package().name), err)]
pub(crate) fn collect_roots(mcx: &MirQueryCtx<'_>) -> Result<IndexSet<Instance>> {
    let tcx = mcx.tcx();
    let mir = mcx.mir();

    let mut instances = IndexSet::new();
    let mut worklist = IndexSet::new();

    // Ensure the main entrypoint is inserted as a root, if one is available, since
    // it cannot be generic.
    //
    // TODO: ensure the signature of the entrypoint is as expected.
    if let Some(main_fn) = tcx.entrypoint() {
        worklist.insert(Instance {
            id: main_fn.id,
            generics: Vec::new(),
        });
    }

    // Add all public, concrete (non-generic) functions and methods into the root
    // set, since we can be sure they are already monomorphic.
    for mir_func in mir
        .functions
        .values()
        .filter(|func| !func.signature.internal && !func.signature.external && !tcx.is_node_generic(func.id))
    {
        worklist.insert(Instance {
            id: mir_func.id,
            generics: Vec::new(),
        });
    }

    let mut visitor = CallVisitor::new(tcx);

    while let Some(workitem) = worklist.pop() {
        if instances.contains(&workitem) {
            continue;
        }

        tracing::debug!(item = workitem.display(tcx).to_string(), "collected");

        let type_parameters = tcx.type_params_of(workitem.id)?;
        debug_assert_eq!(type_parameters.len(), workitem.generics.len());

        for CallLocation { call_expr, callable_id } in visitor.calls_in(tcx.hir(), workitem.id)? {
            let generic_args = tcx
                .mk_type_refs_from(call_expr.type_arguments(), call_expr.id())?
                .into_iter()
                .map(|type_arg| {
                    tcx.instantiate_flat_type_from(&type_arg, type_parameters, &workitem.generics)
                        .to_owned()
                })
                .collect::<Vec<_>>();

            let call_instance = Instance {
                id: callable_id,
                generics: generic_args,
            };

            tracing::trace!(
                from = tcx.hir_path_of_node(workitem.id).to_wide_string(),
                to = call_instance.display(tcx).to_string(),
                "call_edge",
            );

            worklist.insert(call_instance);
        }

        instances.insert(workitem);
    }

    Ok(instances)
}

struct CallVisitor<'tcx, 'hir> {
    tcx: &'tcx TyCheckCtx,
    call_graph: IndexSet<CallLocation<'hir>>,
}

impl<'tcx, 'hir> CallVisitor<'tcx, 'hir> {
    pub fn new(tcx: &'tcx TyCheckCtx) -> Self {
        Self {
            tcx,
            call_graph: IndexSet::new(),
        }
    }

    pub fn calls_in(&mut self, hir: &'hir lume_hir::Map, id: NodeId) -> Result<IndexSet<CallLocation<'hir>>> {
        lume_hir::traverse_node(hir, self, hir.expect_node(id).unwrap())?;

        Ok(std::mem::take(&mut self.call_graph))
    }
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
struct CallLocation<'tcx> {
    pub call_expr: CallExpression<'tcx>,
    pub callable_id: NodeId,
}

impl<'hir> lume_hir::Visitor<'hir> for CallVisitor<'_, 'hir> {
    fn visit_expr(&mut self, expr: &'hir lume_hir::Expression) -> Result<()> {
        let call_expr = match &expr.kind {
            lume_hir::ExpressionKind::InstanceCall(call) => CallExpression::Instanced(call),
            lume_hir::ExpressionKind::StaticCall(call) => CallExpression::Static(call),
            lume_hir::ExpressionKind::IntrinsicCall(call) => CallExpression::Intrinsic(call),
            _ => return Ok(()),
        };

        self.call_graph.insert(CallLocation {
            call_expr,
            callable_id: self.tcx.probe_callable(call_expr)?.id(),
        });

        Ok(())
    }
}
