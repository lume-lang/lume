use indexmap::IndexSet;
use lume_errors::Result;
use lume_hir::CallExpression;
use lume_mir_queries::MirQueryCtx;
use lume_span::NodeId;
use lume_typech::TyCheckCtx;
use lume_types::TypeRef;

use crate::{Generics, Instance, MonoItems};

#[tracing::instrument(level = "INFO", skip_all, fields(package = mcx.tcx().current_package().name), err)]
pub fn collect(mcx: &MirQueryCtx<'_>) -> Result<MonoItems> {
    let mut items = MonoItems::default();
    items.extend(collect_roots(mcx)?);

    collect_monotypes(mcx, &mut items)?;

    Ok(items)
}

#[tracing::instrument(level = "DEBUG", skip_all, fields(package = mcx.tcx().current_package().name), err)]
fn collect_roots(mcx: &MirQueryCtx<'_>) -> Result<IndexSet<Instance>> {
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
            generics: Some(Generics::default()),
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
            generics: Some(Generics::default()),
        });
    }

    let mut visitor = CallVisitor::new(tcx);

    while let Some(workitem) = worklist.pop() {
        if instances.contains(&workitem) {
            continue;
        }

        tracing::debug!(item = workitem.display(tcx).to_string(), "collected");

        let generics = workitem.generics.clone().unwrap_or_default();
        let type_parameters = tcx.canonical_type_parameters_of(workitem.id)?;
        debug_assert_eq!(type_parameters.len(), generics.len());

        for CallLocation { call_expr, callable_id } in visitor.calls_in(tcx.hir(), workitem.id)? {
            let generic_params = tcx.canonical_type_parameters_of(callable_id)?;

            let generic_args = tcx
                .mk_type_refs_from(call_expr.type_arguments(), call_expr.id())?
                .into_iter()
                .map(|type_arg| {
                    tcx.instantiate_flat_type_from(&type_arg, &type_parameters, &generics.types)
                        .to_owned()
                })
                .collect::<Vec<_>>();

            let call_instance = Instance {
                id: callable_id,
                generics: Some(Generics {
                    ids: generic_params,
                    types: generic_args,
                }),
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

#[tracing::instrument(level = "DEBUG", skip_all, fields(package = mcx.tcx().current_package().name), err)]
fn collect_monotypes(mcx: &MirQueryCtx<'_>, items: &mut MonoItems) -> Result<()> {
    // TODO:
    // I'd like to believe there's a better (ie. less-allocating)
    // way of doing this.

    let mut visited = IndexSet::new();
    let mut worklist = IndexSet::new();

    // NOTE:
    // This first "seeding" of the worklist contains instances which refer to
    // *callables*, where-as all later entries will refer to *types*.
    for instance in items.instances.values().flatten() {
        worklist.insert(instance.clone());
    }

    while let Some(instance) = worklist.pop() {
        if visited.contains(&instance) {
            continue;
        }

        let instance_generics = instance.generics.clone().unwrap_or_default();

        if mcx.tcx().tdb().expect_type(instance.id).is_ok() {
            let instance_type = TypeRef {
                instance_of: instance.id,
                bound_types: instance_generics.types.clone(),
                ..Default::default()
            };

            for field in mcx.tcx().hir_fields_on(instance.id)? {
                let field_type = mcx.tcx().mk_type_ref_from(&field.field_type, instance.id)?;
                let field_instance = monotype_of(mcx, field_type, &instance_generics);

                worklist.insert(field_instance);
            }

            for method in mcx.tcx().methods_defined_on(&instance_type) {
                for parameter in mcx.tcx().parameters_of(method.id) {
                    let parameter_type = mcx.tcx().mk_type_ref_from(&parameter.param_type, method.id)?;
                    let parameter_instance = monotype_of(mcx, parameter_type, &instance_generics);

                    worklist.insert(parameter_instance);
                }

                let return_type = mcx
                    .tcx()
                    .mk_type_ref_from(mcx.tcx().return_type_of(method.id).unwrap(), method.id)?;

                let return_instance = monotype_of(mcx, return_type, &instance_generics);
                worklist.insert(return_instance);
            }

            items.types.insert(instance_type);
        }

        for bound_type in &instance_generics.types {
            worklist.insert(mcx.instance_of_type(bound_type.clone()));
        }

        visited.insert(instance);
    }

    for generic_type in &items.types {
        tracing::trace!(
            name = mcx
                .tcx()
                .ty_stringifier(generic_type)
                .include_namespace(true)
                .stringify()?,
            "monotype"
        );
    }

    Ok(())
}

/// Create an instantiated [`Instance`] from the given [`TypeRef`], using the
/// generics from the surrounding context.
fn monotype_of(mcx: &MirQueryCtx, mut type_: TypeRef, generics: &Generics) -> Instance {
    // Replace all contained type parameters with their respective canonical type
    // parameter. This helps with mapping the correct type arguments into the
    // type-ref, as they might not use the same type parameter IDs.
    for bound_type in type_.walk_mut() {
        if !bound_type.bound_types.is_empty() || !mcx.tcx().is_type_parameter(bound_type) {
            continue;
        }

        let Some(owner) = mcx.tcx().hir_parent_node_of(bound_type.instance_of) else {
            continue;
        };

        if let Ok(Some(canonical)) = mcx
            .tcx()
            .hir_canonical_type_of(lume_hir::TypeId::from(bound_type.instance_of), owner.id())
        {
            bound_type.instance_of = canonical.as_node_id();
        }
    }

    for (type_parameter_id, replacement) in generics.iter() {
        type_.replace_contained(type_parameter_id, replacement);
    }

    mcx.instance_of_type(type_)
}
