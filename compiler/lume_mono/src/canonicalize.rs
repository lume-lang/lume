use indexmap::IndexMap;
use lume_mir_queries::MirQueryCtx;
use lume_span::Internable;

use crate::Instance;

#[tracing::instrument(level = "DEBUG", skip_all, fields(package = mcx.tcx().current_package().name))]
pub fn canonicalize(mcx: &mut MirQueryCtx<'_>) {
    let mono_items = crate::collect(mcx).unwrap();
    let functions = mcx.mir().functions.values().map(|func| func.id).collect::<Vec<_>>();

    for func_id in functions {
        if !mono_items.any_of(func_id) {
            continue;
        }

        let base_instance = lume_mir::Instance::from(func_id);
        let base_mir_func = mcx.mir().instance(&base_instance);
        let mut mono_functions = IndexMap::new();

        for instance in mono_items.all_of(func_id) {
            let Some(canon_mir_func) = canonicalize_body(mcx, base_mir_func, instance) else {
                tracing::debug!("skipping canonicalization for {}", instance.display(mcx.tcx()));
                continue;
            };

            mono_functions.insert(instance.to_owned(), canon_mir_func);
        }

        if !mono_functions.is_empty() {
            mcx.mir_mut().functions.shift_remove(&base_instance);
            mcx.mir_mut().functions.extend(mono_functions);
        }
    }

    let mut visitor = UpdateCallInstance {
        mcx,
        function_instance: None,
    };

    let mut replacement_funcs = mcx.mir().functions.clone();

    for func in replacement_funcs.values_mut() {
        lume_mir::walk::walk_mut(func, &mut visitor);
    }

    mcx.mir_mut().functions.extend(replacement_funcs);
}

fn canonicalize_body(
    mcx: &MirQueryCtx<'_>,
    func: &lume_mir::Function,
    instance: &Instance,
) -> Option<lume_mir::Function> {
    let Some(generics) = &instance.generics else {
        return None;
    };

    let mut func = func.clone();
    func.instance = Some(instance.to_owned());

    let mangle_version = lume_mangle::Version::default();
    let mangle_instance = lume_mangle::Instance {
        id: func.id,
        generics: generics.iter().map(|(id, arg)| (id, arg.clone())).collect(),
    };

    func.name = instance.display(mcx.tcx()).to_string().intern();
    func.mangled_name = lume_mangle::mangled(mcx.tcx(), &mangle_instance, mangle_version)
        .unwrap_or_else(|_| panic!("bug!: could not mangle instance {}", instance.display(mcx.tcx())));

    Some(func)
}

struct UpdateCallInstance<'mcx, 'tcx> {
    mcx: &'mcx MirQueryCtx<'tcx>,
    function_instance: Option<Instance>,
}

impl lume_mir::walk::VisitorMut for UpdateCallInstance<'_, '_> {
    fn visit_function(&mut self, func: &mut lume_mir::Function) {
        func.instance.clone_into(&mut self.function_instance);
    }

    fn visit_declaration(&mut self, decl: &mut lume_mir::Declaration) {
        let Some(func_instance) = self.function_instance.as_ref() else {
            return;
        };

        if let lume_mir::DeclarationKind::Call {
            instance,
            name,
            type_args,
            ..
        } = decl.kind.as_mut()
        {
            let inst_instance = self
                .mcx
                .instantiated_instance(func_instance, instance.id, std::mem::take(type_args));

            tracing::trace!(
                owner = &func_instance.display(self.mcx.tcx()).to_string(),
                before = &instance.display(self.mcx.tcx()).to_string(),
                after = &inst_instance.display(self.mcx.tcx()).to_string(),
                "update_call_site"
            );

            let new_name = inst_instance
                .display(self.mcx.tcx())
                .to_string()
                .trim_end_matches(['(', ')'])
                .to_string();

            *instance = inst_instance;
            *name = new_name.intern();
        }
    }
}
