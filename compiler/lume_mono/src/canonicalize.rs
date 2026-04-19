use lume_mir_queries::MirQueryCtx;

use crate::Instance;

pub fn canonicalize(
    mcx: &MirQueryCtx<'_>,
    func: &lume_mir::Function,
    instance: &Instance,
) -> Option<lume_mir::Function> {
    let Some(generics) = &instance.generics else {
        return None;
    };

    if generics.is_empty() {
        return None;
    }

    let mut func = func.clone();
    func.instance = Some(instance.to_owned());

    Some(func)
}
