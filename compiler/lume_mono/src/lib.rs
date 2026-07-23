use indexmap::{IndexMap, IndexSet};
use lume_span::NodeId;
use lume_typech::TyCheckCtx;
use lume_types::TypeRef;

pub(crate) mod collector;
pub use collector::collect;

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Instance {
    /// ID of the method or function which this instance represents.
    pub id: NodeId,

    /// Generic arguments for the callable instance
    pub generics: Vec<TypeRef>,
}

impl Instance {
    pub fn display<'tcx>(&'tcx self, tcx: &'tcx TyCheckCtx) -> InstanceDisplay<'tcx> {
        InstanceDisplay(self, tcx)
    }
}

pub struct InstanceDisplay<'tcx>(&'tcx Instance, &'tcx TyCheckCtx);

impl std::fmt::Display for InstanceDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let InstanceDisplay(instance, tcx) = self;

        write!(f, "{:+}", tcx.hir_path_of_node(instance.id))?;

        if !instance.generics.is_empty() {
            write!(
                f,
                "<{}>",
                instance
                    .generics
                    .iter()
                    .filter_map(|generic| tcx.ty_stringifier(generic).stringify().ok())
                    .collect::<Vec<String>>()
                    .join(", ")
            )?;
        }

        write!(f, "()")?;

        Ok(())
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct MonoItems {
    items: IndexMap<NodeId, IndexSet<Instance>>,
}

impl MonoItems {
    pub fn push(&mut self, item: Instance) {
        self.items.entry(item.id).or_default().insert(item);
    }

    pub fn iter(&self) -> impl Iterator<Item = &Instance> {
        self.items.values().flatten()
    }

    pub fn all_of(&self, id: NodeId) -> impl Iterator<Item = &Instance> {
        static EMPTY: &indexmap::set::Slice<Instance> = indexmap::set::Slice::<Instance>::new();

        self.items.get(&id).map_or(EMPTY.iter(), |set| set.iter())
    }
}

impl IntoIterator for MonoItems {
    type IntoIter = std::iter::Flatten<indexmap::map::IntoValues<NodeId, IndexSet<Instance>>>;
    type Item = Instance;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_values().flatten()
    }
}

impl Extend<Instance> for MonoItems {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = Instance>,
    {
        for item in iter {
            self.push(item);
        }
    }
}
