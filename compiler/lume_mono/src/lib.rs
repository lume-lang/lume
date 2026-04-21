use indexmap::{IndexMap, IndexSet};
use lume_mir::{Generics, Instance};
use lume_span::NodeId;

pub(crate) mod collector;
pub use collector::collect;

pub(crate) mod canonicalize;
pub use canonicalize::canonicalize;
use lume_types::TypeRef;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct MonoItems {
    instances: IndexMap<NodeId, IndexSet<Instance>>,
    types: IndexSet<TypeRef>,
}

impl MonoItems {
    pub fn push_instance(&mut self, item: Instance) {
        self.instances.entry(item.id).or_default().insert(item);
    }

    pub fn iter(&self) -> impl Iterator<Item = &Instance> {
        self.instances.values().flatten()
    }

    pub fn any_of(&self, id: NodeId) -> bool {
        self.instances.get(&id).is_some_and(|set| !set.is_empty())
    }

    pub fn all_of(&self, id: NodeId) -> impl Iterator<Item = &Instance> {
        static EMPTY: &indexmap::set::Slice<Instance> = indexmap::set::Slice::<Instance>::new();

        self.instances.get(&id).map_or(EMPTY.iter(), |set| set.iter())
    }
}

impl IntoIterator for MonoItems {
    type IntoIter = std::iter::Flatten<indexmap::map::IntoValues<NodeId, IndexSet<Instance>>>;
    type Item = Instance;

    fn into_iter(self) -> Self::IntoIter {
        self.instances.into_values().flatten()
    }
}

impl Extend<Instance> for MonoItems {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = Instance>,
    {
        for item in iter {
            self.push_instance(item);
        }
    }
}
