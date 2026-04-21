use lume_span::NodeId;
use lume_types::TypeRef;
use serde::{Deserialize, Serialize};

use crate::TyInferCtx;

#[derive(Serialize, Deserialize, Hash, Debug, Clone, PartialEq, Eq)]
pub struct Instance {
    /// ID of the method or function which this instance represents.
    pub id: NodeId,

    /// Generic arguments for the callable instance
    pub generics: Option<Generics>,
}

impl Instance {
    #[inline]
    pub fn as_usize(&self) -> usize {
        lume_hash::portable_hash(self)
    }

    #[inline]
    pub fn generics_or_empty(&self) -> &Generics {
        self.generics.as_ref().unwrap_or(&EMPTY_GENERICS)
    }

    pub fn display<'tcx>(&'tcx self, tcx: &'tcx TyInferCtx) -> InstanceDisplay<'tcx> {
        InstanceDisplay(self, tcx)
    }
}

impl From<NodeId> for Instance {
    fn from(value: NodeId) -> Self {
        Self {
            id: value,
            generics: None,
        }
    }
}

pub struct InstanceDisplay<'tcx>(&'tcx Instance, &'tcx TyInferCtx);

impl std::fmt::Display for InstanceDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let InstanceDisplay(instance, tcx) = self;

        write!(f, "{:+}", tcx.hir_path_of_node(instance.id))?;

        if let Some(generics) = &instance.generics
            && !generics.is_empty()
        {
            write!(
                f,
                "<{}>",
                generics
                    .iter()
                    .filter_map(|(_id, generic)| tcx.ty_stringifier(generic).stringify().ok())
                    .collect::<Vec<String>>()
                    .join(", ")
            )?;
        }

        Ok(())
    }
}

#[derive(Serialize, Deserialize, Default, Hash, Debug, Clone, PartialEq, Eq)]
pub struct Generics {
    /// Node IDs of the type parameters which are populated by [`Self::types`]
    pub ids: Vec<NodeId>,

    /// Type arguments for the matching type parameters.
    pub types: Vec<TypeRef>,
}
pub static EMPTY_GENERICS: Generics = Generics {
    ids: Vec::new(),
    types: Vec::new(),
};

impl Generics {
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.ids.is_empty()
    }

    #[inline]
    pub fn len(&self) -> usize {
        debug_assert_eq!(self.ids.len(), self.types.len());
        self.ids.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (NodeId, &TypeRef)> {
        self.ids.iter().copied().zip(self.types.iter())
    }
}
