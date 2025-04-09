use crate::hir::Symbol;

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub(crate) struct LookupTable {
    pub(crate) symbols: Vec<Symbol>,
}

impl LookupTable {
    pub(crate) fn new() -> Self {
        Self { symbols: Vec::new() }
    }
}
