use diag_macros::Diagnostic;

use crate::{
    driver::ModuleFileId,
    hir::{ItemId, NodeId},
};

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "No module source file of ID {id:?} was found.")]
pub struct InvalidModuleFileId {
    pub id: ModuleFileId,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "No item of ID {id:?} was found.")]
pub struct InvalidItemId {
    pub id: ItemId,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "No node of ID {id:?} was found.")]
pub struct InvalidNodeId {
    pub id: NodeId,
}
