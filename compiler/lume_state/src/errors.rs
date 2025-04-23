use lume_diag_macros::Diagnostic;

use crate::ModuleFileId;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Could not find source file with ID {id:?}", code = "LM0600")]
pub struct MissingSourceFile {
    pub id: ModuleFileId,
}
