use lume_diag_macros::Diagnostic;
use lume_span::SourceFileId;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "Could not find source file with ID {id:?}", code = "LM0600")]
pub struct MissingSourceFile {
    pub id: SourceFileId,
}
