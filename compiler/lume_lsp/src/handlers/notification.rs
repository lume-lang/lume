use std::path::PathBuf;
use std::str::FromStr;
use std::sync::Arc;

use lsp_types::*;
use lume_span::SourceFile;

use crate::state::State;

pub(crate) fn open_document(state: &mut State, params: DidOpenTextDocumentParams) {
    log::info!("added document {}", params.text_document.uri.as_str());

    fn find_workspace_root(state: &mut State, params: &DidOpenTextDocumentParams) -> Option<Arc<SourceFile>> {
        let uri = &params.text_document.uri;

        if let Some(source_file) = state.source_of_uri(&uri) {
            return Some(source_file);
        }

        // If we don't currently have a current workspace, try to locate the
        // workspace root by iterating the parent directories of the newly-opened file.
        if state.checked.graph.packages.is_empty() {
            let mut iter_path = PathBuf::from(uri.path().as_str());

            while let Some(directory) = iter_path.parent() {
                if !directory.join("Arcfile").exists() {
                    iter_path = directory.to_path_buf();
                    continue;
                }

                let workspace_root = format!("file://{}", directory.to_str().unwrap());
                state.vfs.workspace_root = Uri::from_str(&workspace_root).unwrap();
                state.compile_workspace();

                return state.source_of_uri(&uri);
            }
        }

        None
    }

    let Some(source_file) = find_workspace_root(state, &params) else {
        log::error!("could not find any matching package");
        return;
    };

    let TextDocumentItem { uri, text, .. } = params.text_document;

    state.vfs.add_document(
        uri,
        Arc::new(SourceFile {
            id: source_file.id,
            name: source_file.name.clone(),
            content: text,
            package: source_file.package,
        }),
    );

    state.compile_workspace();
}

pub(crate) fn close_document(state: &mut State, params: DidCloseTextDocumentParams) {
    log::info!("removed document {}", params.text_document.uri.as_str());

    state.vfs.remove_document(&params.text_document.uri);

    state.compile_workspace();
}

pub(crate) fn save_document(state: &mut State, params: DidSaveTextDocumentParams) {
    log::info!("updated document {} (via save)", params.text_document.uri.as_str());

    state
        .vfs
        .change_document(&params.text_document.uri, params.text.unwrap());

    state.compile_workspace();
}

pub(crate) fn change_document(state: &mut State, params: DidChangeTextDocumentParams) {
    log::info!("updated document {} (via change)", params.text_document.uri.as_str());

    let source = params.content_changes.first().unwrap().text.clone();
    state.vfs.change_document(&params.text_document.uri, source);

    state.compile_workspace();
}
