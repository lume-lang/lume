use std::path::PathBuf;
use std::str::FromStr;
use std::sync::Arc;

use lsp_types::*;
use lume_span::SourceFile;

use crate::state::State;

pub(crate) fn open_document(state: &mut State, uri: Uri, content: String) {
    log::info!("added document {}", uri.as_str());

    fn find_workspace_root(state: &mut State, uri: &Uri) -> Option<Arc<SourceFile>> {
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

    let Some(source_file) = find_workspace_root(state, &uri) else {
        log::error!("could not find any matching package");
        return;
    };

    state.vfs.add_document(
        uri,
        Arc::new(SourceFile {
            id: source_file.id,
            name: source_file.name.clone(),
            content,
            package: source_file.package,
        }),
    );

    state.compile_workspace();
}

pub(crate) fn close_document(state: &mut State, uri: Uri) {
    log::info!("removed document {}", uri.as_str());

    state.vfs.remove_document(&uri);
    state.compile_workspace();
}

pub(crate) fn save_document(state: &mut State, uri: Uri, content: String) {
    log::info!("updated document {} (via save)", uri.as_str());

    state.vfs.change_document(&uri, content);
    state.compile_workspace();
}

pub(crate) fn change_document(state: &mut State, uri: Uri, content: String) {
    log::info!("updated document {} (via change)", uri.as_str());

    state.vfs.change_document(&uri, content);
    state.compile_workspace();
}
