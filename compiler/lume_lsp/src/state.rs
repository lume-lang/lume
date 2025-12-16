use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

use crossbeam::channel::Sender;
use indexmap::IndexMap;
use lsp_server::*;
use lsp_types::Uri;
use lume_driver::CheckedPackageGraph;
use lume_errors::{DiagCtx, IntoDiagnostic, Result};
use lume_span::{FileName, Internable, Location, SourceFile};

use crate::symbols::lookup::SymbolLookup;

pub(crate) struct State {
    pub dispatcher: Sender<Message>,

    pub vfs: Vfs,

    pub checked: CheckedWorkspace,

    pub error_files_prev: RwLock<HashSet<Uri>>,
    pub error_files_curr: RwLock<HashSet<Uri>>,

    pub dcx: DiagCtx,
}

impl State {
    pub fn new(dispatcher: Sender<Message>, root: Uri) -> Self {
        Self {
            dispatcher,
            vfs: Vfs::new(root),
            checked: CheckedWorkspace::default(),

            error_files_prev: RwLock::new(HashSet::new()),
            error_files_curr: RwLock::new(HashSet::new()),
            dcx: DiagCtx::new(),
        }
    }

    /// Checks the current workspace and sends any raised diagnostics to the
    /// client.
    pub(crate) fn compile_workspace(&mut self) {
        log::debug!("compiling workspace at {}", self.vfs.workspace_root.as_str());

        std::mem::take(&mut self.error_files_prev);
        std::mem::swap(&mut self.error_files_prev, &mut self.error_files_curr);

        let path = PathBuf::from(self.vfs.workspace_root.as_str());
        let handle = self.dcx.handle();

        let check = || -> lume_errors::Result<CheckedPackageGraph> {
            let driver = lume_driver::Driver::from_root(&path, handle)?;
            let source_overrides = self.vfs.build_source_overrides();

            driver.check(lume_session::Options {
                source_overrides: Some(source_overrides),
                ..Default::default()
            })
        };

        match check() {
            Ok(packages) => {
                self.checked.update_symbol_lookup(packages);
            }
            Err(err) => {
                self.dcx.emit(err);
                self.drain_dcx_diagnostics();
            }
        }
    }

    pub(crate) fn source_of_uri(&self, uri: &Uri) -> Option<Arc<SourceFile>> {
        let file_path = PathBuf::from(uri.path().as_str());

        for package in self.checked.graph.packages.values() {
            for source in package.sources.iter() {
                if file_path.ends_with(source.name.to_pathbuf()) {
                    return Some(source.clone());
                }
            }
        }

        None
    }

    pub(crate) fn location_of(&self, uri: &Uri, line: usize, column: usize) -> Option<Location> {
        let source_file = self.vfs.get_document(uri)?;

        let mut index = column;
        for (line_idx, line_str) in source_file.file.content.lines().enumerate() {
            if line_idx >= line {
                break;
            }

            // +1 for the newline.
            index += line_str.len() + 1;
        }

        let range = index..index + 1;

        Some(
            lume_span::source::Location {
                file: source_file.file.clone(),
                index: range,
            }
            .intern(),
        )
    }

    pub(crate) fn ok<T: serde::Serialize>(&self, id: RequestId, message: &T) -> Result<()> {
        let value = match serde_json::to_value(message) {
            Ok(val) => val,
            Err(err) => return Err(err.into_diagnostic()),
        };

        let resp = Response::new_ok(id, value);

        match self.dispatcher.send(Message::Response(resp)) {
            Ok(()) => Ok(()),
            Err(err) => Err(err.into_diagnostic()),
        }
    }

    pub(crate) fn err(&self, id: RequestId, code: ErrorCode, message: &str) -> Result<()> {
        let resp = Response::new_err(id, code as i32, message.into());

        match self.dispatcher.send(Message::Response(resp)) {
            Ok(()) => Ok(()),
            Err(err) => Err(err.into_diagnostic()),
        }
    }
}

/// Uniquely identifies a source file.
///
/// Each source file has a parent [`PackageId`], which defines which package it
/// belongs to.
#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceFileId(usize);

impl From<&Uri> for SourceFileId {
    fn from(value: &Uri) -> Self {
        Self(lume_span::hash_id(&value))
    }
}

pub(crate) struct Vfs {
    pub(crate) workspace_root: Uri,

    source_files: IndexMap<SourceFileId, MappedSourceFile>,
}

impl Vfs {
    pub fn new(root: Uri) -> Self {
        Self {
            workspace_root: root,
            source_files: IndexMap::new(),
        }
    }

    pub fn get_document(&self, uri: &Uri) -> Option<&MappedSourceFile> {
        self.source_files.values().find(|file| &file.uri == uri)
    }

    pub fn add_document(&mut self, uri: Uri, file: Arc<SourceFile>) {
        let id: SourceFileId = (&uri).into();

        self.source_files.insert(id, MappedSourceFile { uri, file });
    }

    pub fn remove_document(&mut self, uri: &Uri) -> bool {
        let id: SourceFileId = uri.into();

        self.source_files.swap_remove(&id).is_some()
    }

    pub fn change_document(&mut self, uri: &Uri, content: String) {
        let Some(document) = self.get_document(uri) else {
            return;
        };

        self.add_document(
            uri.to_owned(),
            Arc::new(SourceFile {
                id: document.file.id,
                name: document.file.name.clone(),
                content,
                package: document.file.package,
            }),
        );
    }

    /// Builds the overrides of source files which we currently have in-memory
    /// in the language server.
    ///
    /// Some of these might not need to be overwritten, as they are the same as
    /// they are on the disk. But, since the operation is a
    /// [`IndexMap::extend`]-call, it's a relatively quick operation.
    fn build_source_overrides(&self) -> IndexMap<FileName, String> {
        let mut source_overrides = IndexMap::new();

        for source_file in self.source_files.values() {
            let file_path = PathBuf::from(source_file.uri.path().as_str());
            let workspace_root = self.workspace_root.path().as_str();

            let relative_path = if file_path.starts_with(workspace_root) {
                FileName::Real(file_path.strip_prefix(workspace_root).unwrap().to_path_buf())
            } else {
                FileName::Real(file_path)
            };

            source_overrides.insert(relative_path, source_file.file.content.clone());
        }

        source_overrides
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct MappedSourceFile {
    pub(crate) uri: Uri,
    pub(crate) file: Arc<SourceFile>,
}

#[derive(Default)]
pub(crate) struct CheckedWorkspace {
    pub graph: CheckedPackageGraph,
    pub symbols: SymbolLookup,
}

impl CheckedWorkspace {
    pub fn update_symbol_lookup(&mut self, graph: CheckedPackageGraph) {
        let mut symbols = SymbolLookup::default();
        for package in graph.packages.values() {
            let package_symbols = match SymbolLookup::from_hir(package.tcx.hir()) {
                Ok(syms) => syms,
                Err(err) => {
                    log::error!(
                        "error while updating symbol graph for package {}: {}",
                        package.package,
                        err.message()
                    );
                    continue;
                }
            };

            symbols.extend(package_symbols);
        }

        self.graph = graph;
        self.symbols = symbols;
    }
}
