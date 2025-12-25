mod diagnostics;
mod io;
mod reporter;

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crossbeam::channel::Sender;
use diagnostics::Diagnostics;
use io::IO;
use lsp_types::Uri;
use lume_errors::{Result, SimpleDiagnostic};
use lume_span::{Internable, Location, PackageId, SourceFile};

use crate::engine::reporter::Reporter;
use crate::listener::FileLocation;
use crate::symbols::lookup::{SymbolEntry, SymbolLookup};

pub(crate) struct Engine {
    /// Defines the root directory of the project.
    root: PathBuf,

    sender: Sender<lsp_server::Message>,

    /// Virtual file system for all source files in the current project.
    io: IO,

    /// Reporter for sending progress updates to the client.
    reporter: Reporter,

    diagnostics: Diagnostics,

    symbols: SymbolLookup,
    packages: HashMap<PackageId, lume_driver::CheckedPackage>,
}

impl Engine {
    pub fn new(root: PathBuf, sender: Sender<lsp_server::Message>) -> Self {
        let reporter = Reporter::new(sender.clone());
        let diagnostics = Diagnostics::new(root.clone());

        Self {
            root,
            sender,
            io: IO::default(),
            reporter,
            diagnostics,
            symbols: SymbolLookup::default(),
            packages: HashMap::new(),
        }
    }

    pub fn compile(&mut self) {
        log::info!("compiling workspace at {}", self.root.display());

        self.reporter.check_started();

        let result = self.with_result(|engine| {
            let driver = lume_driver::Driver::from_root(&engine.root, engine.diagnostics.dcx.handle())?;

            let source_overrides = engine.io.build_source_overrides(&engine.root);
            let options = lume_session::Options {
                source_overrides: Some(source_overrides),
                ..Default::default()
            };

            driver.check(options)
        });

        if let Some(checked_graph) = result {
            self.reporter.check_indexing();
            self.update_symbol_lookup(checked_graph);
        }

        self.reporter.check_finished();
        self.emit_diagnostics();
    }

    fn with_result<T, F: FnOnce(&mut Self) -> Result<T>>(&mut self, f: F) -> Option<T> {
        match f(self) {
            Ok(result) => Some(result),
            Err(err) => {
                self.diagnostics.dcx.emit(err);
                None
            }
        }
    }

    /// Emits all diagnostics to the LSP client, which have been emitted since
    /// the last emision.
    #[inline]
    fn emit_diagnostics(&mut self) {
        self.diagnostics.drain_to(&self.sender);
    }

    /// Resolves a path relative to the root of the project. If the path is
    /// absolute, it is returned as-is.
    fn resolve_path(&self, path: PathBuf) -> PathBuf {
        if path.is_relative() { self.root.join(path) } else { path }
    }

    /// Gets the package with the given ID, if any.
    pub(crate) fn package(&self, id: PackageId) -> Option<&lume_driver::CheckedPackage> {
        self.packages.get(&id)
    }

    /// Gets the package source file which exists to the given path.
    pub(crate) fn source_of_uri(&self, file_path: &Path) -> Option<Arc<SourceFile>> {
        for package in self.packages.values() {
            for source in package.sources.iter() {
                if file_path.ends_with(source.name.to_pathbuf()) {
                    return Some(source.clone());
                }
            }
        }

        None
    }

    /// Gets the location of an LSP location as a range within a source file.
    pub(crate) fn location_from_lsp(&self, location: &FileLocation) -> Result<Location> {
        let path = self.resolve_path(crate::uri_to_path(&location.uri));

        let Some(source_file) = self.source_of_uri(&path) else {
            return Err(SimpleDiagnostic::new(format!("could not find source file: {}", path.display())).into());
        };

        let index = crate::index_from_position(&source_file.content, location.position);

        Ok(lume_span::source::Location {
            file: source_file,
            index: index..index + 1,
        }
        .intern())
    }

    /// Locates the symbol within the given location.
    pub(crate) fn locate_node(&self, location: Location) -> Option<&SymbolEntry> {
        self.symbols.lookup_position(location)
    }

    /// Updates the symbol lookup table with the given package graph.
    fn update_symbol_lookup(&mut self, graph: lume_driver::CheckedPackageGraph) {
        let mut symbols = SymbolLookup::default();
        for package in graph.packages.values() {
            let package_symbols = SymbolLookup::from_hir(package.tcx.hir());
            symbols.extend(package_symbols);
        }

        self.packages = graph.packages;
        self.symbols = symbols;
    }
}

impl Engine {
    pub(crate) fn open_document(&mut self, uri: Uri, content: String) {
        let path = crate::uri_to_path(&uri);
        log::info!("added document {} to vfs", path.display());

        self.io.map(path, content);
    }

    pub(crate) fn close_document(&mut self, uri: Uri) {
        let path = crate::uri_to_path(&uri);
        log::info!("removed document {} from vfs", path.display());

        self.io.unmap(&path);
    }

    pub(crate) fn save_document(&mut self, uri: Uri) {
        let path = crate::uri_to_path(&uri);
        log::debug!("updated document {} (via save)", path.display());

        self.io.unmap(&path);
        self.compile();
    }

    pub(crate) fn update_document(&mut self, uri: Uri, content: String) {
        let path = crate::uri_to_path(&uri);
        log::debug!("updated document {} (via change)", path.display());

        self.io.map(path, content);
    }
}

impl Engine {
    pub(crate) fn hover(&self, location: FileLocation) -> Result<lsp_types::Hover> {
        let location = self.location_from_lsp(&location)?;
        let hover_markdown = crate::symbols::hover::hover_content_of(self, location)?;

        Ok(lsp_types::Hover {
            contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value: hover_markdown,
            }),
            range: None,
        })
    }

    pub(crate) fn go_to_definition(&self, location: FileLocation) -> Result<lsp_types::GotoDefinitionResponse> {
        let location = self.location_from_lsp(&location)?;
        let Some(definition_location) = crate::symbols::definition::definition_of(self, location) else {
            return Err(lume_errors::SimpleDiagnostic::new(format!("could not find definition for {location}")).into());
        };

        log::debug!("found definition at {definition_location}");

        let definition_pathbuf = definition_location.file.name.to_pathbuf().clone();
        let absolute_path = self.resolve_path(definition_pathbuf);

        Ok(lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location {
            uri: crate::path_to_uri(&absolute_path),
            range: crate::location_range(definition_location),
        }))
    }
}
