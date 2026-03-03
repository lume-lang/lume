pub(crate) mod attr;
pub(crate) mod errors;
pub(crate) mod expr;
pub(crate) mod generics;
pub(crate) mod item;
pub(crate) mod lit;
pub(crate) mod path;
pub(crate) mod pattern;
pub(crate) mod stmt;
pub(crate) mod ty;

use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::{Arc, LazyLock};

pub use lume_ast;
pub use lume_ast::Node as _;
use lume_errors::{DiagCtxHandle, Result};
pub use lume_hir::WithLocation as _;
use lume_hir::symbols::SymbolTable;
use lume_hir::{Map, Path, PathSegment};
use lume_session::Package;
use lume_span::{Internable, Location, NodeId, SourceFile, SourceFileId};

static DEFAULT_STD_IMPORTS: LazyLock<Vec<(&str, Path)>> = LazyLock::new(|| {
    vec![
        ("Boolean", lume_hir::hir_std_type_path!(Boolean)),
        ("String", lume_hir::hir_std_type_path!(String)),
        ("Int8", lume_hir::hir_std_type_path!(Int8)),
        ("UInt8", lume_hir::hir_std_type_path!(UInt8)),
        ("Int16", lume_hir::hir_std_type_path!(Int16)),
        ("UInt16", lume_hir::hir_std_type_path!(UInt16)),
        ("Int32", lume_hir::hir_std_type_path!(Int32)),
        ("UInt32", lume_hir::hir_std_type_path!(UInt32)),
        ("Int64", lume_hir::hir_std_type_path!(Int64)),
        ("UInt64", lume_hir::hir_std_type_path!(UInt64)),
        ("IntPtr", lume_hir::hir_std_type_path!(IntPtr)),
        ("UIntPtr", lume_hir::hir_std_type_path!(UIntPtr)),
        ("Float", lume_hir::hir_std_type_path!(Float)),
        ("Double", lume_hir::hir_std_type_path!(Double)),
        ("Array", lume_hir::hir_std_type_path!(Array)),
        ("Pointer", lume_hir::hir_std_type_path!(Pointer)),
        ("Range", lume_hir::hir_std_type_path!(Range)),
        ("RangeInclusive", lume_hir::hir_std_type_path!(RangeInclusive)),
    ]
});

pub fn lower_to_hir(package: &Package, dcx: DiagCtxHandle) -> Result<Map> {
    let mut ctx = LoweringContext::new(package, dcx);

    for (_file_name, source_file) in ctx.package.files.clone() {
        let arena = lume_data_structures::UntypedArena::new();

        let syntax_tree = ctx.dcx.with(|handle| {
            let mut lexer = lume_lexer::Lexer::new(source_file.clone());
            let tokens = lexer.lex()?;

            let mut parser = lume_parser::Parser::new(source_file.clone(), tokens, handle, &arena);
            parser.parse()
        })?;

        if let Err(err) = ctx.lower_items(source_file.id, syntax_tree.items) {
            ctx.dcx.emit_and_push(err);
        }
    }

    Ok(ctx.map)
}

#[derive(Hash, Debug, PartialEq, Eq)]
pub enum DefinedItem {
    Function(Path),
    Method(Path, PathSegment),
    Type(Path),
}

impl DefinedItem {
    pub fn path(&self) -> Path {
        match self {
            Self::Function(path) | Self::Type(path) => path.clone(),
            Self::Method(path, name) => Path::with_root(path.clone(), name.clone()),
        }
    }

    pub fn location(&self) -> Location {
        self.path().location
    }
}

pub struct LoweringContext<'pkg> {
    package: &'pkg Package,
    dcx: DiagCtxHandle,
    map: Map,

    current_node: NodeId,
    current_file_id: SourceFileId,

    namespace: Option<Path>,
    imports: HashMap<String, Path>,
    defined: HashMap<DefinedItem, NodeId>,

    self_type: Option<Path>,
    current_locals: SymbolTable<String, lume_hir::VariableSource>,
    current_type_params: SymbolTable<String, lume_hir::TypeId>,
}

impl<'pkg> LoweringContext<'pkg> {
    /// Creates a new lowering context for creating HIR maps from AST.
    pub fn new(package: &'pkg Package, dcx: DiagCtxHandle) -> Self {
        let map = Map::empty(package.id);

        // TODO:
        // Scuffed solution for not overwriting the static IDs of the compiler's scalar
        // types. 0x100 is a completely arbitrary number.
        let current_node = NodeId::from_usize(package.id, 0x100);

        Self {
            package,
            dcx,
            map,
            current_node,
            current_file_id: SourceFileId::empty(),
            namespace: None,
            imports: HashMap::new(),
            defined: HashMap::new(),
            self_type: None,
            current_locals: SymbolTable::new(),
            current_type_params: SymbolTable::new(),
        }
    }
}

impl LoweringContext<'_> {
    /// Gets a reference to the current source file.
    fn current_file(&self) -> &Arc<SourceFile> {
        self.package
            .iter_sources()
            .find(|file| file.id == self.current_file_id)
            .expect("invalid source file ID")
    }

    /// Gets the next [`NodeId`] in the sequence.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn next_node_id(&mut self) -> NodeId {
        self.current_node = self.current_node.next();
        self.current_node
    }

    /// Lowers the given AST identifier into a [`lume_hir::Identifier`].
    pub(crate) fn identifier(&self, expr: lume_ast::Identifier) -> lume_hir::Identifier {
        let location = self.location(expr.location.clone());

        lume_hir::Identifier {
            name: expr.name.to_string(),
            location,
        }
    }

    /// Lowers the given AST location into a [`Location`].
    fn location(&self, expr: lume_ast::Location) -> Location {
        lume_span::source::Location {
            file: self.current_file().clone(),
            index: expr.0,
        }
        .intern()
    }

    /// Executes a closure with the current `Self` type set to the given name.
    pub(crate) fn with_self_as<R, F: FnOnce(&mut Self) -> R>(&mut self, name: Path, f: F) -> R {
        self.self_type = Some(name);
        let result = f(self);
        self.self_type = None;
        result
    }

    /// Ensure that the item with the given name is undefined within the file.
    ///
    /// If the item is not defined, it is added into the list of defined items.
    /// If the item is defined, raises an error to reflect it.
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn ensure_item_undefined(&mut self, id: NodeId, item: DefinedItem) -> Result<()> {
        if let Some((existing, _)) = self.defined.get_key_value(&item) {
            return Err(crate::errors::DuplicateDefinition {
                duplicate_range: lume_span::source::Location {
                    file: self.current_file().clone(),
                    index: item.location().index.clone(),
                }
                .intern(),
                original_range: existing.location(),
                name: item.path().to_string(),
            }
            .into());
        }

        self.defined.insert(item, id);

        Ok(())
    }

    /// Gets the [`lume_hir::Path`] for the item with the given name.
    pub(crate) fn resolve_symbol_name(&mut self, path: &lume_ast::Path) -> Result<Path> {
        if path.root.first().is_some_and(|segment| segment.is_self_type()) {
            return self.resolve_self_name(path);
        }

        if let Some(symbol) = self.resolve_imported_symbol(path)? {
            return Ok(symbol.clone());
        }

        let has_namespace_root = path
            .root
            .iter()
            .any(|root| matches!(root, lume_ast::PathSegment::Namespace { .. }));

        let mut root = if let Some(namespace) = &self.namespace
            && !has_namespace_root
        {
            namespace.clone().as_root()
        } else {
            Vec::new()
        };

        root.extend(self.path_segments(path.root.clone())?);

        Ok(Path {
            root,
            name: self.path_segment(path.name.clone())?,
            location: self.location(path.location.clone()),
        })
    }

    /// Gets the [`lume_hir::Path`] for the item with the given name, within the
    /// current parent type.
    fn resolve_self_name(&mut self, path: &lume_ast::Path) -> Result<Path> {
        debug_assert!(path.root.first().is_some_and(|seg| seg.is_self_type()));

        let mut selfless_path = path.clone();
        let self_segment = selfless_path.root.remove(0);

        if let Some(self_path) = self.self_type.clone() {
            return Ok(Path::join(self_path, self.path(selfless_path)?));
        }

        Err(errors::SelfOutsideObjectContext {
            source: self.current_file().clone(),
            range: self_segment.location().0.clone(),
            ty: String::from("Self"),
        }
        .into())
    }

    /// Attemps to resolve a [`lume_hir::Path`] for an imported symbol.
    pub(crate) fn resolve_imported_symbol(&mut self, path: &lume_ast::Path) -> Result<Option<Path>> {
        for (import, symbol) in &self.imports {
            // Match against imported paths, which match the first segment of the imported
            // path.
            //
            // This handles situations where a subpath was imported, which is then
            // referenced later, such as:
            //
            // ```lume
            //     import std (io);
            //
            //     io::File::from_path("foo.txt");
            // ```
            if let Some(name) = path.root.first() {
                if name.name().name == *import {
                    // Since we only matched a subset of the imported symbol,
                    // we need to merge the two paths together, so it forms a fully-qualified path.
                    let mut root: Vec<PathSegment> = symbol.root.clone();

                    for segment in &path.root {
                        root.push(self.path_segment(segment.clone())?);
                    }

                    return Ok(Some(Path {
                        root,
                        name: self.path_segment(path.name.clone())?,
                        location: self.location(path.location.clone()),
                    }));
                }
            }
            // Match against the the imported symbol directly, such as:
            //
            // ```lume
            //     import std.io (File);
            //
            //     File::from_path("foo.txt");
            // ```
            else if path.name.name().name == *import {
                return Ok(Some(Path {
                    root: symbol.root.clone(),
                    name: self.path_segment(path.name.clone())?,
                    location: self.location(path.location.clone()),
                }));
            }
        }

        Ok(None)
    }

    pub(crate) fn lower_items<'ast, I>(&mut self, source_file_id: SourceFileId, items: I) -> Result<()>
    where
        I: IntoIterator<Item = lume_ast::Item<'ast>>,
    {
        self.current_file_id = source_file_id;
        self.namespace = None;

        self.imports.clear();
        for (imported_name, import_path) in DEFAULT_STD_IMPORTS.iter() {
            self.imports.insert(imported_name.to_string(), import_path.clone());
        }

        for item in items {
            self.lower_item(item)?;
        }

        for (id, node) in &self.map.nodes {
            debug_assert_eq!(
                *id,
                node.id(),
                "mismatched between ID key and value: {id:?} != {:?}",
                node.id()
            );
        }

        for import in self.imports.values() {
            self.map.imports.insert(import.clone());
        }

        self.dcx.ensure_untainted()
    }

    pub(crate) fn lower_item(&mut self, item: lume_ast::Item) -> Result<()> {
        self.current_locals.clear();
        self.current_type_params.clear();
        self.self_type = None;

        self.item(item)?;

        self.dcx.ensure_untainted()
    }
}

pub(crate) trait Unique {
    /// Gets the name of the node.
    fn name(&self) -> Cow<'_, str>;
}

impl<T: Unique> Unique for &T {
    fn name(&self) -> Cow<'_, str> {
        Unique::name(*self)
    }
}

impl Unique for lume_hir::Identifier {
    fn name(&self) -> Cow<'_, str> {
        Cow::Borrowed(&self.name)
    }
}

impl Unique for lume_hir::Path {
    fn name(&self) -> Cow<'_, str> {
        Cow::Owned(self.to_wide_string())
    }
}

impl Unique for lume_hir::PathSegment {
    fn name(&self) -> Cow<'_, str> {
        Cow::Borrowed(self.name().as_str())
    }
}

impl Unique for lume_hir::Parameter {
    fn name(&self) -> Cow<'_, str> {
        Cow::Borrowed(&self.name.name)
    }
}

impl Unique for lume_hir::TypeParameter {
    fn name(&self) -> Cow<'_, str> {
        Cow::Borrowed(&self.name.name)
    }
}

impl Unique for lume_hir::Type {
    fn name(&self) -> Cow<'_, str> {
        Unique::name(&self.name)
    }
}

impl Unique for lume_hir::Field {
    fn name(&self) -> Cow<'_, str> {
        Cow::Borrowed(&self.name.name)
    }
}

impl Unique for lume_hir::MethodDefinition {
    fn name(&self) -> Cow<'_, str> {
        Cow::Borrowed(self.signature.name.name().as_str())
    }
}

impl Unique for lume_hir::TraitMethodDefinition {
    fn name(&self) -> Cow<'_, str> {
        Cow::Borrowed(self.signature.name.name().as_str())
    }
}

impl Unique for lume_hir::TraitMethodImplementation {
    fn name(&self) -> Cow<'_, str> {
        Cow::Borrowed(&self.name.name)
    }
}

impl Unique for lume_hir::EnumDefinitionCase {
    fn name(&self) -> Cow<'_, str> {
        Unique::name(&self.name)
    }
}

impl LoweringContext<'_> {
    /// Ensures that the given series of items are unique.
    ///
    /// If a duplicate is found, the provided closure is called with the
    /// duplicate item and the existing item. The closure should return an
    /// error, which is reported to the current diagnostic context.
    pub(crate) fn ensure_unique_series<'a, T, F>(&self, items: &'a [T], on_duplicate: F) -> Result<()>
    where
        T: Unique,
        F: Fn(&T, &T) -> lume_errors::Error,
    {
        let mut seen = HashMap::<Cow<'a, str>, &T>::with_capacity(items.len());

        for item in items {
            let item_name = Unique::name(item);

            if let Some(existing) = seen.get(&item_name) {
                self.dcx.emit_and_push(on_duplicate(item, existing));
            }

            seen.insert(item_name, item);
        }

        self.dcx.ensure_untainted()
    }
}
