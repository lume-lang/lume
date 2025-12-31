use std::borrow::Cow;
use std::collections::HashMap;
use std::hash::Hash;
use std::sync::Arc;

use error_snippet::Result;
use lume_ast::Node;
use lume_errors::{DiagCtxHandle, Error};
use lume_hir::map::Map;
use lume_hir::symbols::*;
use lume_hir::{Path, PathSegment, TypeId};
use lume_lexer::Lexer;
use lume_parser::Parser;
use lume_session::Package;
use lume_span::{Internable, Location, NodeId, PackageId, SourceFile, SourceMap};

mod errors;

mod expr;
mod generic;
mod item;
mod lit;
mod path;
mod pattern;
mod stmt;
mod ty;

const ARRAY_STD_TYPE: &str = "Array";
const ARRAY_NEW_FUNC: &str = "new";
const ARRAY_PUSH_FUNC: &str = "push";

const RANGE_STD_TYPE: &str = "Range";
const RANGE_INCLUSIVE_STD_TYPE: &str = "RangeInclusive";
const RANGE_NEW_FUNC: &str = "new";

const DEFAULT_STD_IMPORTS: &[&str] = &[
    "Boolean",
    "String",
    "Int8",
    "UInt8",
    "Int16",
    "UInt16",
    "Int32",
    "UInt32",
    "Int64",
    "UInt64",
    "IntPtr",
    "UIntPtr",
    "Float",
    "Double",
    "Array",
    "Pointer",
    "Range",
    "RangeInclusive",
];

pub struct LowerState<'a> {
    /// Defines the package of the current project.
    package: &'a Package,

    /// Defines the global source map for all source files.
    source_map: &'a mut SourceMap,

    /// Defines the diagnostics context from the build context.
    dcx: DiagCtxHandle,
}

impl<'a> LowerState<'a> {
    /// Creates a new [`LowerState`] instance.
    pub fn new(package: &'a Package, source_map: &'a mut SourceMap, dcx: DiagCtxHandle) -> Self {
        Self {
            package,
            source_map,
            dcx,
        }
    }

    /// Lowers the current project and state into a HIR map.
    ///
    /// # Errors
    ///
    /// Returns `Err` if any AST nodes are invalid or exist in invalid
    /// locations.
    #[libftrace::traced(level = Info, fields(package = self.package.name))]
    pub fn lower_into(&mut self) -> Result<Map> {
        let map = self.dcx.with(|dcx| {
            let mut lower = LowerModule::new(self.package.id, dcx);

            for (_, source_file) in self.package.files.clone() {
                // Register source file in the state.
                self.source_map.insert(source_file.clone());

                // Parse the contents of the source file.
                let expressions = self.dcx.with(|handle| {
                    let mut lexer = Lexer::new(source_file.clone());
                    let tokens = lexer.lex()?;

                    let mut parser = Parser::new(source_file.clone(), tokens, handle);
                    parser.parse()
                })?;

                // Lowers the parsed module expressions down to HIR.
                if let Err(err) = lower.lower(source_file, expressions) {
                    lower.dcx.emit_and_push(err);
                }
            }

            Ok(lower.map)
        })?;

        self.dcx.ensure_untainted()?;

        Ok(map)
    }
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

pub struct LowerModule {
    /// Defines the file is being lowered.
    file: Arc<SourceFile>,

    /// Handle to the diagnostics context which will handle parsing errors.
    dcx: DiagCtxHandle,

    /// Defines the type map to register types to.
    map: Map,

    /// List of all defined items in the current file.
    defined: HashMap<DefinedItem, NodeId>,

    /// Defines all the local symbols within the current scope.
    locals: SymbolTable<String, lume_hir::VariableSource>,

    /// Mapping between all imported items and their corresponding item IDs.
    imports: HashMap<String, Path>,

    /// Defines the currently containing namespace expressions exist within, if
    /// any.
    namespace: Option<Path>,

    /// Defines the currently containing class expressions exist within, if any.
    self_type: Option<Path>,

    /// Defines the currently visible type parameters.
    type_parameters: Vec<HashMap<String, lume_hir::TypeId>>,

    /// Defines the ID of the current item being lowered, if any.
    current_node: NodeId,
}

impl LowerModule {
    /// Creates a new lowerer for creating HIR maps from AST.
    pub fn new(package_id: PackageId, dcx: DiagCtxHandle) -> LowerModule {
        let map = Map::empty(package_id);
        let current_node = NodeId::empty(package_id);

        LowerModule {
            file: Arc::default(),
            map,
            dcx,
            defined: HashMap::new(),
            locals: SymbolTable::new(),
            imports: HashMap::new(),
            namespace: None,
            self_type: None,
            type_parameters: Vec::new(),
            current_node,
        }
    }

    /// Resets the lowerer to its initial state.
    pub fn reset(&mut self) {
        self.locals.clear();
        self.imports.clear();
        self.namespace = None;
        self.self_type = None;
        self.type_parameters.clear();
    }

    /// Lowers the single given source module into HIR.
    ///
    /// # Errors
    ///
    /// Returns `Err` if any AST nodes are invalid or exist in invalid
    /// locations.
    #[libftrace::traced(level = Info, fields(file = file.name))]
    pub fn lower(&mut self, file: Arc<SourceFile>, expressions: Vec<lume_ast::Item>) -> Result<()> {
        self.file = file;
        self.insert_implicit_imports()?;
        self.lower_items(expressions)?;

        for import in self.imports.values() {
            self.map.imports.insert(import.clone());
        }

        for (id, node) in &self.map.nodes {
            debug_assert_eq!(
                *id,
                node.id(),
                "mismatched between ID key and value: {id:?} != {:?}",
                node.id()
            );
        }

        self.dcx.ensure_untainted()?;
        self.reset();

        Ok(())
    }

    /// Adds implicit imports to the module.
    #[libftrace::traced(level = Trace)]
    fn insert_implicit_imports(&mut self) -> Result<()> {
        let import_item = lume_ast::Import::std(DEFAULT_STD_IMPORTS);

        self.import(import_item)
    }

    /// Gets the next [`NodeId`] in the sequence.
    #[libftrace::traced(level = Trace)]
    fn next_node_id(&mut self) -> NodeId {
        self.current_node = self.current_node.next();

        self.current_node
    }

    /// Ensure that the item with the given name is undefined within the file.
    ///
    /// If the item is not defined, it is added into the list of defined items.
    /// If the item is defined, raises an error to reflect it.
    #[libftrace::traced(level = Trace)]
    fn ensure_item_undefined(&mut self, id: NodeId, item: DefinedItem) -> Result<()> {
        if let Some((existing, _)) = self.defined.get_key_value(&item) {
            return Err(crate::errors::DuplicateDefinition {
                duplicate_range: lume_span::source::Location {
                    file: self.file.clone(),
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

    /// Gets the [`lume_hir::TypeId`] if the currently visible type parameter
    /// with the name of `name`, if any.
    fn id_of_type_param(&self, name: &str) -> Option<lume_hir::TypeId> {
        for layer in self.type_parameters.iter().rev() {
            if let Some(id) = layer.get(name) {
                return Some(*id);
            }
        }

        None
    }

    /// Adds a new type parameter scope, which can be popped later.
    fn add_type_param_scope(&mut self) {
        self.type_parameters.push(HashMap::new());
    }

    /// Adds a new type parameter, which is currently visible.
    fn add_type_param(&mut self, type_param: lume_hir::TypeParameter) -> NodeId {
        let id = type_param.id;

        self.type_parameters
            .last_mut()
            .unwrap()
            .insert(type_param.name.name.clone(), TypeId::from(id));

        self.map.nodes.insert(
            type_param.id,
            lume_hir::Node::Type(lume_hir::TypeDefinition::TypeParameter(Box::new(type_param))),
        );

        id
    }

    /// Gets the [`lume_hir::Path`] for the item with the given name.
    fn resolve_symbol_name(&mut self, path: &lume_ast::Path) -> Result<Path> {
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

        root.extend(self.path_root(path.root.clone())?);

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
            source: self.file.clone(),
            range: self_segment.location().0.clone(),
            ty: String::from("Self"),
        }
        .into())
    }

    /// Attemps to resolve a [`lume_hir::Path`] for an imported symbol.
    fn resolve_imported_symbol(&mut self, path: &lume_ast::Path) -> Result<Option<Path>> {
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

    /// Adds the given node `![lang_item(name = ...)]` to the `lang_items` map,
    /// if present in the given slice.
    fn add_lang_items(&mut self, node: NodeId, attrs: &[lume_ast::Attribute]) -> Result<()> {
        for attr in attrs {
            if attr.name.as_str() != "lang_item" {
                continue;
            }

            let Some(name_arg) = attr.arguments.iter().find(|arg| arg.key.as_str() == "name") else {
                return Err(errors::LangItemMissingName {
                    source: self.file.clone(),
                    range: attr.location.0.clone(),
                }
                .into());
            };

            let lume_ast::Literal::String(lang_name) = &name_arg.value else {
                return Err(errors::LangItemInvalidNameType {
                    source: self.file.clone(),
                    range: attr.location.0.clone(),
                }
                .into());
            };

            self.map.lang_items.add_name(lang_name.value.as_str(), node)?;
        }

        Ok(())
    }

    fn location(&self, expr: lume_ast::Location) -> Location {
        lume_span::source::Location {
            file: self.file.clone(),
            index: expr.0,
        }
        .intern()
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
        Cow::Borrowed(&self.name.name)
    }
}

impl Unique for lume_hir::TraitMethodDefinition {
    fn name(&self) -> Cow<'_, str> {
        Cow::Borrowed(&self.name.name)
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

impl LowerModule {
    /// Ensures that the given series of items are unique.
    ///
    /// If a duplicate is found, the provided closure is called with the
    /// duplicate item and the existing item. The closure should return an
    /// error, which is reported to the current diagnostic context.
    pub(crate) fn ensure_unique_series<'a, T, F>(&self, items: &'a [T], on_duplicate: F) -> Result<()>
    where
        T: Unique,
        F: Fn(&T, &T) -> Error,
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
