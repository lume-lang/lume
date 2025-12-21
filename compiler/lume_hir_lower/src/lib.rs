use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use error_snippet::Result;
use lume_errors::DiagCtxHandle;
use lume_hir::map::Map;
use lume_hir::symbols::*;
use lume_hir::{Identifier, Path, PathSegment, TypeId};
use lume_lexer::Lexer;
use lume_parser::Parser;
use lume_session::Package;
use lume_span::{Internable, Location, NodeId, SourceFile, SourceMap};

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

    /// Lowers the given project and state into a HIR map.
    ///
    /// # Errors
    ///
    /// Returns `Err` if any AST nodes are invalid or exist in invalid
    /// locations.
    #[libftrace::traced(level = Debug)]
    pub fn lower(package: &'a Package, source_map: &'a mut SourceMap, dcx: DiagCtxHandle) -> Result<Map> {
        let mut lower = LowerState::new(package, source_map, dcx);

        lower.lower_into()
    }

    /// Lowers the current project and state into a HIR map.
    ///
    /// # Errors
    ///
    /// Returns `Err` if any AST nodes are invalid or exist in invalid
    /// locations.
    #[libftrace::traced(level = Info, fields(package = self.package.name))]
    pub fn lower_into(&mut self) -> Result<Map> {
        // Create a new HIR map for the module.
        let mut lume_hir = Map::empty(self.package.id);
        let mut item_idx = NodeId::empty(self.package.id);
        let mut defined_items = HashSet::<DefinedItem>::new();

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
            self.dcx.with(|handle| {
                LowerModule::lower(
                    &mut lume_hir,
                    &mut item_idx,
                    &mut defined_items,
                    source_file,
                    handle,
                    expressions,
                )
            })?;
        }

        Ok(lume_hir)
    }
}

#[derive(Hash, Debug, PartialEq, Eq)]
pub enum DefinedItem {
    Function(Path),
    Type(Path),
}

impl DefinedItem {
    pub fn path(&self) -> &Path {
        match self {
            Self::Function(path) | Self::Type(path) => path,
        }
    }

    pub fn location(&self) -> Location {
        self.path().location
    }
}

pub struct LowerModule<'a> {
    /// Defines the file is being lowered.
    file: Arc<SourceFile>,

    /// Handle to the diagnostics context which will handle parsing errors.
    dcx: DiagCtxHandle,

    /// Defines the type map to register types to.
    map: &'a mut Map,

    /// List of all defined items in the current file.
    defined: HashSet<DefinedItem>,

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

impl<'a> LowerModule<'a> {
    /// Creates a new lowerer for creating HIR maps from AST.
    pub fn new(map: &'a mut Map, item_idx: NodeId, file: Arc<SourceFile>, dcx: DiagCtxHandle) -> LowerModule<'a> {
        LowerModule {
            file,
            map,
            dcx,
            defined: HashSet::new(),
            locals: SymbolTable::new(),
            imports: HashMap::new(),
            namespace: None,
            self_type: None,
            type_parameters: Vec::new(),
            current_node: item_idx,
        }
    }

    /// Lowers the single given source module into HIR.
    ///
    /// # Errors
    ///
    /// Returns `Err` if any AST nodes are invalid or exist in invalid
    /// locations.
    #[libftrace::traced(level = Info, fields(file = file.name))]
    pub fn lower(
        map: &'a mut Map,
        node_idx: &'a mut NodeId,
        defined: &mut HashSet<DefinedItem>,
        file: Arc<SourceFile>,
        dcx: DiagCtxHandle,
        expressions: Vec<lume_ast::TopLevelExpression>,
    ) -> Result<()> {
        let mut lower = LowerModule::new(map, *node_idx, file, dcx);
        std::mem::swap(&mut lower.defined, defined);

        lower.insert_implicit_imports()?;

        for expr in expressions {
            if let Err(err) = lower.top_level_expression(expr) {
                lower.dcx.emit(err);
            }
        }

        for import in lower.imports.values() {
            lower.map.imports.insert(import.clone());
        }

        #[cfg(debug_assertions)]
        for (id, node) in &lower.map.nodes {
            assert_eq!(
                *id,
                node.id(),
                "mismatched between ID key and value: {id:?} != {:?}",
                node.id()
            );
        }

        lower.dcx.ensure_untainted()?;

        *node_idx = lower.current_node;
        *defined = std::mem::take(&mut lower.defined);

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
    fn ensure_item_undefined(&mut self, item: DefinedItem) -> Result<()> {
        if let Some(existing) = self.defined.get(&item) {
            return Err(crate::errors::DuplicateDefinition {
                duplicate_range: lume_span::source::Location {
                    file: self.file.clone(),
                    index: item.location().index.clone(),
                },
                original_range: existing.location().clone_inner(),
                name: item.path().to_string(),
            }
            .into());
        }

        self.defined.insert(item);

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

            self.map.lang_items.insert(lang_name.value.clone(), node);
        }

        Ok(())
    }

    fn identifier(&self, expr: lume_ast::Identifier) -> Identifier {
        let location = self.location(expr.location.clone());

        Identifier {
            name: expr.name,
            location,
        }
    }

    fn expand_name(&mut self, name: lume_ast::PathSegment) -> Result<Path> {
        if let Some(ns) = &self.namespace {
            Ok(Path::with_root(ns.clone(), self.path_segment(name)?))
        } else {
            Ok(Path::rooted(self.path_segment(name)?))
        }
    }

    fn path_root(&mut self, expr: Vec<lume_ast::PathSegment>) -> Result<Vec<PathSegment>> {
        expr.into_iter()
            .map(|seg| self.path_segment(seg))
            .collect::<Result<Vec<_>>>()
    }

    fn path_segment(&mut self, expr: lume_ast::PathSegment) -> Result<PathSegment> {
        match expr {
            lume_ast::PathSegment::Namespace { name } => Ok(PathSegment::namespace(self.identifier(name))),
            lume_ast::PathSegment::Type {
                name,
                bound_types,
                location,
            } => {
                let name = self.identifier(name);
                let location = self.location(location);
                let bound_types = bound_types
                    .into_iter()
                    .map(|arg| self.type_ref(arg))
                    .collect::<Result<Vec<_>>>()?;

                Ok(PathSegment::Type {
                    name,
                    bound_types,
                    location,
                })
            }
            lume_ast::PathSegment::Callable {
                name,
                bound_types,
                location,
            } => {
                let name = self.identifier(name);
                let location = self.location(location);
                let bound_types = bound_types
                    .into_iter()
                    .map(|arg| self.type_ref(arg))
                    .collect::<Result<Vec<_>>>()?;

                Ok(PathSegment::Callable {
                    name,
                    bound_types,
                    location,
                })
            }
            lume_ast::PathSegment::Variant { name, location } => {
                let name = self.identifier(name);
                let location = self.location(location);

                Ok(PathSegment::Variant { name, location })
            }
        }
    }

    fn location(&self, expr: lume_ast::Location) -> Location {
        lume_span::source::Location {
            file: self.file.clone(),
            index: expr.0,
        }
        .intern()
    }
}
