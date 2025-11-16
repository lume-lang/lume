use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::sync::Arc;

use error_snippet::Result;
use lume_errors::DiagCtxHandle;
use lume_hir::map::Map;
use lume_hir::symbols::*;
use lume_hir::{Identifier, Path, PathSegment};
use lume_lexer::Lexer;
use lume_parser::Parser;
use lume_session::Package;
use lume_span::{Internable, Location, NodeId, SourceFile, SourceMap};

mod errors;

mod def;
mod expr;
mod generic;
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

#[macro_export]
macro_rules! err {
    (
        $self:expr,
        $location:expr,
        $kind:ident $(,)?
        $(
            $field: ident,
            $value: expr
        ),*
    ) => {
        $kind {
            source: $self.file.clone(),
            range: $location.index.clone(),
            $( $field: $value ),*
        }
        .into()
    };
}

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
    #[tracing::instrument(name = "lume_hir_lower::lower_state::lower", level = "DEBUG", skip_all, err)]
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
    #[tracing::instrument(
        parent = None,
        name = "lume_hir_lower::lower_state::lower_into",
        level = "INFO",
        skip_all,
        fields(package = self.package.name),
        err
    )]
    pub fn lower_into(&mut self) -> Result<Map> {
        // Create a new HIR map for the module.
        let mut lume_hir = Map::empty(self.package.id);
        let mut item_idx = NodeId::empty(self.package.id);

        let use_std = !self.package.dependencies.no_std;

        for (_, source_file) in self.package.files.clone() {
            // Register source file in the state.
            self.source_map.insert(source_file.clone());

            // Parse the contents of the source file.
            let expressions = self.dcx.with(|handle| {
                let mut lexer = Lexer::new(source_file.clone());
                let tokens = lexer.lex()?;

                let mut parser = Parser::new(source_file.clone(), tokens, handle)?;
                parser.parse()
            })?;

            // Lowers the parsed module expressions down to HIR.
            self.dcx.with(|handle| {
                LowerModule::lower(&mut lume_hir, &mut item_idx, source_file, handle, expressions, use_std)
            })?;
        }

        Ok(lume_hir)
    }
}

#[derive(Hash, Debug, PartialEq, Eq)]
enum DefinedItem {
    Function(Path),
    Type(Path),
}

impl DefinedItem {
    pub fn path(&self) -> &Path {
        match self {
            Self::Function(path) | Self::Type(path) => path,
        }
    }

    pub fn location(&self) -> &Range<usize> {
        &self.path().location.index
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
            current_node: item_idx,
        }
    }

    /// Lowers the single given source module into HIR.
    ///
    /// # Errors
    ///
    /// Returns `Err` if any AST nodes are invalid or exist in invalid
    /// locations.
    #[tracing::instrument(
        parent = None,
        name = "lume_hir_lower::lower_module::lower",
        level = "INFO",
        skip_all,
        fields(file = %file.name),
        err
    )]
    pub fn lower(
        map: &'a mut Map,
        node_idx: &'a mut NodeId,
        file: Arc<SourceFile>,
        dcx: DiagCtxHandle,
        expressions: Vec<lume_ast::TopLevelExpression>,
        import_std: bool,
    ) -> Result<()> {
        let mut lower = LowerModule::new(map, *node_idx, file, dcx);

        if import_std {
            lower.insert_implicit_imports()?;
        }

        for expr in expressions {
            if let Err(err) = lower.top_level_expression(expr) {
                lower.dcx.emit(err);
            }
        }

        for import in lower.imports.values() {
            lower.map.imports.insert(import.clone());
        }

        lower.dcx.ensure_untainted()?;

        *node_idx = lower.current_node;

        Ok(())
    }

    /// Adds implicit imports to the module.
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn insert_implicit_imports(&mut self) -> Result<()> {
        let import_item = lume_ast::Import::std(DEFAULT_STD_IMPORTS);

        self.top_import(import_item)
    }

    /// Gets the next [`NodeId`] in the sequence.
    #[tracing::instrument(level = "TRACE", skip(self), ret)]
    fn next_node_id(&mut self) -> NodeId {
        self.current_node = self.current_node.next();

        self.current_node
    }

    /// Ensure that the item with the given name is undefined within the file.
    ///
    /// If the item is not defined, it is added into the list of defined items.
    /// If the item is defined, raises an error to reflect it.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn ensure_item_undefined(&mut self, item: DefinedItem) -> Result<()> {
        if let Some(existing) = self.defined.get(&item) {
            return Err(crate::errors::DuplicateDefinition {
                source: self.file.clone(),
                duplicate_range: item.location().clone(),
                original_range: existing.location().clone(),
                name: item.path().to_string(),
            }
            .into());
        }

        self.defined.insert(item);

        Ok(())
    }

    /// Gets the [`lume_hir::Path`] for the item with the given name.
    fn resolve_symbol_name(&self, path: &lume_ast::Path) -> Result<Path> {
        if let Some(symbol) = self.resolve_imported_symbol(path)? {
            return Ok(symbol.clone());
        }

        let root = if let Some(namespace) = &self.namespace
            && path.root.is_empty()
        {
            namespace.clone().as_root()
        } else {
            self.path_root(path.root.clone())?
        };

        // Since all names hash to the same value, we can compute what the item ID
        // would be, if the symbol is registered within the module.
        Ok(Path {
            root,
            name: self.path_segment(path.name.clone())?,
            location: self.location(path.location.clone()),
        })
    }

    /// Attemps to resolve a [`lume_hir::Path`] for an imported symbol.
    fn resolve_imported_symbol(&self, path: &lume_ast::Path) -> Result<Option<Path>> {
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

    fn expand_name(&self, name: lume_ast::PathSegment) -> Result<Path> {
        if let Some(ns) = &self.namespace {
            Ok(Path::with_root(ns.clone(), self.path_segment(name)?))
        } else {
            Ok(Path::rooted(self.path_segment(name)?))
        }
    }

    fn path_root(&self, expr: Vec<lume_ast::PathSegment>) -> Result<Vec<PathSegment>> {
        expr.into_iter()
            .map(|seg| self.path_segment(seg))
            .collect::<Result<Vec<_>>>()
    }

    fn path_segment(&self, expr: lume_ast::PathSegment) -> Result<PathSegment> {
        match expr {
            lume_ast::PathSegment::Namespace { name } => Ok(PathSegment::namespace(self.identifier(name))),
            lume_ast::PathSegment::Type {
                name,
                type_arguments,
                location,
            } => {
                let name = self.identifier(name);
                let location = self.location(location);
                let type_arguments = type_arguments
                    .into_iter()
                    .map(|arg| self.type_ref(arg))
                    .collect::<Result<Vec<_>>>()?;

                Ok(PathSegment::Type {
                    name,
                    type_arguments,
                    location,
                })
            }
            lume_ast::PathSegment::Callable {
                name,
                type_arguments,
                location,
            } => {
                let name = self.identifier(name);
                let location = self.location(location);
                let type_arguments = type_arguments
                    .into_iter()
                    .map(|arg| self.type_ref(arg))
                    .collect::<Result<Vec<_>>>()?;

                Ok(PathSegment::Callable {
                    name,
                    type_arguments,
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

    #[tracing::instrument(level = "DEBUG", skip_all, ret, err)]
    fn top_namespace(&mut self, expr: lume_ast::Namespace) -> Result<()> {
        self.namespace = Some(self.import_path(expr.path)?);

        Ok(())
    }

    fn top_level_expression(&mut self, expr: lume_ast::TopLevelExpression) -> Result<()> {
        let hir_ast = match expr {
            lume_ast::TopLevelExpression::Import(i) => {
                self.top_import(*i)?;

                return Ok(());
            }
            lume_ast::TopLevelExpression::Namespace(i) => {
                self.top_namespace(*i)?;

                return Ok(());
            }
            lume_ast::TopLevelExpression::TypeDefinition(t) => self.def_type(*t)?,
            lume_ast::TopLevelExpression::FunctionDefinition(f) => self.def_function(*f)?,
            lume_ast::TopLevelExpression::TraitImpl(f) => self.def_trait_impl(*f)?,
            lume_ast::TopLevelExpression::Impl(f) => self.def_impl(*f)?,
        };

        let id = hir_ast.id();

        // Ensure that the ID doesn't overwrite an existing entry.
        debug_assert!(!self.map.nodes.contains_key(&id));

        self.map.nodes.insert(id, hir_ast);

        Ok(())
    }

    #[tracing::instrument(level = "DEBUG", skip_all, ret, err)]
    fn top_import(&mut self, expr: lume_ast::Import) -> Result<()> {
        for imported_name in expr.names {
            let namespace = self.import_path(expr.path.clone())?;

            let import_path_name = if imported_name.is_lower() {
                PathSegment::callable(self.identifier(imported_name))
            } else {
                PathSegment::ty(self.identifier(imported_name))
            };

            let imported_path = Path::with_root(namespace, import_path_name);

            self.imports.insert(imported_path.name.to_string(), imported_path);
        }

        Ok(())
    }
}
