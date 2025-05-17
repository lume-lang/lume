use std::collections::HashMap;
use std::sync::Arc;

use arc::Project;
use error_snippet::Result;
use lume_ast::{self as ast};
use lume_errors::DiagCtxHandle;
use lume_hir::map::Map;
use lume_hir::stdlib::Assets;
use lume_hir::symbols::*;
use lume_parser::Parser;
use lume_span::{Location, SourceFile, hash_id};
use lume_types::{Identifier, SymbolName};

mod errors;

mod def;
mod expr;
mod generic;
mod lit;
mod stmt;
mod ty;

#[cfg(test)]
mod tests;

const ARRAY_STD_TYPE: &str = "Array";
const ARRAY_WITH_CAPACITY_FUNC: &str = "with_capacity";

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
            range: $location.index,
            $( $field: $value ),*
        }
        .into()
    };
}

pub struct LowerState<'a> {
    /// Defines the project of the current package project.
    project: &'a Project,

    /// Defines the state being lowered.
    state: &'a mut lume_state::State,
}

impl<'a> LowerState<'a> {
    /// Creates a new [`LowerState`] instance.
    pub fn new(project: &'a Project, state: &'a mut lume_state::State) -> Self {
        Self { project, state }
    }

    /// Lowers the given project and state into a HIR map.
    ///
    /// # Errors
    ///
    /// Returns `Err` if any AST nodes are invalid or exist in invalid locations.
    pub fn lower(project: &'a Project, state: &'a mut lume_state::State) -> Result<Map> {
        let mut lower = LowerState::new(project, state);

        lower.lower_into()
    }

    /// Lowers the current project and state into a HIR map.
    ///
    /// # Errors
    ///
    /// Returns `Err` if any AST nodes are invalid or exist in invalid locations.
    pub fn lower_into(&mut self) -> Result<Map> {
        // Create a new HIR map for the module.
        let mut lume_hir = Map::empty(self.project.id);

        // Read all the sources files before parsing, so if any of them
        // are inaccessible, we don't waste effort parsing them.
        let source_files = self.source_files()?;

        for source_file in source_files {
            // Register source file in the state.
            self.state.source_map.insert(source_file.clone());

            // Parse the contents of the source file.
            let expressions = Parser::parse_src(self.state, source_file.id)?;

            // Lowers the parsed module expressions down to HIR.
            self.state
                .dcx_mut()
                .with(|handle| LowerModule::lower(&mut lume_hir, source_file, handle, expressions))?;
        }

        Ok(lume_hir)
    }

    /// Gets all the source files to include in the compilation.
    fn source_files(&self) -> Result<Vec<Arc<SourceFile>>> {
        let package_id = self.project.id;

        // Add all the source files within the standard library.
        let mut sources_files = Assets::as_sources(package_id);

        // As well as all the files within the project itself
        let project_file_names = self.project.files()?;

        sources_files.extend(
            project_file_names
                .into_iter()
                .map(|path| {
                    // We get the relative path of the file within the project,
                    // so error messages don't use the full path to a file.
                    let relative_path = self.project.relative_source_path(&path).to_string_lossy().to_string();

                    let content = std::fs::read_to_string(path)?;

                    Ok(Arc::new(SourceFile::new(package_id, relative_path, content)))
                })
                .collect::<Result<Vec<_>>>()?,
        );

        Ok(sources_files)
    }
}

pub struct LowerModule<'a> {
    /// Defines the file is being lowered.
    file: Arc<SourceFile>,

    /// Handle to the diagnostics context which will handle parsing errors.
    dcx: DiagCtxHandle,

    /// Defines the type map to register types to.
    map: &'a mut Map,

    /// Defines all the local symbols within the current scope.
    locals: SymbolTable,

    /// Mapping between all imported items and their corresponding item IDs.
    imports: HashMap<String, SymbolName>,

    /// Defines the currently containing namespace expressions exist within, if any.
    namespace: lume_types::NamespacePath,

    /// Defines the currently containing class expressions exist within, if any.
    self_type: Option<SymbolName>,

    /// Defines the current counter for [`LocalId`] instances, so they can stay unique.
    local_id_counter: u64,

    /// Defines the current counter for [`ItemId`] instances, which refer to implementation blocks.
    impl_id_counter: u64,
}

impl<'a> LowerModule<'a> {
    /// Creates a new lowerer for creating HIR maps from AST.
    pub fn new(map: &mut Map, file: Arc<SourceFile>, dcx: DiagCtxHandle) -> LowerModule {
        LowerModule {
            file,
            map,
            dcx,
            locals: SymbolTable::new(),
            imports: HashMap::new(),
            namespace: lume_types::NamespacePath::empty(),
            self_type: None,
            local_id_counter: 0,
            impl_id_counter: 0,
        }
    }

    /// Lowers the single given source module into HIR.
    ///
    /// # Errors
    ///
    /// Returns `Err` if any AST nodes are invalid or exist in invalid locations.
    pub fn lower(
        map: &'a mut Map,
        file: Arc<SourceFile>,
        dcx: DiagCtxHandle,
        expressions: Vec<ast::TopLevelExpression>,
    ) -> Result<()> {
        let mut lower = LowerModule::new(map, file, dcx);
        lower.insert_implicit_imports();

        for expr in expressions {
            lower.top_level_expression(expr)?;
        }

        Ok(())
    }

    /// Adds implicit imports to the module.
    fn insert_implicit_imports(&mut self) {
        let import_item = ast::Import::std(DEFAULT_STD_IMPORTS);

        self.top_import(import_item);
    }

    /// Converts the given value into an [`lume_hir::ItemId`].
    fn item_id<T: std::hash::Hash + Sized>(&self, value: T) -> lume_hir::ItemId {
        let id = hash_id(&value);

        lume_hir::ItemId(self.map.package, id)
    }

    /// Converts the given value into an [`lume_hir::ItemId`].
    ///
    /// Since implementations often use the parent type as the hash value,
    /// there *will* be key collisions where an implementation will get the same [`lume_hir::ItemId`]
    /// as the parent type, which would override the original type. So, the given Lume file:
    ///
    /// ```lm
    /// // uses `Foo` as it's hash value
    /// struct Foo {}
    ///
    /// // also uses `Foo` as it's hash value (!!!)
    /// impl Foo {}
    /// ```
    fn impl_id<T: std::hash::Hash + Sized>(&mut self, value: T) -> lume_hir::ItemId {
        let id = hash_id(&hash_id(&value).wrapping_add(self.impl_id_counter));

        self.impl_id_counter += 1;

        lume_hir::ItemId(self.map.package, id)
    }

    /// Generates the next [`lume_hir::NodeId`] instance in the chain.
    ///
    /// Local IDs are simply incremented over the last used ID, starting from 0.
    fn next_expr_id(&mut self) -> lume_hir::ExpressionId {
        let id = self.local_id_counter;
        self.local_id_counter += 1;

        lume_hir::ExpressionId(self.map.package, lume_hir::LocalId(id))
    }

    /// Generates the next [`lume_hir::NodeId`] instance in the chain.
    ///
    /// Local IDs are simply incremented over the last used ID, starting from 0.
    fn next_stmt_id(&mut self) -> lume_hir::StatementId {
        let id = self.local_id_counter;
        self.local_id_counter += 1;

        lume_hir::StatementId(self.map.package, lume_hir::LocalId(id))
    }

    /// Gets the [`lume_hir::SymbolName`] for the item with the given name.
    fn resolve_symbol_name(&self, path: &ast::Path) -> SymbolName {
        if let Some(symbol) = self.resolve_imported_symbol(path) {
            return symbol.clone();
        }

        // Since all names hash to the same value, we can compute what the item ID
        // would be, if the symbol is registered within the module.
        SymbolName {
            namespace: self.identifier_path(path.root.clone()),
            name: self.identifier(path.name.clone()),
            location: self.location(path.location.clone()),
        }
    }

    /// Attemps to resolve a [`lume_hir::SymbolName`] for an imported symbol.
    fn resolve_imported_symbol(&self, path: &ast::Path) -> Option<SymbolName> {
        for (import, symbol) in &self.imports {
            // Match against imported paths, which match the first segment of the imported path.
            //
            // This handles situations where a subpath was imported, which is then referenced later, such as:
            //
            // ```lume
            //     import std (io);
            //
            //     io::File::from_path("foo.txt");
            // ```
            if let Some(name) = path.root.path.first() {
                if name.name == *import {
                    // Since we only matched a subset of the imported symbol,
                    // we need to merge the two paths together, so it forms a fully-qualified path.
                    let mut merged_symbol = SymbolName {
                        namespace: lume_types::NamespacePath::empty(),
                        name: lume_types::Identifier::from(&path.name.name),
                        location: self.location(path.location.clone()),
                    };

                    for segment in &symbol.namespace.path {
                        merged_symbol
                            .namespace
                            .path
                            .push(lume_types::Identifier::from(&segment.name));
                    }

                    for segment in &path.root.path {
                        merged_symbol
                            .namespace
                            .path
                            .push(lume_types::Identifier::from(&segment.name));
                    }

                    return Some(merged_symbol);
                }
            }
            // Match against the the imported symbol directly, such as:
            //
            // ```lume
            //     import std.io (File);
            //
            //     File::from_path("foo.txt");
            // ```
            else if path.name.name == *import {
                return Some(symbol.clone());
            }
        }

        None
    }

    fn symbol_name(&self, name: ast::Identifier) -> SymbolName {
        let location = self.location(name.location.clone());

        SymbolName {
            name: self.identifier(name),
            namespace: self.namespace.clone(),
            location,
        }
    }

    fn identifier(&self, expr: ast::Identifier) -> Identifier {
        let location = self.location(expr.location.clone());

        lume_types::Identifier {
            name: expr.name,
            location,
        }
    }

    fn identifier_path(&self, expr: ast::NamespacePath) -> lume_types::NamespacePath {
        let location = self.location(expr.location.clone());
        let path = expr.path.into_iter().map(|p| self.identifier(p)).collect::<Vec<_>>();

        lume_types::NamespacePath { path, location }
    }

    fn location(&self, expr: ast::Location) -> Location {
        Location {
            file: self.file.clone(),
            index: expr.0,
        }
    }

    fn top_namespace(&mut self, expr: ast::Namespace) {
        self.namespace = self.identifier_path(expr.path);
    }

    fn top_level_expression(&mut self, expr: ast::TopLevelExpression) -> Result<()> {
        let hir_ast = match expr {
            ast::TopLevelExpression::Import(i) => {
                self.top_import(*i);

                return Ok(());
            }
            ast::TopLevelExpression::Namespace(i) => {
                self.top_namespace(*i);

                return Ok(());
            }
            ast::TopLevelExpression::TypeDefinition(t) => self.def_type(*t)?,
            ast::TopLevelExpression::FunctionDefinition(f) => self.def_function(*f)?,
            ast::TopLevelExpression::Use(f) => self.def_use(*f)?,
            ast::TopLevelExpression::Impl(f) => self.def_impl(*f)?,
        };

        let id = hir_ast.id();

        // Ensure that the ID doesn't overwrite an existing entry.
        debug_assert!(!self.map.items.contains_key(&id));

        self.map.items.insert(id, hir_ast.clone());

        Ok(())
    }

    fn top_import(&mut self, expr: ast::Import) {
        for imported_name in expr.names {
            let location = self.location(imported_name.location.clone());

            let imported_symbol_name = SymbolName {
                name: self.identifier(imported_name.clone()),
                namespace: self.identifier_path(expr.path.clone()),
                location,
            };

            self.imports.insert(imported_name.name.clone(), imported_symbol_name);
        }
    }
}
