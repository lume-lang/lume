pub(crate) mod attr;
pub(crate) mod desugar;
pub(crate) mod errors;
pub(crate) mod expr;
pub(crate) mod generics;
pub(crate) mod item;
pub(crate) mod lit;
pub(crate) mod make;
pub(crate) mod path;
pub(crate) mod pattern;
pub(crate) mod stmt;
pub(crate) mod ty;

use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::{Arc, LazyLock};

pub use lume_ast;
use lume_ast::AstNode;
use lume_errors::{DiagCtxHandle, Result};
pub use lume_hir::WithLocation as _;
use lume_hir::symbols::SymbolTable;
use lume_hir::{Map, Path, PathSegment, Place};
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
        let mut lexer = lume_lexer::Lexer::new(source_file.clone());
        let tokens = match lexer.lex() {
            Ok(tokens) => tokens,
            Err(err) => {
                ctx.dcx.emit_and_push(err);
                continue;
            }
        };

        let parser = lume_parser::Parser::from_tokens(source_file.clone(), tokens.into_iter());
        let syntax_tree = parser.parse(lume_parser::Target::Item);

        for error in syntax_tree.errors() {
            ctx.dcx.emit_and_push(
                lume_errors::SimpleDiagnostic::new(error.message())
                    .with_label(lume_errors::Label::error(
                        Some(source_file.clone()),
                        error.span().0..error.span().1,
                        "error occurred here",
                    ))
                    .into(),
            );
        }

        let source_node = lume_ast::SourceFile::cast(syntax_tree.syntax()).unwrap();

        if let Err(err) = ctx.lower_items(source_file.id, source_node.item()) {
            ctx.dcx.emit_and_push(err);
        }
    }

    Ok(ctx.map)
}

#[derive(Hash, Debug, PartialEq, Eq)]
pub enum DefinedItem {
    Function(Path),
    Method(Path),
    Type(Path),
}

impl DefinedItem {
    pub fn path(&self) -> Path {
        match self {
            Self::Function(path) | Self::Method(path) | Self::Type(path) => path.clone(),
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
    pub(crate) fn ident(&self, expr: lume_ast::Name) -> lume_hir::Identifier {
        let location = self.location(expr.location());

        lume_hir::Identifier {
            name: expr.as_text(),
            location,
        }
    }

    /// Lowers the given AST identifier into a [`lume_hir::Identifier`].
    pub(crate) fn ident_opt(&self, name: Option<lume_ast::Name>) -> lume_hir::Identifier {
        let location = self.location(name.as_ref().map_or(lume_ast::Location(0..0), |n| n.location()));

        lume_hir::Identifier {
            name: name.map_or(String::from("[missing-name]"), |n| n.as_text()),
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

    pub(crate) fn existing_type_id<N: AsRef<str>>(&self, name: N) -> Option<lume_hir::TypeId> {
        self.current_type_params.retrieve(name.as_ref()).copied()
    }

    pub(crate) fn existing_type_id_or_new<N: AsRef<str>>(&mut self, name: N) -> lume_hir::TypeId {
        self.existing_type_id(name)
            .unwrap_or_else(|| lume_hir::TypeId::from(self.next_node_id()))
    }

    /// Ensure that the item with the given name is undefined within the file.
    ///
    /// If the item is not defined, it is added into the list of defined items.
    /// If the item is defined, raises an error to reflect it.
    #[tracing::instrument(level = "TRACE", skip_all)]
    fn ensure_item_undefined(&mut self, id: NodeId, item: DefinedItem) {
        if let Some((existing, _)) = self.defined.get_key_value(&item) {
            self.dcx.emit_and_push(
                crate::errors::DuplicateDefinition {
                    duplicate_range: lume_span::source::Location {
                        file: self.current_file().clone(),
                        index: item.location().index.clone(),
                    }
                    .intern(),
                    original_range: existing.location(),
                    name: item.path().to_string(),
                }
                .into(),
            );

            return;
        }

        self.defined.insert(item, id);
    }

    pub(crate) fn lower_items<I>(&mut self, source_file_id: SourceFileId, items: I) -> Result<()>
    where
        I: IntoIterator<Item = lume_ast::Item>,
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

pub(crate) trait Sentinal {
    fn missing() -> Self;
}

impl Sentinal for lume_hir::Identifier {
    fn missing() -> Self {
        Self {
            name: String::from("[missing name]"),
            location: Location::empty(),
        }
    }
}

impl Sentinal for lume_hir::PathSegment {
    fn missing() -> Self {
        Self::Missing
    }
}

impl Sentinal for lume_hir::Path {
    fn missing() -> Self {
        Self {
            root: Vec::new(),
            name: PathSegment::Missing,
            location: Location::empty(),
        }
    }
}

impl Sentinal for lume_hir::Block {
    fn missing() -> Self {
        Self {
            id: NodeId::default(),
            statements: Vec::new(),
            location: Location::empty(),
        }
    }
}

impl Sentinal for lume_hir::LiteralPattern {
    fn missing() -> Self {
        Self {
            literal: lume_hir::Literal::missing(),
            location: Location::empty(),
        }
    }
}

impl Sentinal for lume_hir::Literal {
    fn missing() -> Self {
        Self {
            id: NodeId::default(),
            kind: lume_hir::LiteralKind::Boolean(lume_hir::BooleanLiteral {
                id: NodeId::default(),
                value: false,
            }),
            location: Location::empty(),
        }
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
        Cow::Borrowed(self.signature.name.name().as_str())
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
    pub(crate) fn ensure_unique_series<'a, T, F>(&self, items: &'a [T], on_duplicate: F)
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
    }
}
