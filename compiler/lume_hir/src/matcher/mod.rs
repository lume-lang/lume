use std::collections::HashMap;
use std::ops::ControlFlow;

use crate::visitor::traverse_node;
use crate::*;

pub mod expr;
pub use expr::*;

pub mod item;
pub use item::*;

pub mod pat;
pub use pat::*;

pub mod stmt;
pub use stmt::*;

pub mod string;
pub use string::*;

/// Performs a global match against all nodes in the given HIR map.
///
/// Searches the HIR map for nodes which satisfy `matcher` and calls `callback`
/// for each match. After each match, the return value of `callback` defines
/// whether the matcher will continue or stop.
///
/// # Examples
///
/// To match with all function definitions in the entire HIR map:
/// ```
/// use std::ops::ControlFlow;
/// use lume_hir::matcher::*;
///
/// fn all_functions(hir: &lume_hir::Map) -> Vec<lume_span::NodeId> {
///     let mut functions = Vec::new();
///
///     find_matches(
///         hir,
///         &function([]).bind("func"),
///         &mut |result| {
///             functions.push(result.bound_node("func").unwrap().id());
///
///             ControlFlow::Continue(())
///         }
///     );
///
///     functions
/// }
/// ```
pub fn find_matches<'hir>(
    hir: &'hir Map,
    matcher: &dyn Predicate<Subject = Node>,
    callback: &mut dyn FnMut(&Results<'hir>) -> ControlFlow<()>,
) {
    let mut visitor = PredicateVisitor { hir, matcher, callback };
    let _ = traverse(hir, &mut visitor);
}

/// Performs a narrowed match against all nodes in the given HIR map, which are
/// descendants of the given entrypoint node.
///
/// Searches the HIR map for nodes which satisfy `matcher` and calls `callback`
/// for each match. After each match, the return value of `callback` defines
/// whether the matcher will continue or stop.
///
/// # Examples
///
/// To find all methods inside of the given [`Node`] (which we assume is
/// actually a [`Node::Impl`]):
/// ```
/// use std::ops::ControlFlow;
/// use lume_hir::matcher::*;
///
/// fn all_methods_in(hir: &lume_hir::Map, impl_node: &lume_hir::Node) -> Vec<lume_span::NodeId> {
///     let mut methods = Vec::new();
///
///     find_matches_in(
///         hir,
///         &method([]).bind("func"),
///         &mut |result| {
///             methods.push(result.bound_node("func").unwrap().id());
///
///             ControlFlow::Continue(())
///         },
///         impl_node
///     );
///
///     methods
/// }
/// ```
pub fn find_matches_in<'hir>(
    hir: &'hir Map,
    matcher: &dyn Predicate<Subject = Node>,
    callback: &mut dyn FnMut(&Results<'hir>) -> ControlFlow<()>,
    entrypoint: &Node,
) {
    let mut visitor = PredicateVisitor { hir, matcher, callback };
    let _ = traverse_node(hir, &mut visitor, entrypoint);
}

/// Performs a global match against all nodes in the given HIR map.
///
/// Searches the HIR map for nodes which satisfy `matcher` and calls `callback`
/// if a match is found. After the first match is found, the matcher stops and
/// returns.
///
/// This is equivalent to:
/// ```ignore
/// find_matches(hir, matcher, &mut |result| {
///     (callback)(result);
///     ControlFlow::Break(())
/// });
/// ```
pub fn find_first_match<'hir>(
    hir: &'hir Map,
    matcher: &dyn Predicate<Subject = Node>,
    callback: &mut dyn FnMut(&Results<'hir>),
) {
    find_matches(hir, matcher, &mut |result| {
        (callback)(result);
        ControlFlow::Break(())
    });
}

/// Holds the bindings for a given match operation.
///
/// Every time a match is found, all nodes which are bound using
/// [`PredicateExt::bind()`] are added to this mapping, which can then be
/// retrieved using [`Results::get()`] or [`Results::get_node()`].
#[derive(Default)]
pub struct Bindings {
    nodes: HashMap<&'static str, NodeId>,
    types: HashMap<&'static str, TypeId>,
}

impl Bindings {
    /// Inserts a new node binding in the map.
    #[inline]
    pub(crate) fn bind_node(&mut self, key: &'static str, node: NodeId) {
        self.nodes.insert(key, node);
    }

    /// Inserts a new type binding in the map.
    #[inline]
    pub(crate) fn bind_type(&mut self, key: &'static str, node: TypeId) {
        self.types.insert(key, node);
    }
}

/// Contains the results for a given match.
///
/// Every time a match is found, all nodes which are bound using
/// [`PredicateExt::bind()`] are added to this mapping, which can then be
/// retrieved using [`Results::get()`] or [`Results::get_node()`].
pub struct Results<'hir> {
    hir: &'hir Map,
    bindings: Bindings,
}

impl Results<'_> {
    /// Gets the node associated with the given binding, if any. Otherwise, if
    /// no binding was found for the current match, returns [`None`].
    #[inline]
    pub fn bound_node(&self, key: &'static str) -> Option<&'_ Node> {
        self.bindings.nodes.get(&key).map(|id| self.hir.node(*id).unwrap())
    }

    /// Gets the type associated with the given type binding, if any. Otherwise,
    /// if no binding was found for the current match, returns [`None`].
    #[inline]
    pub fn bound_type(&self, key: &'static str) -> Option<&'_ Type> {
        self.bindings.types.get(&key).map(|id| self.hir.type_(*id).unwrap())
    }
}

/// A trait for predicates that can be used to match HIR nodes.
///
/// A predicate is a function that takes a "subject" (denoted by
/// [`Self::Subject`]) and returns whether it satisfies the predicate.
///
/// These predicates can vary in function, from whether a function has a return
/// type, or a block expression has a return value. At the same time, these
/// predicates can be composed into larger predicates. For example, you could
/// find all arithmetic expressions which only use interger literals.
pub trait Predicate {
    /// The subject type that this predicate can match.
    type Subject: ?Sized;

    /// Returns whether the predicate is satisfied with the given subject.
    fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool;
}

impl<T> Predicate for PredicateBox<T> {
    type Subject = T;

    fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
        Predicate::satisfies(Box::as_ref(self), hir, subject, bindings)
    }
}

impl<T> Predicate for [PredicateBox<T>] {
    type Subject = T;

    fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
        self.iter()
            .all(|predicate| Predicate::satisfies(predicate, hir, subject, bindings))
    }
}

impl<T> Predicate for Vec<PredicateBox<T>> {
    type Subject = T;

    fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
        self.iter()
            .all(|predicate| Predicate::satisfies(predicate, hir, subject, bindings))
    }
}

pub type PredicateBox<T> = Box<dyn Predicate<Subject = T>>;

/// When any subjects match the inner predicate, the name is bound to the
/// subject's node ID.
pub struct Bind<P> {
    predicate: P,
    name: &'static str,
}

impl<P, S> Predicate for Bind<P>
where
    P: Predicate<Subject = S>,
    S: HasNodeId,
{
    type Subject = S;

    fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
        if Predicate::satisfies(&self.predicate, hir, subject, bindings) {
            bindings.bind_node(self.name, subject.node_id());

            true
        } else {
            false
        }
    }
}

pub trait PredicateBindExt<S> {
    /// Bind the node to a name, so that it can be retrieved. When a bound node
    /// is matched, it will appear in the [`Results`] structure which is passed
    /// to the matching closure.
    ///
    /// This is especially useful for larger predicates, where there might be
    /// multiple nodes to read from.
    fn bind(self, name: &'static str) -> Box<dyn Predicate<Subject = S>>;
}

impl<S> PredicateBindExt<S> for PredicateBox<S>
where
    S: HasNodeId + 'static,
{
    fn bind(self, name: &'static str) -> Box<dyn Predicate<Subject = S>> {
        Box::new(Bind { predicate: self, name })
    }
}

/// When any subjects match the inner predicate, the name is bound to the
/// subject's type ID.
pub struct BindType<P> {
    predicate: P,
    name: &'static str,
}

impl<P> Predicate for BindType<P>
where
    P: Predicate<Subject = Type>,
{
    type Subject = Type;

    fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
        if Predicate::satisfies(&self.predicate, hir, subject, bindings) {
            bindings.bind_type(self.name, subject.id);

            true
        } else {
            false
        }
    }
}

pub trait PredicateBindTypeExt<S> {
    /// Bind the type to a name, so that it can be retrieved. When a bound type
    /// is matched, it will appear in the [`Results`] structure which is passed
    /// to the matching closure.
    ///
    /// This is especially useful for larger predicates, where there might be
    /// multiple type to read from.
    fn bind_type(self, name: &'static str) -> Box<dyn Predicate<Subject = S>>;
}

impl PredicateBindTypeExt<Type> for PredicateBox<Type> {
    fn bind_type(self, name: &'static str) -> Box<dyn Predicate<Subject = Type>> {
        Box::new(BindType { predicate: self, name })
    }
}

struct PredicateVisitor<'a, 'hir> {
    hir: &'hir Map,
    matcher: &'a dyn Predicate<Subject = Node>,
    callback: &'a mut dyn FnMut(&Results<'hir>) -> ControlFlow<()>,
}

impl Visitor for PredicateVisitor<'_, '_> {
    type Break = ();

    fn visit_node(&mut self, node: &Node) -> std::ops::ControlFlow<Self::Break> {
        let mut bindings = Bindings::default();

        if self.matcher.satisfies(self.hir, node, &mut bindings) {
            (self.callback)(&Results {
                hir: self.hir,
                bindings,
            })?;
        }

        ControlFlow::Continue(())
    }
}

#[macro_export]
macro_rules! lumec_matchers {
    (
        $(
            $(#[$attr:meta])*
            matcher $matcher_name:ident(
                $predicate_name:ident
            ) -> bool {
                type Parent = $parent_type:ty;
                type Subject = $subject_type:ty;

                $tt:item
            }
        )*
    ) => {
        $(
            $(#[$attr])*
            pub fn $matcher_name(
                $predicate_name: impl IntoIterator<Item = PredicateBox<$subject_type>>
            ) -> PredicateBox<$parent_type> {
                struct Matcher {
                    $predicate_name: Vec<PredicateBox<$subject_type>>,
                }

                impl Predicate for Matcher {
                    type Subject = $parent_type;

                    $tt
                }

                Box::new(Matcher {
                    $predicate_name: $predicate_name.into_iter().collect(),
                })
            }
        )*
    };
}

macro_rules! impl_ext_trait {
    (
        $trait_name:ty, $func_ident:ident() -> $return_ty:ty
        {
            $($impl_ty:ty => $block:expr),*
        }
    ) => {
        $(
            impl $trait_name for $impl_ty {
                fn $func_ident(&self) -> $return_ty {
                    let block: fn(&$impl_ty) -> $return_ty = $block;

                    (block)(self)
                }
            }
        )*
    };
}

pub trait HasNodeId {
    fn node_id(&self) -> NodeId;
}

impl_ext_trait!(
    HasNodeId, node_id() -> NodeId
    {
        // Nodes
        Node => |item| item.id(),
        FunctionDefinition => |item| item.id,
        TraitImplementation => |item| item.id,
        Implementation => |item| item.id,
        Field => |item| item.id,
        Parameter => |item| item.id,
        MethodDefinition => |item| item.id,
        TraitMethodDefinition => |item| item.id,
        TraitMethodImplementation => |item| item.id,
        Pattern => |item| item.id,
        Statement => |item| item.id,
        Expression => |item| item.id,
        TypeVariable => |item| item.id,

        // Types
        TypeDefinition => |item| item.id(),
        EnumDefinition => |item| item.id,
        StructDefinition => |item| item.id,
        TraitDefinition => |item| item.id,
        TypeParameter => |item| item.id,

        // Statement
        VariableDeclaration => |item| item.id,
        Break => |item| item.id,
        Continue => |item| item.id,
        Final => |item| item.id,
        Return => |item| item.id,
        InfiniteLoop => |item| item.id,

        // Expression
        Assignment => |item| item.id,
        Cast => |item| item.id,
        Construct => |item| item.id,
        DerefExpr => |item| item.id,
        RefExpr => |item| item.id,
        StaticCall => |item| item.id,
        InstanceCall => |item| item.id,
        IntrinsicCall => |item| item.id,
        If => |item| item.id,
        Is => |item| item.id,
        Literal => |item| item.id,
        Member => |item| item.id,
        Scope => |item| item.id,
        Switch => |item| item.id,
        Variable => |item| item.id,
        Variant => |item| item.id
    }
);

pub trait HasName {
    fn name(&self) -> &str;
}

impl_ext_trait!(
    HasName, name() -> &str
    {
        // Nodes
        FunctionDefinition => |item| item.signature.name.name().as_str(),
        Field => |item| item.name.as_str(),
        Parameter => |item| item.name.as_str(),
        MethodDefinition => |item| item.signature.name.name().as_str(),
        TraitMethodDefinition => |item| item.signature.name.name().as_str(),
        TraitMethodImplementation => |item| item.signature.name.name().as_str(),

        // Types
        EnumDefinition => |item| item.name.name().as_str(),
        StructDefinition => |item| item.name.name().as_str(),
        TraitDefinition => |item| item.name.name().as_str(),
        TypeParameter => |item| item.name.as_str(),

        // Statement
        VariableDeclaration => |item| item.name.as_str(),

        // Expression
        Variable => |item| item.name.as_str()
    }
);

pub trait HasAttributes {
    fn attributes(&self) -> &[Attribute];
}

impl_ext_trait!(
    HasAttributes, attributes() -> &[Attribute]
    {
        FunctionDefinition => |item| &item.attrs,
        EnumDefinition => |item| &item.attrs,
        StructDefinition => |item| &item.attrs,
        Implementation => |item| &item.attrs,
        Field => |item| &item.attrs,
        MethodDefinition => |item| &item.attrs,
        TraitDefinition => |item| &item.attrs,
        TraitMethodDefinition => |item| &item.attrs,
        TraitMethodImplementation => |item| &item.attrs
    }
);

pub trait HasDocumentation: Sized {
    fn docs(&self) -> Option<&String>;
}

impl_ext_trait!(
    HasDocumentation, docs() -> Option<&String>
    {
        FunctionDefinition => |item| item.doc_comment.as_ref(),
        EnumDefinition => |item| item.doc_comment.as_ref(),
        EnumDefinitionCase => |item| item.doc_comment.as_ref(),
        StructDefinition => |item| item.doc_comment.as_ref(),
        Field => |item| item.doc_comment.as_ref(),
        MethodDefinition => |item| item.doc_comment.as_ref(),
        TraitDefinition => |item| item.doc_comment.as_ref(),
        TraitMethodDefinition => |item| item.doc_comment.as_ref(),
        TraitMethodImplementation => |item| item.doc_comment.as_ref()
    }
);

pub trait HasVisibility: Sized {
    fn visibility(&self) -> Visibility;
}

impl_ext_trait!(
    HasVisibility, visibility() -> Visibility
    {
        FunctionDefinition => |item| item.visibility,
        EnumDefinition => |item| item.visibility,
        StructDefinition => |item| item.visibility,
        Field => |item| item.visibility,
        MethodDefinition => |item| item.visibility,
        TraitDefinition => |item| item.visibility
    }
);

pub trait HasSignature {
    fn signature(&self) -> &FnSignature;
}

impl_ext_trait!(
    HasSignature, signature() -> &FnSignature
    {
        FunctionDefinition => |item| &item.signature,
        MethodDefinition => |item| &item.signature,
        TraitMethodDefinition => |item| &item.signature,
        TraitMethodImplementation => |item| &item.signature
    }
);

pub trait HasBody {
    fn body(&self) -> Option<&Block>;
}

impl_ext_trait!(
    HasBody, body() -> Option<&Block>
    {
        FunctionDefinition => |item| item.block.as_ref(),
        MethodDefinition => |item| item.block.as_ref(),
        TraitMethodDefinition => |item| item.block.as_ref(),
        TraitMethodImplementation => |item| item.block.as_ref()
    }
);

pub trait HasBlock {
    fn block(&self) -> NodeId;
}

impl_ext_trait!(
    HasBlock, block() -> NodeId
    {
        InfiniteLoop => |item| item.block.id,
        Condition => |item| item.block.id,
        SwitchCase => |item| item.branch
    }
);

pub trait HasPattern {
    fn pattern(&self) -> NodeId;
}

impl_ext_trait!(
    HasPattern, pattern() -> NodeId
    {
        Is => |item| item.pattern,
        SwitchCase => |item| item.pattern,
        PatternField => |item| item.pattern
    }
);
