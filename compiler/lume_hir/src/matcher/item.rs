use std::marker::PhantomData;

use crate::matcher::{Bindings, HasBody, HasDocumentation, HasName, HasVisibility, Predicate, PredicateBox};
use crate::*;

/// Matches anything, regardless of content or subject.
///
/// This can be useful when binding a value without actually wanting to add a
/// predicate to the matcher.
///
/// For example:
/// ```lume
/// switch foo {
///     .. => { },
/// }
/// ```
/// with a matcher of:
/// ```rust (ignore)
/// expression([
///     switch([
///         has_case([
///             has_pattern([ident_pattern([])]),
///             has_branch([any().bind("ident_block")])
///         ]),
///     ])
/// ])
/// ```
/// will bind the `{ }` branch, without any other predicate.
pub fn any<P: 'static>() -> PredicateBox<P> {
    struct Matcher<P> {
        _marker: PhantomData<P>,
    }

    impl<P> Predicate for Matcher<P> {
        type Subject = P;

        fn satisfies(&self, _hir: &Map, _subject: &Self::Subject, _bindings: &mut Bindings) -> bool {
            true
        }
    }

    Box::new(Matcher::<P> { _marker: PhantomData })
}

/// Matches anything regardless of sub-predicates.
///
/// This can be useful when a predicate doesn't need to match, but still bind
/// nodes when it does match.
///
/// For example:
/// ```lume
/// let foo: T = bar;
/// ```
/// with a matcher of:
/// ```rust (ignore)
/// statement([
///     var_decl([
///         optionally([has_type_decl([any().bind("var_type")])]),
///     ]).bind("var_decl")
/// ])
/// ```
/// will bind the type `T` to `"var_type"` when present. Regardless of whether
/// there is a type declaration, `"var_decl"` will be bound.
pub fn optionally<S>(predicates: impl IntoIterator<Item = PredicateBox<S>>) -> PredicateBox<S>
where
    S: 'static,
{
    struct Matcher<S> {
        predicates: Vec<PredicateBox<S>>,
    }

    impl<S> Predicate for Matcher<S> {
        type Subject = S;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            Predicate::satisfies(&self.predicates, hir, subject, bindings);

            true
        }
    }

    Box::new(Matcher {
        predicates: predicates.into_iter().collect(),
    })
}

/// Matches with the node if the name of the node is equal to the given string.
pub fn has_name<P>(name: impl AsRef<str>) -> PredicateBox<P>
where
    P: HasName + 'static,
{
    struct Matcher<P> {
        name: String,
        _marker: PhantomData<P>,
    }

    impl<P> Predicate for Matcher<P>
    where
        P: HasName,
    {
        type Subject = P;

        fn satisfies(&self, _hir: &Map, subject: &Self::Subject, _bindings: &mut Bindings) -> bool {
            <Self::Subject as HasName>::name(subject) == self.name
        }
    }

    Box::new(Matcher::<P> {
        name: name.as_ref().to_string(),
        _marker: PhantomData,
    })
}

/// Matches with the documentation string of a node, if it has one.
pub fn has_documentation<P>(predicates: impl IntoIterator<Item = PredicateBox<String>>) -> PredicateBox<P>
where
    P: HasDocumentation + 'static,
{
    struct Matcher<P> {
        predicates: Vec<PredicateBox<String>>,
        _marker: PhantomData<P>,
    }

    impl<P> Predicate for Matcher<P>
    where
        P: HasDocumentation,
    {
        type Subject = P;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            if let Some(subject) = <Self::Subject as HasDocumentation>::docs(subject) {
                Predicate::satisfies(&self.predicates, hir, subject, bindings)
            } else {
                false
            }
        }
    }

    Box::new(Matcher::<P> {
        predicates: predicates.into_iter().collect(),
        _marker: PhantomData,
    })
}

/// Matches with the node if the visibility of the node is equal to the given
/// visibility.
pub fn has_visibility<P>(visibility: Visibility) -> PredicateBox<P>
where
    P: HasVisibility + 'static,
{
    struct Matcher<P> {
        visibility: Visibility,
        _marker: PhantomData<P>,
    }

    impl<P> Predicate for Matcher<P>
    where
        P: HasVisibility,
    {
        type Subject = P;

        fn satisfies(&self, _hir: &Map, subject: &Self::Subject, _bindings: &mut Bindings) -> bool {
            <Self::Subject as HasVisibility>::visibility(subject) == self.visibility
        }
    }

    Box::new(Matcher::<P> {
        visibility,
        _marker: PhantomData,
    })
}

/// Matches with the block of a node, if it has one.
pub fn has_body<P>(predicates: impl IntoIterator<Item = PredicateBox<Block>>) -> PredicateBox<P>
where
    P: HasBody + 'static,
{
    struct Matcher<P> {
        predicates: Vec<PredicateBox<Block>>,
        _marker: PhantomData<P>,
    }

    impl<P> Predicate for Matcher<P>
    where
        P: HasBody,
    {
        type Subject = P;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            if let Some(block) = <Self::Subject as HasBody>::body(subject) {
                Predicate::satisfies(&self.predicates, hir, block, bindings)
            } else {
                false
            }
        }
    }

    Box::new(Matcher::<P> {
        predicates: predicates.into_iter().collect(),
        _marker: PhantomData,
    })
}

/// Matches with all items which pass the given filter.
pub fn filter<S, F>(filter: F, predicates: impl IntoIterator<Item = PredicateBox<S>>) -> PredicateBox<S>
where
    S: 'static,
    F: Fn(&S) -> bool + 'static,
{
    struct Matcher<S, F> {
        predicates: Vec<PredicateBox<S>>,
        filter: F,
    }

    impl<S, F> Predicate for Matcher<S, F>
    where
        F: Fn(&S) -> bool,
    {
        type Subject = S;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            if (self.filter)(subject) {
                Predicate::satisfies(&self.predicates, hir, subject, bindings)
            } else {
                false
            }
        }
    }

    Box::new(Matcher {
        predicates: predicates.into_iter().collect(),
        filter,
    })
}

lumec_matchers! {
    /// Matches all function definitions.
    matcher function(predicates) -> bool {
        type Parent = Node;
        type Subject = FunctionDefinition;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            if let Node::Function(subject) = subject {
                Predicate::satisfies(&self.predicates, hir, subject, bindings)
            } else {
                false
            }
        }
    }

    /// Matches all method definitions.
    matcher method(predicates) -> bool {
        type Parent = Node;
        type Subject = MethodDefinition;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            if let Node::Method(subject) = subject {
                Predicate::satisfies(&self.predicates, hir, subject, bindings)
            } else {
                false
            }
        }
    }
}
