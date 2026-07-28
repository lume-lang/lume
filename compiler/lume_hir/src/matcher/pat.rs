use std::marker::PhantomData;

use crate::matcher::{Bindings, HasPattern, Predicate, PredicateBox};
use crate::*;

lumec_matchers! {
    /// Matches all patterns.
    matcher pattern(predicates) -> bool {
        type Parent = Node;
        type Subject = Pattern;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            if let Node::Pattern(subject) = subject {
                Predicate::satisfies(&self.predicates, hir, subject, bindings)
            } else {
                false
            }
        }
    }
}

lumec_matchers! {
    /// Matches all literal patterns.
    matcher literal_pattern(predicates) -> bool {
        type Parent = Pattern;
        type Subject = LiteralPattern;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            if let PatternKind::Literal(subject) = &subject.kind {
                Predicate::satisfies(&self.predicates, hir, subject, bindings)
            } else {
                false
            }
        }
    }
}

lumec_matchers! {
    /// Matches all identifier patterns.
    matcher ident_pattern(predicates) -> bool {
        type Parent = Pattern;
        type Subject = IdentifierPattern;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            if let PatternKind::Identifier(subject) = &subject.kind {
                Predicate::satisfies(&self.predicates, hir, subject, bindings)
            } else {
                false
            }
        }
    }
}

lumec_matchers! {
    /// Matches all variant patterns.
    matcher variant_pattern(predicates) -> bool {
        type Parent = Pattern;
        type Subject = VariantPattern;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            if let PatternKind::Variant(subject) = &subject.kind {
                Predicate::satisfies(&self.predicates, hir, subject, bindings)
            } else {
                false
            }
        }
    }
}

lumec_matchers! {
    /// Matches all wildcard patterns.
    matcher wildcard_pattern(predicates) -> bool {
        type Parent = Pattern;
        type Subject = WildcardPattern;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            if let PatternKind::Wildcard(subject) = &subject.kind {
                Predicate::satisfies(&self.predicates, hir, subject, bindings)
            } else {
                false
            }
        }
    }
}

/// Matches with the pattern of a node, if it has one.
pub fn has_pattern<P>(predicates: impl IntoIterator<Item = PredicateBox<Pattern>>) -> PredicateBox<P>
where
    P: HasPattern + 'static,
{
    struct Matcher<P> {
        predicates: Vec<PredicateBox<Pattern>>,
        _marker: PhantomData<P>,
    }

    impl<P> Predicate for Matcher<P>
    where
        P: HasPattern,
    {
        type Subject = P;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            let pattern_id = <Self::Subject as HasPattern>::pattern(subject);
            let pattern_node = hir.expect_pattern(pattern_id).unwrap();

            Predicate::satisfies(&self.predicates, hir, pattern_node, bindings)
        }
    }

    Box::new(Matcher::<P> {
        predicates: predicates.into_iter().collect(),
        _marker: PhantomData,
    })
}
