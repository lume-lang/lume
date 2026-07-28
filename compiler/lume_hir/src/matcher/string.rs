use crate::matcher::{Bindings, Predicate, PredicateBox};
use crate::*;

/// Matches with a string which is entirely empty.
pub fn str_empty(empty: bool) -> PredicateBox<String> {
    struct Matcher {
        empty: bool,
    }

    impl Predicate for Matcher {
        type Subject = String;

        fn satisfies(&self, _hir: &Map, subject: &Self::Subject, _bindings: &mut Bindings) -> bool {
            subject.is_empty() == self.empty
        }
    }

    Box::new(Matcher { empty })
}

/// Matches with a string which contains the given substring.
pub fn str_contains(value: impl AsRef<str>) -> PredicateBox<String> {
    struct Matcher {
        value: String,
    }

    impl Predicate for Matcher {
        type Subject = String;

        fn satisfies(&self, _hir: &Map, subject: &Self::Subject, _bindings: &mut Bindings) -> bool {
            subject.contains(&self.value)
        }
    }

    Box::new(Matcher {
        value: value.as_ref().to_string(),
    })
}
