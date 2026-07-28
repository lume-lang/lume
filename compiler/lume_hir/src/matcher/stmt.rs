use crate::matcher::{Bindings, Predicate, PredicateBox};
use crate::*;

lumec_matchers! {
    /// Matches all statements.
    matcher statement(predicates) -> bool {
        type Parent = Node;
        type Subject = Statement;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            if let Node::Statement(subject) = subject {
                Predicate::satisfies(&self.predicates, hir, subject, bindings)
            } else {
                false
            }
        }
    }

    /// Matches all variable declarations.
    matcher var_decl(predicates) -> bool {
        type Parent = Statement;
        type Subject = VariableDeclaration;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            if let StatementKind::Variable(subject) = &subject.kind {
                Predicate::satisfies(&self.predicates, hir, subject, bindings)
            } else {
                false
            }
        }
    }

    /// Matches with the declared type of a variable declaration, if it has one.
    matcher has_type_decl(predicates) -> bool {
        type Parent = VariableDeclaration;
        type Subject = Type;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            if let Some(subject) = &subject.declared_type {
                Predicate::satisfies(&self.predicates, hir, subject, bindings)
            } else {
                false
            }
        }
    }
}
