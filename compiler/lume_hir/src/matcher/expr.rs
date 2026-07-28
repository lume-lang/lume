use crate::matcher::{Bindings, Predicate, PredicateBox};
use crate::*;

lumec_matchers! {
    /// Matches all expressions.
    matcher expression(predicates) -> bool {
        type Parent = Node;
        type Subject = Expression;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            if let Node::Expression(subject) = subject {
                Predicate::satisfies(&self.predicates, hir, subject, bindings)
            } else {
                false
            }
        }
    }

    /// Matches all variable references.
    matcher var_reference(predicates) -> bool {
        type Parent = Expression;
        type Subject = Variable;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            if let ExpressionKind::Variable(subject) = &subject.kind {
                Predicate::satisfies(&self.predicates, hir, subject, bindings)
            } else {
                false
            }
        }
    }

    /// Matches all switch expressions.
    matcher switch(predicates) -> bool {
        type Parent = Expression;
        type Subject = Switch;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            if let ExpressionKind::Switch(subject) = &subject.kind {
                Predicate::satisfies(&self.predicates, hir, subject, bindings)
            } else {
                false
            }
        }
    }

    /// Matches the operand of a switch expression.
    matcher has_operand(predicates) -> bool {
        type Parent = Switch;
        type Subject = Expression;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            let operand = hir.expect_expression(subject.operand).unwrap();

            Predicate::satisfies(&self.predicates, hir, operand, bindings)
        }
    }

    /// Matches a switch arm of a switch expression.
    matcher has_case(predicates) -> bool {
        type Parent = Switch;
        type Subject = SwitchCase;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            subject.cases.iter().any(|case| Predicate::satisfies(&self.predicates, hir, case, bindings))
        }
    }

    /// Matches the branch of a switch case arm.
    matcher has_branch(predicates) -> bool {
        type Parent = SwitchCase;
        type Subject = Expression;

        fn satisfies(&self, hir: &Map, subject: &Self::Subject, bindings: &mut Bindings) -> bool {
            let operand = hir.expect_expression(subject.branch).unwrap();

            Predicate::satisfies(&self.predicates, hir, operand, bindings)
        }
    }
}
