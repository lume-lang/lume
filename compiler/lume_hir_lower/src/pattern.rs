use error_snippet::Result;

use crate::LowerModule;

impl LowerModule {
    #[libftrace::traced(level = Debug)]
    pub(super) fn pattern(&mut self, pattern: lume_ast::Pattern) -> Result<lume_hir::Pattern> {
        let id = self.next_node_id();

        let pat = match pattern {
            lume_ast::Pattern::Literal(pat) => {
                let literal = self.literal(pat);
                let location = literal.location;

                lume_hir::Pattern {
                    id,
                    kind: lume_hir::PatternKind::Literal(lume_hir::LiteralPattern { literal, location }),
                    location,
                }
            }
            lume_ast::Pattern::Identifier(pat) => {
                let name = self.identifier(pat);
                let location = name.location;

                let pattern = lume_hir::Pattern {
                    id,
                    kind: lume_hir::PatternKind::Identifier(lume_hir::IdentifierPattern {
                        name: name.clone(),
                        location,
                    }),
                    location,
                };

                self.locals
                    .define(name.to_string(), lume_hir::VariableSource::Pattern(pattern.clone()));

                pattern
            }
            lume_ast::Pattern::Variant(pat) => {
                let name = self.resolve_symbol_name(&pat.name)?;
                let location = self.location(pat.location);

                let fields = pat
                    .fields
                    .into_iter()
                    .map(|arg| self.subpattern(arg))
                    .collect::<Result<Vec<_>>>()?;

                lume_hir::Pattern {
                    id,
                    kind: lume_hir::PatternKind::Variant(lume_hir::VariantPattern { name, fields, location }),
                    location,
                }
            }
            lume_ast::Pattern::Wildcard(pat) => {
                let location = self.location(pat.location);

                lume_hir::Pattern {
                    id,
                    kind: lume_hir::PatternKind::Wildcard(lume_hir::WildcardPattern { location }),
                    location,
                }
            }
        };

        self.map.nodes.insert(id, lume_hir::Node::Pattern(pat.clone()));

        Ok(pat)
    }

    #[libftrace::traced(level = Debug)]
    fn subpattern(&mut self, pattern: lume_ast::Pattern) -> Result<lume_hir::Pattern> {
        match pattern {
            lume_ast::Pattern::Literal(_) | lume_ast::Pattern::Wildcard(_) | lume_ast::Pattern::Variant(_) => {
                self.pattern(pattern)
            }
            lume_ast::Pattern::Identifier(pat) => {
                let id = self.next_node_id();
                let name = self.identifier(pat);
                let location = name.location;

                let pattern = lume_hir::Pattern {
                    id,
                    kind: lume_hir::PatternKind::Identifier(lume_hir::IdentifierPattern {
                        name: name.clone(),
                        location,
                    }),
                    location,
                };

                self.locals
                    .define(name.to_string(), lume_hir::VariableSource::Pattern(pattern.clone()));

                Ok(pattern)
            }
        }
    }
}
