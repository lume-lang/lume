use crate::*;

impl LoweringContext<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn pattern(&mut self, pattern: lume_ast::Pat) -> NodeId {
        let id = self.next_node_id();

        let pat = match pattern {
            lume_ast::Pat::PatLiteral(pat) => {
                let literal = pat
                    .literal()
                    .map_or(lume_hir::Literal::missing(), |lit| self.literal(lit));

                let location = self.location(pat.location());

                lume_hir::Pattern {
                    id,
                    kind: lume_hir::PatternKind::Literal(lume_hir::LiteralPattern { literal, location }),
                    location,
                }
            }
            lume_ast::Pat::PatIdent(pat) => {
                let name = self.ident_opt(pat.name());
                let location = self.location(pat.location());

                let pattern = lume_hir::Pattern {
                    id,
                    kind: lume_hir::PatternKind::Identifier(lume_hir::IdentifierPattern {
                        name: name.clone(),
                        location,
                    }),
                    location,
                };

                self.current_locals
                    .define(name.to_string(), lume_hir::VariableSource::Pattern(pattern.id));

                pattern
            }
            lume_ast::Pat::PatVariant(pat) => {
                let name = self.resolve_symbol_name_opt(pat.path());
                let fields = pat.pat().map(|arg| self.subpattern(arg)).collect::<Vec<_>>();
                let location = self.location(pat.location());

                lume_hir::Pattern {
                    id,
                    kind: lume_hir::PatternKind::Variant(lume_hir::VariantPattern { name, fields, location }),
                    location,
                }
            }
            lume_ast::Pat::PatWildcard(pat) => {
                let location = self.location(pat.location());

                lume_hir::Pattern {
                    id,
                    kind: lume_hir::PatternKind::Wildcard(lume_hir::WildcardPattern { location }),
                    location,
                }
            }
        };

        self.map.nodes.insert(id, lume_hir::Node::Pattern(pat));

        id
    }

    pub(crate) fn missing_pat(&mut self, id: Option<NodeId>) -> lume_hir::Pattern {
        lume_hir::Pattern {
            id: id.unwrap_or_else(|| self.next_node_id()),
            kind: lume_hir::PatternKind::Missing,
            location: Location::empty(),
        }
    }

    pub(super) fn pattern_opt(&mut self, pattern: Option<lume_ast::Pat>) -> NodeId {
        if let Some(pat) = pattern {
            self.pattern(pat)
        } else {
            let id = self.next_node_id();
            let pat = self.missing_pat(Some(id));

            self.map.nodes.insert(id, lume_hir::Node::Pattern(pat));

            id
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn subpattern(&mut self, pattern: lume_ast::Pat) -> NodeId {
        match pattern {
            lume_ast::Pat::PatLiteral(_) | lume_ast::Pat::PatWildcard(_) | lume_ast::Pat::PatVariant(_) => {
                self.pattern(pattern)
            }
            lume_ast::Pat::PatIdent(pat) => {
                let id = self.next_node_id();
                let name = self.ident_opt(pat.name());
                let location = self.location(pat.location());

                let pattern = lume_hir::Pattern {
                    id,
                    kind: lume_hir::PatternKind::Identifier(lume_hir::IdentifierPattern {
                        name: name.clone(),
                        location,
                    }),
                    location,
                };

                self.map.nodes.insert(id, lume_hir::Node::Pattern(pattern));

                self.current_locals
                    .define(name.to_string(), lume_hir::VariableSource::Pattern(id));

                id
            }
        }
    }
}
