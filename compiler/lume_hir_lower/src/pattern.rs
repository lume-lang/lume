use crate::LowerModule;

use error_snippet::Result;
use lume_span::PatternId;

impl LowerModule<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn pattern(&mut self, pattern: lume_ast::Pattern) -> Result<lume_hir::Pattern> {
        let id = self.next_pat_id();

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
                    .enumerate()
                    .map(|(idx, arg)| self.subpattern(arg, id, idx))
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

        self.map.patterns.insert(id, pat.clone());

        Ok(pat)
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn subpattern(&mut self, pattern: lume_ast::Pattern, parent: PatternId, idx: usize) -> Result<lume_hir::Pattern> {
        match pattern {
            lume_ast::Pattern::Literal(_) | lume_ast::Pattern::Wildcard(_) | lume_ast::Pattern::Variant(_) => {
                self.pattern(pattern)
            }
            lume_ast::Pattern::Identifier(pat) => {
                let id = self.next_pat_id();
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

                let ident_decl_value = lume_hir::Expression {
                    id: self.next_expr_id(),
                    kind: lume_hir::ExpressionKind::Field(lume_hir::PatternField {
                        id: self.next_expr_id(),
                        pattern: parent,
                        field: idx,
                        location,
                    }),
                    location,
                };

                let ident_decl_value_id = ident_decl_value.id;
                self.map.expressions.insert(ident_decl_value_id, ident_decl_value);

                // When visiting an identifier sub-pattern, we implicitly define a new variable
                // which refers to the sub-pattern, so it can be referenced within the branch.
                let ident_decl = lume_hir::VariableDeclaration {
                    id: self.next_stmt_id(),
                    name: lume_hir::Identifier {
                        name: name.to_string(),
                        location,
                    },
                    declared_type: None,
                    value: ident_decl_value_id,
                    location,
                };

                self.locals
                    .define(name.to_string(), lume_hir::VariableSource::Variable(ident_decl));

                Ok(pattern)
            }
        }
    }
}
