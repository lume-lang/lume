use crate::LowerModule;

use error_snippet::Result;

impl LowerModule<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn pattern(&mut self, pattern: lume_ast::Pattern) -> Result<lume_hir::Pattern> {
        match pattern {
            lume_ast::Pattern::Literal(pat) => Ok(lume_hir::Pattern::Literal(self.literal(pat))),
            lume_ast::Pattern::Identifier(pat) => Ok(lume_hir::Pattern::Identifier(self.identifier(pat))),
            lume_ast::Pattern::Variant(pat) => {
                let name = self.resolve_symbol_name(&pat.name)?;
                let location = self.location(pat.location);

                let fields = pat
                    .fields
                    .into_iter()
                    .map(|arg| self.pattern(arg))
                    .collect::<Result<Vec<_>>>()?;

                Ok(lume_hir::Pattern::Variant(lume_hir::VariantPattern {
                    name,
                    fields,
                    location,
                }))
            }
            lume_ast::Pattern::Wildcard(pat) => Ok(lume_hir::Pattern::Wildcard(lume_hir::WildcardPattern {
                location: self.location(pat.location),
            })),
        }
    }
}
