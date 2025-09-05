use error_snippet::Result;
use lume_span::DefId;

use crate::LowerFunction;

impl LowerFunction<'_> {
    pub(crate) fn pattern(&mut self, pattern: &lume_hir::Pattern) -> Result<lume_tir::Pattern> {
        match &pattern.kind {
            lume_hir::PatternKind::Literal(lit) => {
                let literal = self.literal(&lit.literal);

                Ok(lume_tir::Pattern {
                    id: pattern.id,
                    kind: lume_tir::PatternKind::Literal(literal),
                })
            }
            lume_hir::PatternKind::Identifier(_) => {
                let var = self.mark_variable(lume_tir::VariableSource::Variable);
                self.variable_mapping.insert(DefId::Pattern(pattern.id), var);

                Ok(lume_tir::Pattern {
                    id: pattern.id,
                    kind: lume_tir::PatternKind::Variable(var),
                })
            }
            lume_hir::PatternKind::Variant(pat) => {
                let enum_type = pat.enum_name();
                let def_id = DefId::Pattern(pattern.id);

                let index = self.lower.tcx.enum_case_with_name(&pat.name)?.idx;
                let ty = self.lower.tcx.find_type_ref_from(&enum_type, def_id)?.unwrap();
                let name = self.path_hir(&pat.name, def_id)?;

                Ok(lume_tir::Pattern {
                    id: pattern.id,
                    kind: lume_tir::PatternKind::Variant(lume_tir::VariantPattern {
                        index: index as u8,
                        ty,
                        name,
                    }),
                })
            }
            lume_hir::PatternKind::Wildcard(_) => Ok(lume_tir::Pattern {
                id: pattern.id,
                kind: lume_tir::PatternKind::Wildcard,
            }),
        }
    }
}
