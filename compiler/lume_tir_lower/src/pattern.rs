use error_snippet::Result;

use crate::LowerFunction;

impl LowerFunction<'_> {
    pub(crate) fn pattern(&mut self, pattern: &lume_hir::Pattern) -> Result<lume_tir::Pattern> {
        let pattern_type = self.lower.tcx.type_of_pattern(pattern)?;

        match &pattern.kind {
            lume_hir::PatternKind::Literal(lit) => {
                let literal = self.literal(&lit.literal);

                Ok(lume_tir::Pattern {
                    id: pattern.id,
                    ty: pattern_type,
                    kind: lume_tir::PatternKind::Literal(literal),
                    location: pattern.location,
                })
            }
            lume_hir::PatternKind::Identifier(_) => {
                let var = self.mark_variable(lume_tir::VariableSource::Variable);
                self.variables.add_mapping(pattern.id, var);

                Ok(lume_tir::Pattern {
                    id: pattern.id,
                    ty: pattern_type,
                    kind: lume_tir::PatternKind::Variable(var),
                    location: pattern.location,
                })
            }
            lume_hir::PatternKind::Variant(pat) => {
                let enum_type = pat.enum_name();

                let index = self.lower.tcx.enum_case_with_name(&pat.name)?.idx;
                let ty = self.lower.tcx.find_type_ref_from(&enum_type, pattern.id)?.unwrap();
                let name = self.path_hir(&pat.name, pattern.id)?;

                let fields = pat
                    .fields
                    .iter()
                    .map(|field| self.pattern(field))
                    .collect::<Result<Vec<_>>>()?;

                Ok(lume_tir::Pattern {
                    id: pattern.id,
                    ty: pattern_type,
                    kind: lume_tir::PatternKind::Variant(lume_tir::VariantPattern {
                        index: u8::try_from(index).unwrap(),
                        ty,
                        name,
                        fields,
                    }),
                    location: pattern.location,
                })
            }
            lume_hir::PatternKind::Wildcard(_) => Ok(lume_tir::Pattern {
                id: pattern.id,
                ty: pattern_type,
                kind: lume_tir::PatternKind::Wildcard,
                location: pattern.location,
            }),
        }
    }
}
