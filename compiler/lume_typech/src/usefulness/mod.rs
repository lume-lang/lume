mod errors;

use indexmap::IndexSet;
use lume_errors::Result;
use lume_types::TypeRef;

use crate::TyCheckCtx;

impl TyCheckCtx {
    /// Checks whether the given `switch` expression is exhausted.
    pub(crate) fn check_switch_exhaustiveness(&self, expr: &lume_hir::Switch) -> Result<()> {
        let operand_type = self.type_of(expr.operand)?;
        let patterns = expr.cases.iter().map(|case| &case.pattern).collect::<Vec<_>>();

        self.do_patterns_exhaust_type(&operand_type, &patterns)
    }

    /// Determines whether the given patterns exhaust all the possible values of
    /// the given type.
    fn do_patterns_exhaust_type(&self, ty: &TypeRef, patterns: &[&lume_hir::Pattern]) -> Result<()> {
        // If the patterns have any fallback patterns, it is always exhausted.
        if patterns.iter().any(|pat| pat.is_fallback()) {
            return Ok(());
        }

        // If there aren't any fallback patterns, we do not consider any floating-point
        // numbers to be exhausted, since they're generally unstable for comparisons.
        if ty.is_float() {
            return Err(errors::CaseNotCoveredFloat {
                location: ty.location,
                unmatched_case: String::from(".."),
                matched_type: self.new_named_type(ty, true)?,
            }
            .into());
        }

        // While we could allow exhaustion of integer patterns, we currently don't allow
        // it.
        if ty.is_integer() {
            return Err(errors::CaseNotCoveredInteger {
                location: ty.location,
                unmatched_case: String::from(".."),
                matched_type: self.new_named_type(ty, true)?,
            }
            .into());
        }

        let Ok(enum_def) = self.enum_def_type(ty.instance_of) else {
            panic!("bug!: expected to find `EnumDefinition` because of exhausted pattern types");
        };

        let mut missing_variants: IndexSet<usize> = IndexSet::from_iter(0..enum_def.cases.len());

        for pattern in patterns {
            if let lume_hir::PatternKind::Variant(variant_pattern) = &pattern.kind {
                debug_assert!(variant_pattern.enum_name().is_name_match(&enum_def.name));

                let enum_case_def = self.enum_case_with_name(&variant_pattern.name)?;
                missing_variants.swap_remove(&enum_case_def.idx);
            }
        }

        if !missing_variants.is_empty() {
            let matched_type = self.new_named_type(ty, true)?;
            let mut unmatched_cases = Vec::with_capacity(missing_variants.len());

            for missing_variant in missing_variants {
                let variant_name = format!("{:+}::{:+}", enum_def.name, enum_def.cases[missing_variant].name);

                unmatched_cases.push(variant_name);
            }

            return Err(errors::CaseNotCovered::from_cases(matched_type, unmatched_cases, ty.location).into());
        }

        Ok(())
    }
}
