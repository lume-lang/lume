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

        self.do_patterns_exhaust_type(&operand_type, &patterns, &[])
    }

    /// Determines whether the given patterns exhaust all the possible values of
    /// the given type.
    fn do_patterns_exhaust_type(
        &self,
        ty: &TypeRef,
        patterns: &[&lume_hir::Pattern],
        parents: &[(&lume_hir::VariantPattern, usize)],
    ) -> Result<()> {
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

                // Only test the patterns which match the name of the current pattern, since
                // we can otherwise situtations where all technically all variants are present,
                // but not in all possible configurations.
                //
                // For example, given variants like this:
                // ```lm
                // enum Foo {
                //     A(Bar),
                //     B(Bar),
                // }
                //
                // enum Bar {
                //     A,
                //     B,
                // }
                //
                // fn foo(kind: Foo) -> Int32 {
                //     switch kind {
                //         Foo::A(Bar::A) => 4_i32,
                //         Foo::B(Bar::A) => 5_i32,
                //         Foo::B(Bar::B) => 6_i32,
                //     }
                // }
                // ```
                // would not raise any error since all variants are present, even though
                // `Foo::A(Bar::B)` isn't in the matrix.
                //
                // If we limit to only matching expressions, we essentially get this, where we
                // only check for `Foo::A`:
                // ```lm
                // fn foo(kind: Foo) -> Int32 {
                //     switch kind {
                //         Foo::A(Bar::A) => 4_i32,
                //     }
                // }
                // ```
                // which would raise the correct error.
                let matching_patterns = matching_variant_patterns(variant_pattern, patterns);

                for (field_idx, enum_field) in enum_case_def.parameters.iter().enumerate() {
                    let enum_field_ty = self
                        .mk_type_ref_from(enum_field, enum_def.id)?
                        .with_location(ty.location);

                    let subpatterns = column_of_patterns(&matching_patterns, field_idx)?;
                    let parents = &[parents, &[(variant_pattern, field_idx)][..]].concat();

                    self.do_patterns_exhaust_type(&enum_field_ty, &subpatterns, &parents)?;
                }
            }
        }

        if !missing_variants.is_empty() {
            let matched_type = self.new_named_type(ty, true)?;
            let mut unmatched_cases = Vec::with_capacity(missing_variants.len());

            for missing_variant in missing_variants {
                let variant_name = format!("{:+}::{:+}", enum_def.name, enum_def.cases[missing_variant].name);
                let formatted_lineage = self.format_variant_lineage(variant_name, parents)?;

                unmatched_cases.push(formatted_lineage);
            }

            return Err(errors::CaseNotCovered::from_cases(matched_type, unmatched_cases, ty.location).into());
        }

        Ok(())
    }

    /// Formats the given "lineage" of variant patterns into a single string,
    /// which can be rendered within an error message to the user.
    ///
    /// For example, given a `variant_name` of `Bar::B` which is nested within
    /// a parent chain of `(Foo::A, 1)` where `Foo::A` has 3 fields, this method
    /// would return `Foo::A(.., Bar::B, ..)`.
    fn format_variant_lineage(
        &self,
        variant_name: String,
        parents: &[(&lume_hir::VariantPattern, usize)],
    ) -> Result<String> {
        let mut fmt = variant_name;

        for (parent, field_idx) in parents {
            let enum_case_def = self.enum_case_with_name(&parent.name)?;
            let field_len = enum_case_def.parameters.len();

            let placeholders_before = ".., ".repeat(*field_idx);
            let placeholders_after = ", ..".repeat(field_len - *field_idx - 1);

            fmt = format!("{:+}({placeholders_before}{fmt}{placeholders_after})", parent.name);
        }

        Ok(fmt)
    }
}

/// Filters out all patterns from `patterns` which are a name match of `primary`
/// and returns them.
fn matching_variant_patterns<'pat>(
    primary: &'pat lume_hir::VariantPattern,
    patterns: &[&'pat lume_hir::Pattern],
) -> Vec<&'pat lume_hir::Pattern> {
    let mut matching = Vec::new();

    for pattern in patterns {
        let lume_hir::PatternKind::Variant(variant_pattern) = &pattern.kind else {
            continue;
        };

        if variant_pattern.name.is_name_match(&primary.name) {
            matching.push(*pattern);
        }
    }

    matching
}

/// Given a list of patterns, like the ones within a switch expression,
/// which are assumed to all be variant patterns, get the (zero-indexed) Nth
/// column within each of them.
///
/// For example, assume a switch expression like this:
/// ```lm
/// switch a {
///     Foo::A(One::A, Two::A, Three::A) => { },
///     Foo::B(One::B, Two::B, Three::B) => { },
///     Foo::C(One::C, Two::C, Three::C) => { },
/// }
/// ```
///
/// when passing the patterns of the `switch` expression and a `column` of
/// 1, this method returns the patterns `[Two::A, Two::B, Two::C]`.
fn column_of_patterns<'pat>(
    patterns: &[&'pat lume_hir::Pattern],
    column: usize,
) -> Result<Vec<&'pat lume_hir::Pattern>> {
    let mut subpatterns = Vec::new();

    for pattern in patterns {
        let lume_hir::PatternKind::Variant(pattern) = &pattern.kind else {
            continue;
        };

        let Some(subpattern) = pattern.fields.get(column) else {
            libftrace::warning!("variant subpattern `{:+}` did not have column {column}", pattern.name);
            continue;
        };

        subpatterns.push(subpattern);
    }

    Ok(subpatterns)
}
