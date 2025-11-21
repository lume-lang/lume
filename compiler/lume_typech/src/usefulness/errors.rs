use error_snippet_derive::Diagnostic;
use lume_span::Location;
use lume_types::NamedTypeRef;

#[derive(Diagnostic, Clone, Debug, PartialEq, Eq)]
#[diagnostic(
    message = "non-exhaustive switch expression",
    code = "LM4343",
    help = "try adding a fallback case to catch unmatched cases"
)]
pub struct CaseNotCovered {
    #[label(source, "unmatched switch case: {unmatched_case}")]
    pub location: Location,

    pub unmatched_case: String,
    pub matched_type: NamedTypeRef,
}

impl CaseNotCovered {
    pub fn from_cases(matched_type: NamedTypeRef, cases: Vec<String>, location: Location) -> Self {
        let mut unmatched_len = 0;
        let mut unmatched_cases = Vec::<String>::new();

        let cases_len = cases.len();

        for (idx, case) in cases.into_iter().enumerate() {
            if unmatched_len > 20 {
                let remaining = cases_len - idx + 1;

                unmatched_cases.push(format!("and {remaining} more"));
                break;
            }

            unmatched_len += case.len();
            unmatched_cases.push(case);
        }

        Self {
            location,
            unmatched_case: unmatched_cases.join(", "),
            matched_type,
        }
    }
}

#[derive(Diagnostic, Clone, Debug, PartialEq, Eq)]
#[diagnostic(
    message = "non-exhaustive switch expression",
    code = "LM4343",
    help = "integers must have a fallback to be exhausted"
)]
pub struct CaseNotCoveredInteger {
    #[label(source, "switch case `{unmatched_case}` is unmatched")]
    pub location: Location,

    pub unmatched_case: String,
    pub matched_type: NamedTypeRef,
}

#[derive(Diagnostic, Clone, Debug, PartialEq, Eq)]
#[diagnostic(
    message = "non-exhaustive switch expression",
    code = "LM4343",
    help = "floating-point numbers must have a fallback to be exhausted"
)]
pub struct CaseNotCoveredFloat {
    #[label(source, "switch case `{unmatched_case}` is unmatched")]
    pub location: Location,

    pub unmatched_case: String,
    pub matched_type: NamedTypeRef,
}

#[derive(Diagnostic, Clone, Debug, PartialEq, Eq)]
#[diagnostic(
    message = "non-exhaustive switch expression",
    code = "LM4343",
    help = "strings must have a fallback to be exhausted"
)]
pub struct CaseNotCoveredString {
    #[label(source, "switch case `{unmatched_case}` is unmatched")]
    pub location: Location,

    pub unmatched_case: String,
    pub matched_type: NamedTypeRef,
}
