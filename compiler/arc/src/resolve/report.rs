use indexmap::IndexMap;

use crate::manifest::Manifest;
use crate::resolve::PackageKey;
use crate::resolve::provider::{DependencyError, VersionSet};

pub(crate) fn report(
    packages: IndexMap<PackageKey, Manifest>,
    derivation_tree: &pubgrub::DerivationTree<PackageKey, VersionSet, DependencyError>,
) -> String {
    let formatter = Reporter {
        packages,
        fallback: pubgrub::DefaultStringReportFormatter,
    };

    match derivation_tree {
        pubgrub::DerivationTree::External(external) => pubgrub::ReportFormatter::format_external(&formatter, external),
        pubgrub::DerivationTree::Derived(_) => todo!(),
    }
}

#[derive(Default)]
struct Reporter {
    packages: IndexMap<PackageKey, Manifest>,
    fallback: pubgrub::DefaultStringReportFormatter,
}

impl pubgrub::ReportFormatter<PackageKey, VersionSet, DependencyError> for Reporter {
    type Output = String;

    // Format an [External] incompatibility.
    fn format_external(&self, external: &pubgrub::External<PackageKey, VersionSet, DependencyError>) -> Self::Output {
        match external {
            pubgrub::External::FromDependencyOf(pkg_a, _set_a, pkg_b, set_b) => {
                let metadata_a = self.packages.get(pkg_a).expect("expected metadata of package");
                let metadata_b = self.packages.get(pkg_b).expect("expected metadata of package");

                let constraint_b = metadata_a
                    .dependencies
                    .get(metadata_b.package.name.get_ref())
                    .map(|dep| dep.required_version.clone())
                    .unwrap();

                if !set_b.versions.iter().any(|v| constraint_b.matches(v)) {
                    return format!(
                        "{} {} depends on {} {constraint_b}, but no version matches constraint",
                        metadata_a.package.name, metadata_a.package.version, metadata_b.package.name,
                    );
                }

                // Fallback to default behaviour
                external.to_string()
            }
            pubgrub::External::NoVersions(pkg, set) => format!("could not find versions {set} of {pkg}"),
            pubgrub::External::NotRoot(_, _) => unreachable!(),
            pubgrub::External::Custom(pkg, set, m) => match m {
                DependencyError::PackageFetchFailed { source } => {
                    format!("could not fetch dependency {source}, required by {pkg} {set}")
                }
                DependencyError::VersionsFetchFailed { package_name } => {
                    format!("could not retrieve versions for {package_name}, required by {pkg} {set}")
                }
                DependencyError::VersionNotFound => format!("could not find any matching version of {pkg}"),
            },
        }
    }

    /// Format terms of an incompatibility.
    fn format_terms(&self, terms: &pubgrub::Map<PackageKey, pubgrub::Term<VersionSet>>) -> Self::Output {
        pubgrub::ReportFormatter::<PackageKey, VersionSet, DependencyError>::format_terms(&self.fallback, terms)
    }

    /// Simplest case, we just combine two external incompatibilities.
    fn explain_both_external(
        &self,
        external1: &pubgrub::External<PackageKey, VersionSet, DependencyError>,
        external2: &pubgrub::External<PackageKey, VersionSet, DependencyError>,
        current_terms: &pubgrub::Map<PackageKey, pubgrub::Term<VersionSet>>,
    ) -> Self::Output {
        self.fallback.explain_both_external(external1, external2, current_terms)
    }

    /// Both causes have already been explained so we use their refs.
    fn explain_both_ref(
        &self,
        ref_id1: usize,
        derived1: &pubgrub::Derived<PackageKey, VersionSet, DependencyError>,
        ref_id2: usize,
        derived2: &pubgrub::Derived<PackageKey, VersionSet, DependencyError>,
        current_terms: &pubgrub::Map<PackageKey, pubgrub::Term<VersionSet>>,
    ) -> Self::Output {
        self.fallback
            .explain_both_ref(ref_id1, derived1, ref_id2, derived2, current_terms)
    }

    /// One cause is derived (already explained so one-line),
    /// the other is a one-line external cause,
    /// and finally we conclude with the current incompatibility.
    fn explain_ref_and_external(
        &self,
        ref_id: usize,
        derived: &pubgrub::Derived<PackageKey, VersionSet, DependencyError>,
        external: &pubgrub::External<PackageKey, VersionSet, DependencyError>,
        current_terms: &pubgrub::Map<PackageKey, pubgrub::Term<VersionSet>>,
    ) -> Self::Output {
        self.fallback
            .explain_ref_and_external(ref_id, derived, external, current_terms)
    }

    /// Add an external cause to the chain of explanations.
    fn and_explain_external(
        &self,
        external: &pubgrub::External<PackageKey, VersionSet, DependencyError>,
        current_terms: &pubgrub::Map<PackageKey, pubgrub::Term<VersionSet>>,
    ) -> Self::Output {
        self.fallback.and_explain_external(external, current_terms)
    }

    /// Add an already explained incompat to the chain of explanations.
    fn and_explain_ref(
        &self,
        ref_id: usize,
        derived: &pubgrub::Derived<PackageKey, VersionSet, DependencyError>,
        current_terms: &pubgrub::Map<PackageKey, pubgrub::Term<VersionSet>>,
    ) -> Self::Output {
        self.fallback.and_explain_ref(ref_id, derived, current_terms)
    }

    /// Add an already explained incompat to the chain of explanations.
    fn and_explain_prior_and_external(
        &self,
        prior_external: &pubgrub::External<PackageKey, VersionSet, DependencyError>,
        external: &pubgrub::External<PackageKey, VersionSet, DependencyError>,
        current_terms: &pubgrub::Map<PackageKey, pubgrub::Term<VersionSet>>,
    ) -> Self::Output {
        self.fallback
            .and_explain_prior_and_external(prior_external, external, current_terms)
    }
}
