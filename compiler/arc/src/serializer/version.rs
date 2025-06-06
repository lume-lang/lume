use error_snippet::Result;
use semver::{Version, VersionReq};

use crate::Package;
use crate::ProjectParser;
use crate::errors::*;
use crate::parser::{Property, Spanned};

impl ProjectParser {
    /// Gets the SemVer-version requirement from the given [`Property`].
    pub(crate) fn version_req(&self, prop: &Property) -> Result<Spanned<VersionReq>> {
        let version_str = self.expect_prop_string(prop)?.to_owned();
        let location = prop.value.location().clone();

        match VersionReq::parse(&version_str) {
            Ok(version) => Ok(Spanned::new(version, location)),
            Err(_) => Err(ArcfileInvalidVersion {
                source: self.source.clone(),
                range: location,
                field: prop.name.value().clone(),
                version: version_str.to_string(),
            }
            .into()),
        }
    }

    /// Gets the SemVer-version from the given [`Property`].
    pub(crate) fn version(&self, prop: &Property) -> Result<Spanned<Version>> {
        let version_str = self.expect_prop_string(prop)?.to_owned();
        let location = prop.value.location().clone();

        match Version::parse(&version_str) {
            Ok(version) => Ok(Spanned::new(version, location)),
            Err(_) => Err(ArcfileInvalidVersion {
                source: self.source.clone(),
                range: location,
                field: prop.name.value().clone(),
                version: version_str.to_string(),
            }
            .into()),
        }
    }

    /// Verifies that the current Lume compiler version is compatible
    /// with the one required by the [`Package`].
    pub(crate) fn verify_lume_version(&self, package: &Package) -> Result<()> {
        let required_lume_version = package.lume_version.clone();
        let current_lume_version = self.current_lume_version.clone();

        if !required_lume_version.value().matches(&current_lume_version) {
            return Err(ArcfileIncompatibleLumeVersion {
                source: self.source.clone(),
                range: required_lume_version.span().clone(),
                current: current_lume_version,
                required: required_lume_version.into_value(),
            }
            .into());
        }

        Ok(())
    }

    /// Gets the current Lume compiler version.
    pub(crate) fn current_lume_version() -> Version {
        let version_str = env!("CARGO_PKG_VERSION");

        match Version::parse(version_str) {
            Ok(version) => version,
            Err(err) => panic!("Invalid Lume compiler version `{version_str}`: {err}"),
        }
    }
}
