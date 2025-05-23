use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::parser::{Block, Parser, Property, Value};
use crate::{Package, Project, Spanned, errors::*};

use error_snippet::Result;
use lume_errors::DiagCtxHandle;
use lume_span::{PackageId, SourceFile};
use semver::{Version, VersionReq};

pub const DEFAULT_ARCFILE: &str = "Arcfile";

pub(crate) struct ProjectParser {
    /// Absolute path to the project's Arcfile.
    path: PathBuf,

    /// Handle to the diagnostics context which will handle parsing errors.
    dcx: DiagCtxHandle,

    /// Source file of the project's Arcfile.
    source: Arc<SourceFile>,

    /// Defines the parsed blocks within the source file.
    blocks: Vec<Block>,

    /// Defines the current Lume version.
    current_lume_version: Version,
}

impl ProjectParser {
    /// Creates a new [`ProjectParser`] instance using the given `Arcfile` source file path.
    ///
    /// # Errors
    ///
    /// This method may fail if the given path is unreadable or
    /// otherwise inaccessible.
    fn new(path: &Path, dcx: DiagCtxHandle) -> Result<Self> {
        let content = match std::fs::read_to_string(path) {
            Ok(content) => content,
            Err(err) => {
                return Err(ArcfileIoError {
                    inner: vec![err.into()],
                }
                .into());
            }
        };

        let file_name: String = match path.file_name() {
            Some(os_str) => os_str.to_string_lossy().into_owned(),
            None => DEFAULT_ARCFILE.into(),
        };

        let source = SourceFile::new(PackageId::empty(), file_name, content);

        Self::from_source(path, Arc::new(source), dcx)
    }

    /// Creates a new [`ProjectParser`] instance using the given `Arcfile` source file.
    ///
    /// # Errors
    ///
    /// This method may fail if the given `Arcfile` is improperly formatted or
    /// otherwise invalid.
    pub fn from_source(path: &Path, source: Arc<SourceFile>, mut dcx: DiagCtxHandle) -> Result<Self> {
        let blocks = dcx.with(|handle| Parser::new(source.clone(), handle).parse())?;

        Ok(Self {
            path: path.to_path_buf(),
            source,
            dcx,
            blocks,
            current_lume_version: Self::current_lume_version(),
        })
    }

    /// Creates a new [`ProjectParser`] instance by locating the `Arcfile` in the given root directory.
    ///
    /// # Errors
    ///
    /// This method may fail if:
    /// - the given path has no `Arcfile` stored within it,
    /// - the located `Arcfile` doesn't refer to a file
    /// - or if the given `Arcfile` is otherwise invalid
    pub fn locate(root: &Path, dcx: DiagCtxHandle) -> Result<Project> {
        let path = root.join(DEFAULT_ARCFILE);
        if !path.is_file() {
            return Err(ArcfileMissing {
                dir: root.to_path_buf(),
            }
            .into());
        }

        ProjectParser::new(&path, dcx)?.parse()
    }

    /// Parses a [`Project`] from the input source file.
    fn parse(&mut self) -> Result<Project> {
        let project = Project {
            path: self.path.clone(),
            packages: self.parse_packages()?,
        };

        Ok(project)
    }

    /// Parses a list of zero-or-more [`Package`] instances from the input source code.
    fn parse_packages(&mut self) -> Result<Vec<Package>> {
        let mut packages = Vec::new();

        for block in &self.blocks {
            if block.ty.value() != "Package" {
                self.dcx.emit(
                    ArcfileUnknownItem {
                        source: block.location.source.clone(),
                        range: block.ty.span().clone(),
                        name: block.ty.value().clone(),
                    }
                    .into(),
                );

                continue;
            }

            packages.push(self.parse_package(block)?);
        }

        if packages.is_empty() {
            return Err(ArcfileNoPackages {
                path: self.path.to_string_lossy().into_owned(),
            }
            .into());
        }

        Ok(packages)
    }

    /// Parses a single [`Package`] instance from the given [`Block`].
    fn parse_package(&self, block: &Block) -> Result<Package> {
        let name = match block.arguments.first() {
            Some(name) => match name {
                Value::String(str, _) => str.clone(),
                value => {
                    return Err(ArcfileUnexpectedType {
                        source: block.location.source.clone(),
                        range: value.location().range.clone(),
                        expected: String::from("String"),
                        found: value.to_string(),
                    }
                    .into());
                }
            },
            None => {
                return Err(ArcfileMissingName {
                    source: block.location.source.clone(),
                    range: block.location.range.clone(),
                }
                .into());
            }
        };

        let version = match block.find_prop("version") {
            Some(prop) => Some(self.version(prop)?),
            None => None,
        };

        let lume_version = match block.find_prop("lume_version") {
            Some(prop) => self.version_req(prop)?,
            None => {
                return Err(ArcfileMissingLumeVersion {
                    source: block.location.source.clone(),
                    range: block.location.range.clone(),
                }
                .into());
            }
        };

        let description = self.opt_string_prop(block, "description")?;
        let license = self.opt_string_prop(block, "license")?;
        let repository = self.opt_string_prop(block, "repository")?;

        let package = Package {
            id: PackageId::from_name(&name),
            path: self.path.parent().unwrap().to_path_buf(),
            name,
            lume_version,
            version,
            description,
            license,
            repository,
        };

        self.verify_lume_version(&package)?;

        Ok(package)
    }

    /// Gets the SemVer-version requirement from the given [`Property`].
    fn version_req(&self, prop: &Property) -> Result<Spanned<VersionReq>> {
        let version_str = self.expect_string(&prop.value)?;
        let location = prop.value.location().clone();

        match VersionReq::parse(&version_str) {
            Ok(version) => Ok(Spanned::new(version, location.range)),
            Err(_) => Err(ArcfileInvalidVersion {
                source: self.source.clone(),
                range: location.range,
                field: prop.name.value().clone(),
                version: version_str.to_string(),
            }
            .into()),
        }
    }

    /// Gets the SemVer-version from the given [`Property`].
    fn version(&self, prop: &Property) -> Result<Spanned<Version>> {
        let version_str = self.expect_string(&prop.value)?;
        let location = prop.value.location().clone();

        match Version::parse(&version_str) {
            Ok(version) => Ok(Spanned::new(version, location.range)),
            Err(_) => Err(ArcfileInvalidVersion {
                source: self.source.clone(),
                range: location.range,
                field: prop.name.value().clone(),
                version: version_str.to_string(),
            }
            .into()),
        }
    }

    /// Verifies that the current Lume compiler version is compatible
    /// with the one required by the [`Package`].
    fn verify_lume_version(&self, package: &Package) -> Result<()> {
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
    fn current_lume_version() -> Version {
        let version_str = env!("CARGO_PKG_VERSION");

        match Version::parse(version_str) {
            Ok(version) => version,
            Err(err) => panic!("Invalid Lume compiler version `{version_str}`: {err}"),
        }
    }

    /// Expects the given [`Block`] to have a [`Property`] with the given name.
    fn opt_string_prop(&self, block: &Block, name: &'static str) -> Result<Option<String>> {
        match block.find_prop(name) {
            Some(prop) => Ok(Some(self.expect_prop_string(prop)?)),
            None => Ok(None),
        }
    }

    /// Expects the value of the given [`Property`] to be of type [`Value::String`].
    fn expect_prop_string(&self, prop: &Property) -> Result<String> {
        match &prop.value {
            Value::String(str, _) => Ok(str.clone()),
            kind => Err(ArcfileUnexpectedType {
                source: self.source.clone(),
                range: prop.location.range.clone(),
                expected: String::from("String"),
                found: kind.to_string(),
            }
            .into()),
        }
    }

    /// Expects the given [`Value`] to be of type [`Value::String`].
    fn expect_string(&self, value: &Value) -> Result<String> {
        match &value {
            Value::String(str, _) => Ok(str.clone()),
            kind => Err(ArcfileUnexpectedType {
                source: self.source.clone(),
                range: value.location().range.clone(),
                expected: String::from("String"),
                found: kind.to_string(),
            }
            .into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use error_snippet::Error;

    #[track_caller]
    fn parser(input: &str) -> ProjectParser {
        let path = Path::new("<test>");
        let source = SourceFile::internal(input.to_string());
        let dcx = DiagCtxHandle::shim();

        ProjectParser::from_source(path, Arc::new(source), dcx).unwrap()
    }

    #[track_caller]
    fn parse(input: &str) -> Project {
        parser(input).parse().unwrap()
    }

    #[track_caller]
    fn parse_err(input: &str) -> Error {
        parser(input).parse().unwrap_err()
    }

    macro_rules! assert_snap {
        ($input: expr) => {
            insta::assert_debug_snapshot!($input);
        };
    }

    macro_rules! assert_snap_eq {
        ($input: expr) => {
            let parsed = parse($input);

            insta::with_settings!({
                description => $input,
                omit_expression => true
            }, {
                insta::assert_debug_snapshot!(parsed);
            });
        };
    }

    macro_rules! assert_err_snap_eq {
        ($input: expr) => {
            let parsed = parse_err($input);

            insta::with_settings!({
                description => $input,
                omit_expression => true
            }, {
                insta::assert_debug_snapshot!(parsed);
            });
        };
    }

    #[test]
    fn test_empty_file() {
        assert_err_snap_eq!("");
    }

    #[test]
    fn test_without_name() {
        assert_err_snap_eq!("Package {}");
    }

    #[test]
    fn test_without_lume_version() {
        assert_err_snap_eq!("Package \"sample\" {}");
    }

    #[test]
    fn test_with_unexpected_name_type() {
        assert_err_snap_eq!("Package 123 {}");
    }

    #[test]
    fn test_with_unexpected_lume_version_type() {
        assert_err_snap_eq!(
            "Package \"sample\" {
                lume_version = 1
            }"
        );
    }

    #[test]
    fn test_invalid_version_string() {
        assert_err_snap_eq!(
            "Package \"sample\" {
                lume_version = \"^1-1\"
            }"
        );
    }

    #[test]
    fn test_name() {
        assert_snap_eq!(
            "Package \"sample\" {
                lume_version = \"^0\"
            }"
        );
    }

    #[test]
    fn test_description() {
        assert_snap_eq!(
            "Package \"sample\" {
                lume_version = \"^0\"
                description = \"Some description\"
            }"
        );
    }

    #[test]
    fn test_version() {
        assert_snap_eq!(
            "Package \"sample\" {
                lume_version = \"^0\"
                version = \"1.0.0\"
            }"
        );
    }

    #[test]
    fn test_incompatible_version() {
        let mut parser = parser(
            "Package \"sample\" {
                version = \"1.0.0\"
                lume_version = \"^2\"
            }",
        );

        parser.current_lume_version = Version::new(1, 0, 0);

        assert_snap!(parser.parse().unwrap_err());
    }

    #[test]
    fn test_prerelease_lume_version_success() {
        let mut parser = parser(
            "Package \"sample\" {
                version = \"1.0.0\"
                lume_version = \"1.0.0-rc3\"
            }",
        );

        parser.current_lume_version = Version::parse("1.0.0-rc3").unwrap();

        assert_snap!(parser.parse().unwrap());
    }

    #[test]
    fn test_prerelease_lume_version_stable() {
        let mut parser = parser(
            "Package \"sample\" {
                version = \"1.0.0\"
                lume_version = \"1.0.0-rc3\"
            }",
        );

        parser.current_lume_version = Version::parse("1.0.0").unwrap();

        assert_snap!(parser.parse().unwrap());
    }

    #[test]
    fn test_prerelease_lume_version_failure() {
        let mut parser = parser(
            "Package \"sample\" {
                version = \"1.0.0\"
                lume_version = \"1.0.0-rc3\"
            }",
        );

        parser.current_lume_version = Version::parse("1.0.0-rc2").unwrap();

        assert_snap!(parser.parse().unwrap_err());
    }

    #[test]
    fn test_multiple_packages() {
        assert_snap_eq!(
            "Package \"package01\" { lume_version = \"^0\" }
            Package \"package02\" { lume_version = \"^0\" }
            Package \"package03\" { lume_version = \"^0\" }"
        );
    }
}
