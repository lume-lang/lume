use crate::{Project, Spanned, errors::*};

use error_snippet::{IntoDiagnostic, Result};
use lume_span::{PackageId, SourceFile};
use semver::{Version, VersionReq};
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

pub const DEFAULT_ARCFILE: &str = "Arcfile";

pub(crate) struct ProjectParser {
    /// Absolute path to the project's Arcfile.
    path: PathBuf,

    /// Source file of the project's Arcfile.
    source: Arc<SourceFile>,

    /// Represents the parsed TOML document.
    document: toml_edit::ImDocument<String>,

    /// Defines the current Lume version.
    current_lume_version: Version,
}

impl ProjectParser {
    pub fn new(path: &Path) -> Result<Self> {
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

        Self::from_source(path, Arc::new(source))
    }

    pub fn from_source(path: &Path, source: Arc<SourceFile>) -> Result<Self> {
        let document = match toml_edit::ImDocument::parse(source.content.clone()) {
            Ok(doc) => doc,
            Err(err) => {
                return Err(ArcfileTomlError {
                    inner: vec![err.into_diagnostic()],
                }
                .into());
            }
        };

        Ok(Self {
            path: path.to_path_buf(),
            source,
            document,
            current_lume_version: Self::current_lume_version(),
        })
    }

    /// Creates a new `ProjectParser` instance by locating the Arcfile in the given root directory.
    pub fn locate(root: &Path) -> Result<Project> {
        let path = root.join(DEFAULT_ARCFILE);
        if !path.is_file() {
            return Err(ArcfileMissing {
                dir: root.to_path_buf(),
            }
            .into());
        }

        ProjectParser::new(&path)?.parse()
    }

    /// Parses the project from the TOML table.
    fn parse(&self) -> Result<Project> {
        let mut project = Project::default();

        self.parse_package(&mut project)?;

        project.id = PackageId::new(project.name.as_str());
        self.path.clone_into(&mut project.path);

        Ok(project)
    }

    /// Gets the section from the given table.
    fn parse_package(&self, project: &mut Project) -> Result<()> {
        let Some(section) = self.section(&self.document, "package")? else {
            return Err(ArcfileMissingSection {
                name: "package".to_string(),
            }
            .into());
        };

        project.name = match self.string(section, "name")? {
            Some(sec) => sec.value().clone(),
            None => {
                return Err(ArcfileMissingName {
                    source: self.source.clone(),
                    range: section.span().unwrap(),
                }
                .into());
            }
        };

        project.version = self.version(section, "version")?;

        project.lume_version = match self.version_req(section, "lume_version")? {
            Some(sec) => sec,
            None => {
                return Err(ArcfileMissingLumeVersion {
                    source: self.source.clone(),
                    range: section.span().unwrap(),
                }
                .into());
            }
        };

        project.description = self.string(section, "description")?.map(|s| s.value().clone());

        self.verify_lume_version(project)?;

        Ok(())
    }

    /// Gets the section from the given table.
    fn section<'a>(&'a self, table: &'a toml_edit::Table, name: &str) -> Result<Option<&'a toml_edit::Table>> {
        let Some(property) = table.get(name) else {
            return Ok(None);
        };

        match &property {
            toml_edit::Item::Table(table) => Ok(Some(table)),
            item => Err(self.unexpected_type(name, "table", item).into()),
        }
    }

    /// Gets the string from the given table.
    fn string<'a>(
        &'a self,
        table: &'a toml_edit::Table,
        name: &str,
    ) -> Result<Option<&'a toml_edit::Formatted<String>>> {
        let Some(property) = table.get(name) else {
            return Ok(None);
        };

        match &property {
            toml_edit::Item::Value(toml_edit::Value::String(val)) => Ok(Some(val)),
            item => Err(self.unexpected_type(name, "string", item).into()),
        }
    }

    /// Gets the SemVer-version requirement from the given table property.
    fn version_req(&self, table: &toml_edit::Table, name: &str) -> Result<Option<Spanned<VersionReq>>> {
        let Some(version_str) = self.string(table, name)? else {
            return Ok(None);
        };

        match VersionReq::parse(version_str.value()) {
            Ok(version) => {
                let span = Spanned::new(version, version_str.span().unwrap());

                Ok(Some(span))
            }
            Err(_) => Err(self.invalid_version(name, version_str).into()),
        }
    }

    /// Gets the SemVer-version from the given table property.
    fn version(&self, table: &toml_edit::Table, name: &str) -> Result<Option<Spanned<Version>>> {
        let Some(version_str) = self.string(table, name)? else {
            return Ok(None);
        };

        match Version::parse(version_str.value()) {
            Ok(version) => {
                let span = Spanned::new(version, version_str.span().unwrap());

                Ok(Some(span))
            }
            Err(_) => Err(self.invalid_version(name, version_str).into()),
        }
    }

    fn unexpected_type(&self, name: &str, expected: &str, found: &toml_edit::Item) -> ArcfileUnexpectedType {
        ArcfileUnexpectedType {
            source: self.source.clone(),
            range: found.span().unwrap(),
            name: name.to_string(),
            expected: expected.to_string(),
            found: found.type_name(),
        }
    }

    fn invalid_version(&self, field: &str, value: &toml_edit::Formatted<String>) -> ArcfileInvalidVersion {
        ArcfileInvalidVersion {
            source: self.source.clone(),
            range: value.span().unwrap(),
            field: field.to_string(),
            version: value.to_string(),
        }
    }

    /// Verifies that the current Lume compiler version is compatible
    /// with the one required by the project.
    fn verify_lume_version(&self, project: &Project) -> Result<()> {
        let required_lume_version = project.lume_version.clone();
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use error_snippet::Error;

    fn parser(input: &str) -> ProjectParser {
        let source = SourceFile::internal(input.to_string());

        ProjectParser::from_source(Path::new("<test>"), Arc::new(source)).unwrap()
    }

    fn parse(input: &str) -> Project {
        parser(input).parse().unwrap()
    }

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
            assert_snap!(parse($input));
        };
    }

    macro_rules! assert_err_snap_eq {
        ($input: expr) => {
            assert_snap!(parse_err($input));
        };
    }

    #[test]
    fn test_empty_file() {
        assert_err_snap_eq!("");
    }

    #[test]
    fn test_without_package() {
        assert_err_snap_eq!("name = 'Test'");
    }

    #[test]
    fn test_without_name() {
        assert_err_snap_eq!(
            "[package]
            lume_version = '^0'"
        );
    }

    #[test]
    fn test_without_lume_version() {
        assert_err_snap_eq!(
            "[package]
            name = 'sample'"
        );
    }

    #[test]
    fn test_with_unexpected_package_type() {
        assert_err_snap_eq!("package = 'sample'");
    }

    #[test]
    fn test_with_unexpected_name_type() {
        assert_err_snap_eq!(
            "[package]
            name = 1"
        );
    }

    #[test]
    fn test_with_unexpected_lume_version_type() {
        assert_err_snap_eq!(
            "[package]
            name = 'sample'
            lume_version = 1"
        );
    }

    #[test]
    fn test_invalid_version_string() {
        assert_err_snap_eq!(
            "[package]
            name = 'sample'
            lume_version = '^1-1'"
        );
    }

    #[test]
    fn test_name() {
        assert_snap_eq!(
            "[package]
            name = 'some-package'
            lume_version = '^0'"
        );
    }

    #[test]
    fn test_description() {
        assert_snap_eq!(
            "[package]
            name = 'sample'
            lume_version = '^0'
            description = 'Some description'"
        );
    }

    #[test]
    fn test_version() {
        assert_snap_eq!(
            "[package]
            name = 'some-package'
            version = '1.0.0'
            lume_version = '^0'"
        );
    }

    #[test]
    fn test_incompatible_version() {
        let mut parser = parser(
            "[package]
            name = 'some-package'
            version = '1.0.0'
            lume_version = '^2'",
        );

        parser.current_lume_version = Version::new(1, 0, 0);

        assert_snap!(parser.parse().unwrap_err());
    }

    #[test]
    fn test_prerelease_lume_version_success() {
        let mut parser = parser(
            "[package]
            name = 'some-package'
            version = '1.0.0'
            lume_version = '1.0.0-rc3'",
        );

        parser.current_lume_version = Version::parse("1.0.0-rc3").unwrap();

        assert_snap!(parser.parse().unwrap());
    }

    #[test]
    fn test_prerelease_lume_version_stable() {
        let mut parser = parser(
            "[package]
            name = 'some-package'
            version = '1.0.0'
            lume_version = '1.0.0-rc3'",
        );

        parser.current_lume_version = Version::parse("1.0.0").unwrap();

        assert_snap!(parser.parse().unwrap());
    }

    #[test]
    fn test_prerelease_lume_version_failure() {
        let mut parser = parser(
            "[package]
            name = 'some-package'
            version = '1.0.0'
            lume_version = '1.0.0-rc3'",
        );

        parser.current_lume_version = Version::parse("1.0.0-rc2").unwrap();

        assert_snap!(parser.parse().unwrap_err());
    }
}
