use crate::{Project, ProjectId, errors::*};

use lume_diag::Result;
use lume_diag::source::{NamedSource, Source};
use semver::{Version, VersionReq};
use std::path::{Path, PathBuf};
use toml::{Table, Value};

pub const DEFAULT_ARCFILE: &str = "Arcfile";

pub(crate) struct ProjectParser {
    /// Absolute path to the project's Arcfile.
    path: PathBuf,

    /// Source file of the project's Arcfile.
    source: NamedSource,
}

impl ProjectParser {
    pub fn new(path: &Path) -> Result<Self> {
        let content = match std::fs::read_to_string(path) {
            Ok(content) => content,
            Err(err) => return Err(ArcfileIoError { inner: err }.into()),
        };

        let file_name: String = match path.file_name() {
            Some(os_str) => os_str.to_string_lossy().into_owned(),
            None => DEFAULT_ARCFILE.into(),
        };

        let source = NamedSource::new(file_name, content);

        Ok(Self {
            path: path.to_path_buf(),
            source,
        })
    }

    /// Creates a new `ProjectParser` instance by locating the Arcfile in the given root directory.
    pub fn locate(root: &Path) -> Result<Project> {
        let path = root.join(DEFAULT_ARCFILE);
        if !path.is_file() {
            return Err(ArcfileIoError {
                inner: std::io::Error::new(std::io::ErrorKind::NotFound, "Arcfile not found"),
            }
            .into());
        }

        ProjectParser::new(&path)?.parse()
    }

    /// Parses the project from the TOML table.
    fn parse(&self) -> Result<Project> {
        let table = match self.source.content().parse::<Table>() {
            Ok(table) => table,
            Err(err) => return Err(ArcfileTomlError { inner: err }.into()),
        };

        let package = self.required("package", self.section(&table, "package")?)?;

        let name = match self.required("name", self.string(&package, "name")?) {
            Ok(name) => name,
            Err(_) => {
                return Err(ArcfileMissingName {
                    source: self.source.clone(),
                    range: 0..0,
                }
                .into());
            }
        };

        let version = match self.required("version", self.version(&package, "version")?) {
            Ok(version) => version,
            Err(_) => {
                return Err(ArcfileMissingVersion {
                    source: self.source.clone(),
                    range: 0..0,
                }
                .into());
            }
        };

        let lume_version = match self.required("lume_version", self.version_req(&package, "lume_version")?) {
            Ok(version) => version,
            Err(_) => {
                return Err(ArcfileMissingLumeVersion {
                    source: self.source.clone(),
                    range: 0..0,
                }
                .into());
            }
        };

        let description = self.string(&package, "description")?;

        let current_lume_version = self.current_lume_version();
        if !lume_version.matches(&current_lume_version) {
            return Err(ArcfileIncompatibleLumeVersion {
                source: self.source.clone(),
                range: 0..0,
                current: current_lume_version.to_string(),
                required: lume_version.to_string(),
            }
            .into());
        }

        let id = ProjectId::from(name.clone());

        let project = Project {
            id,
            path: self.path.to_path_buf(),
            name,
            lume_version,
            version: Some(version),
            description,
        };

        Ok(project)
    }

    /// Gets the property value from the given table and asserts its type.
    ///
    /// If the property is not found within the table, returns `None`.
    fn property(&self, table: &Table, name: &str, property_type: &str) -> Result<Option<Value>> {
        let property: &toml::Value = match table.get(name) {
            Some(property) => property,
            None => return Ok(None),
        };

        if property.type_str() != property_type {
            return Err(self.invalid_property(name, property_type, property.type_str()).into());
        }

        Ok(Some(property.clone()))
    }

    /// Calls the given callback and returns it's result if given. If the callback returns `None`, returns an error.
    fn required<T>(&self, name: &str, value: Option<T>) -> Result<T> {
        match value {
            Some(property) => Ok(property),
            None => Err(self.missing_section(name).into()),
        }
    }

    /// Gets the section from the given table.
    fn section(&self, table: &Table, name: &str) -> Result<Option<Table>> {
        let property = match self.property(table, name, "table")? {
            Some(property) => property,
            None => return Ok(None),
        };

        let table = property.as_table().unwrap().clone();

        Ok(Some(table))
    }

    /// Gets the string from the given table.
    fn string(&self, table: &Table, name: &str) -> Result<Option<String>> {
        let prop = match self.property(table, name, "string")? {
            Some(prop) => prop,
            None => return Ok(None),
        };

        let content: &str = prop.as_str().unwrap();

        Ok(Some(content.to_string()))
    }

    /// Gets the SemVer-version requirement from the given table property.
    fn version_req(&self, table: &Table, name: &str) -> Result<Option<VersionReq>> {
        let version_str = match self.string(table, name)? {
            Some(prop) => prop,
            None => return Ok(None),
        };

        let version = match VersionReq::parse(version_str.as_ref()) {
            Ok(version) => version,
            Err(_) => {
                return Err(ArcfileInvalidVersion {
                    source: self.source.clone(),
                    range: 0..0,
                    field: name.to_string(),
                    version: version_str,
                }
                .into());
            }
        };

        Ok(Some(version))
    }

    /// Gets the SemVer-version from the given table property.
    fn version(&self, table: &Table, name: &str) -> Result<Option<Version>> {
        let version_str = match self.string(table, name)? {
            Some(prop) => prop,
            None => return Ok(None),
        };

        let version = match Version::parse(version_str.as_ref()) {
            Ok(version) => version,
            Err(_) => {
                return Err(ArcfileInvalidVersion {
                    source: self.source.clone(),
                    range: 0..0,
                    field: name.to_string(),
                    version: version_str,
                }
                .into());
            }
        };

        Ok(Some(version))
    }

    fn missing_section(&self, section: &str) -> ArcfileMissingSection {
        ArcfileMissingSection { name: section.into() }
    }

    fn invalid_property(&self, property: &str, expected: &str, actual: &str) -> ArcfileInvalidPropertyType {
        ArcfileInvalidPropertyType {
            name: property.into(),
            expected: expected.into(),
            actual: actual.into(),
        }
    }

    /// Gets the current Lume compiler version.
    fn current_lume_version(&self) -> Version {
        let version_str = env!("CARGO_PKG_VERSION");

        match Version::parse(version_str) {
            Ok(version) => version,
            Err(_) => panic!("Invalid Lume compiler version: {}", version_str),
        }
    }
}
