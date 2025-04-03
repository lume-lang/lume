pub mod errors;

use crate::errors::ArcError;
use errors::ArcfileErrorKind;

use glob::glob;
use miette::NamedSource;
use semver::{Version, VersionReq};
use serde::{Deserialize, Serialize};
use std::{
    fs,
    path::{Path, PathBuf},
};
use toml::{Table, Value};

pub const DEFAULT_ARCFILE: &str = "Arcfile";

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Project {
    /// Defines the path to the Arcfile.
    path: PathBuf,

    /// Defines the name of the package.
    name: String,

    /// Defines the minimum required version of Lume.
    lume_version: VersionReq,

    /// Defines the current version of the package.
    version: Option<Version>,

    /// Defines an optional description of the package.
    description: Option<String>,
}

impl Project {
    /// Locates the project in the given root directory.
    pub fn locate(root: &Path) -> Result<Project, ArcError> {
        ProjectParser::locate(root)
    }

    /// Attempts to find all the Lume source files within the project.
    ///
    /// By default, it will only find source files with the `.lm` file extension. If any files are explicitly
    /// excluded from within the Arcfile, they will be ignored.
    pub fn files(&self) -> Result<Vec<PathBuf>, ArcError> {
        let root_directory = match self.path.parent() {
            Some(parent) => parent,
            None => panic!("Arcfile must be located within a directory"),
        };

        let glob_pattern = format!("{}/**/*.lm", root_directory.display());
        let mut matched_files = Vec::new();

        for file in glob(&glob_pattern).unwrap() {
            let file = match file {
                Ok(file) => file,
                Err(err) => return Err(ArcError::GlobError(err)),
            };

            matched_files.push(file);
        }

        Ok(matched_files)
    }
}

struct ProjectParser {
    /// Absolute path to the project's Arcfile.
    path: PathBuf,

    /// Source file of the project's Arcfile.
    source: NamedSource<String>,
}

impl ProjectParser {
    pub fn new(path: &Path) -> Result<Self, ArcError> {
        let content = match fs::read_to_string(path) {
            Ok(content) => content,
            Err(err) => return Err(ArcError::IoError(err)),
        };

        let file_name: String = match path.file_name() {
            Some(os_str) => os_str.to_string_lossy().into_owned(),
            None => "Arcfile".into(),
        };

        let source = NamedSource::new(file_name, content);

        Ok(Self {
            path: path.to_path_buf(),
            source,
        })
    }

    /// Creates a new `ProjectParser` instance by locating the Arcfile in the given root directory.
    pub fn locate(root: &Path) -> Result<Project, ArcError> {
        let path = root.join(DEFAULT_ARCFILE);
        if !path.is_file() {
            return Err(ArcError::IoError(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "Arcfile not found",
            )));
        }

        ProjectParser::new(&path)?.parse()
    }

    /// Parses the project from the TOML table.
    fn parse(&self) -> Result<Project, ArcError> {
        let table = match self.source.inner().parse::<Table>() {
            Ok(table) => table,
            Err(err) => return Err(ArcError::TomlError(err)),
        };

        let package = self.required("package", self.section(&table, "package")?)?;

        let name = self.required_or(self.string(&package, "name")?, ArcfileErrorKind::MissingName)?;
        let version = self.required_or(self.version(&package, "version")?, ArcfileErrorKind::MissingVersion)?;
        let lume_version = self.required_or(
            self.version_req(&package, "lume_version")?,
            ArcfileErrorKind::MissingLumeVersion,
        )?;

        let description = self.string(&package, "description")?;

        let current_lume_version = self.current_lume_version();
        if !lume_version.matches(&current_lume_version) {
            return Err(self.err(ArcfileErrorKind::IncompatibleLumeVersion(
                lume_version.to_string(),
                current_lume_version.to_string(),
            )));
        }

        let project = Project {
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
    fn property(&self, table: &Table, name: &str, property_type: &str) -> Result<Option<Value>, ArcError> {
        let property: &toml::Value = match table.get(name) {
            Some(property) => property,
            None => return Ok(None),
        };

        if property.type_str() != property_type {
            return Err(self.invalid_property(name, property_type, property.type_str()));
        }

        Ok(Some(property.clone()))
    }

    /// Calls the given callback and returns it's result if given. If the callback returns `None`, returns an error.
    fn required<T>(&self, name: &str, value: Option<T>) -> Result<T, ArcError> {
        match value {
            Some(property) => Ok(property),
            None => Err(self.missing_section(name)),
        }
    }

    /// Calls the given callback and returns it's result if given. If the callback returns `None`, returns an error.
    fn required_or<T>(&self, value: Option<T>, error: ArcfileErrorKind) -> Result<T, ArcError> {
        match value {
            Some(property) => Ok(property),
            None => Err(self.err(error)),
        }
    }

    /// Gets the section from the given table.
    fn section(&self, table: &Table, name: &str) -> Result<Option<Table>, ArcError> {
        let property = match self.property(table, name, "table")? {
            Some(property) => property,
            None => return Ok(None),
        };

        let table = property.as_table().unwrap().clone();

        Ok(Some(table))
    }

    /// Gets the string from the given table.
    fn string(&self, table: &Table, name: &str) -> Result<Option<String>, ArcError> {
        let prop = match self.property(table, name, "string")? {
            Some(prop) => prop,
            None => return Ok(None),
        };

        let content: &str = prop.as_str().unwrap();

        Ok(Some(content.to_string()))
    }

    /// Gets the SemVer-version requirement from the given table property.
    fn version_req(&self, table: &Table, name: &str) -> Result<Option<VersionReq>, ArcError> {
        let version_str = match self.string(table, name)? {
            Some(prop) => prop,
            None => return Ok(None),
        };

        let version = match VersionReq::parse(version_str.as_ref()) {
            Ok(version) => version,
            Err(_) => {
                return Err(self.err(ArcfileErrorKind::InvalidVersion(version_str, name.into())));
            }
        };

        Ok(Some(version))
    }

    /// Gets the SemVer-version from the given table property.
    fn version(&self, table: &Table, name: &str) -> Result<Option<Version>, ArcError> {
        let version_str = match self.string(table, name)? {
            Some(prop) => prop,
            None => return Ok(None),
        };

        let version = match Version::parse(version_str.as_ref()) {
            Ok(version) => version,
            Err(_) => {
                return Err(self.err(ArcfileErrorKind::InvalidVersion(version_str, name.into())));
            }
        };

        Ok(Some(version))
    }

    fn missing_section(&self, section: &str) -> ArcError {
        self.err(ArcfileErrorKind::MissingSection(section.into()))
    }

    fn invalid_property(&self, property: &str, expected: &str, actual: &str) -> ArcError {
        self.err(ArcfileErrorKind::InvalidPropertyType(
            property.into(),
            expected.into(),
            actual.into(),
        ))
    }

    fn err(&self, kind: ArcfileErrorKind) -> ArcError {
        ArcError::ArcfileError(kind)
    }

    /// Gets the current Lume compiler version.
    fn current_lume_version(&self) -> Version {
        let version_str = env!("CARGO_PKG_VERSION");

        let version = match Version::parse(version_str.as_ref()) {
            Ok(version) => version,
            Err(_) => panic!("Invalid Lume compiler version: {}", version_str),
        };

        version
    }
}
