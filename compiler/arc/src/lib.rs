pub mod errors;
pub(crate) mod parser;

use crate::errors::*;

use fxhash::hash64;
use glob::glob;
use lume_diag::Result;
use parser::ProjectParser;
use semver::{Version, VersionReq};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub struct ProjectId(pub u64);

impl From<String> for ProjectId {
    /// Creates a new [`ProjectId`] from a string, by taking it's hash value.
    fn from(value: String) -> ProjectId {
        let hash = hash64(value.as_bytes());

        ProjectId(hash)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Project {
    /// Uniquely identifies the project.
    pub id: ProjectId,

    /// Defines the path to the Arcfile.
    pub path: PathBuf,

    /// Defines the name of the package.
    pub name: String,

    /// Defines the minimum required version of Lume.
    pub lume_version: VersionReq,

    /// Defines the current version of the package.
    pub version: Option<Version>,

    /// Defines an optional description of the package.
    pub description: Option<String>,
}

impl Project {
    /// Locates the project in the given root directory.
    pub fn locate(root: &Path) -> Result<Project> {
        ProjectParser::locate(root)
    }

    /// Attempts to find all the Lume source files within the project.
    ///
    /// By default, it will only find source files with the `.lm` file extension. If any files are explicitly
    /// excluded from within the Arcfile, they will be ignored.
    pub fn files(&self) -> Result<Vec<PathBuf>> {
        let root_directory = match self.path.parent() {
            Some(parent) => parent,
            None => panic!("Arcfile must be located within a directory"),
        };

        let glob_pattern = format!("{}/**/*.lm", root_directory.display());
        let mut matched_files = Vec::new();

        for file in glob(&glob_pattern).unwrap() {
            let file = match file {
                Ok(file) => file,
                Err(err) => return Err(ArcfileGlobError { inner: err }.into()),
            };

            matched_files.push(file);
        }

        Ok(matched_files)
    }
}
