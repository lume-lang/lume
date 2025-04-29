pub mod errors;
pub(crate) mod parser;

use crate::errors::ArcfileGlobError;
use crate::parser::ProjectParser;

use glob::glob;
use lume_diag::Result;
use lume_span::PackageId;
use semver::{Version, VersionReq};
use std::path::{Path, PathBuf};

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    value: T,
    span: std::ops::Range<usize>,
}

impl<T> Spanned<T> {
    /// Creates a new spanned value.
    pub fn new(value: T, span: std::ops::Range<usize>) -> Spanned<T> {
        Self { value, span }
    }

    /// Gets a reference to the value held by the span.
    pub fn value(&self) -> &T {
        &self.value
    }

    /// Moves the span into the held value.
    pub fn into_value(self) -> T {
        self.value
    }

    /// Gets a reference to the value held by the span.
    pub fn span(&self) -> &std::ops::Range<usize> {
        &self.span
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Project {
    /// Uniquely identifies the project.
    pub id: PackageId,

    /// Defines the path to the Arcfile.
    pub path: PathBuf,

    /// Defines the name of the package.
    pub name: String,

    /// Defines the minimum required version of Lume.
    pub lume_version: Spanned<VersionReq>,

    /// Defines the current version of the package.
    pub version: Option<Spanned<Version>>,

    /// Defines an optional description of the package.
    pub description: Option<String>,
}

impl Project {
    /// Locates the project in the given root directory.
    pub fn locate(root: &Path) -> Result<Project> {
        ProjectParser::locate(root)
    }

    /// Gets the absolute path to the project directory.
    ///
    /// The project directory is the parent directory of the Arcfile.
    pub fn root(&self) -> &Path {
        self.path.parent().unwrap()
    }

    /// Gets the relative path to a file within the project directory.
    pub fn relative_source_path<'a>(&'a self, file: &'a Path) -> &'a Path {
        let root = self.root();

        if file.is_absolute() && file.starts_with(root) {
            file.strip_prefix(root).unwrap()
        } else {
            file
        }
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
