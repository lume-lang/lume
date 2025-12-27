use std::path::PathBuf;
use std::sync::Arc;

use lume_session::Package;
use lume_span::{PackageId, SourceFile};

pub struct PackageBuilder {
    package: Package,
}

impl PackageBuilder {
    /// Create a new package builder with the given name.
    pub fn new(name: &str) -> Self {
        assert!(!name.is_empty(), "package name must be non-empty");

        let id = PackageId::from_name(name);
        let package = Package {
            id,
            name: name.to_string(),
            ..Default::default()
        };

        Self { package }
    }

    /// Sets the root directory for the package.
    pub fn with_root<N>(mut self, root: N) -> Self
    where
        N: Into<PathBuf>,
    {
        self.package.path = root.into();
        self
    }

    /// Adds a source file to the package, with the given name and source code.
    pub fn with_source<N, F>(mut self, file_path: N, source: F) -> Self
    where
        N: Into<PathBuf>,
        F: Into<String>,
    {
        let source_file = SourceFile::new(self.package.id, file_path, source);

        self.package.add_source(Arc::new(source_file));
        self
    }

    /// Adds the standard library source files to the package.
    pub fn with_standard_library(mut self) -> Self {
        self.package.add_std_sources();
        self
    }

    /// Creates and returns the built package.
    pub fn finish(self) -> Package {
        self.package
    }
}
