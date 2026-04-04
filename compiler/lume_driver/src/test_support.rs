use std::path::{Path, PathBuf};

use indexmap::IndexMap;
use lume_errors::{DiagCtxHandle, Result};
use lume_session::{FileLoader, FileSystemLoader, Options};

use crate::{Config, Driver, Pipeline};

/// Creates a new workspace builder at the specified root directory.
///
/// The workspace builder operates on a virtual filesystem, so the given
/// root directory does not need to exist. It is merely used as a base path
/// for file paths within the virtual filesystem.
///
/// # Example:
///
/// ```
/// use std::path::PathBuf;
///
/// use lume_driver::test_support::workspace;
/// use lume_errors::DiagCtxHandle;
///
/// let _ = workspace(PathBuf::new())
///     .build(DiagCtxHandle::shim());
/// ```
#[inline]
pub fn workspace<P: AsRef<Path>>(root: P) -> WorkspaceBuilder {
    WorkspaceBuilder::new(root)
}

pub struct WorkspaceBuilder {
    vfs: VirtualFileSystem,
    config: Config,
}

impl WorkspaceBuilder {
    /// Creates a new workspace builder with the specified root directory.
    ///
    /// The workspace builder operates on a virtual filesystem, so the given
    /// root directory does not need to exist. It is merely used as a base path
    /// for file paths within the virtual filesystem.
    ///
    /// # Example:
    ///
    /// ```
    /// use std::path::PathBuf;
    ///
    /// use lume_driver::test_support::WorkspaceBuilder;
    /// use lume_errors::DiagCtxHandle;
    ///
    /// let _ = WorkspaceBuilder::new(PathBuf::new())
    ///     .build(DiagCtxHandle::shim());
    /// ```
    pub fn new<P: AsRef<Path>>(root: P) -> Self {
        Self {
            vfs: VirtualFileSystem::new(root),
            config: Config {
                options: Options {
                    enable_incremental: false,
                    ..Default::default()
                },
                dry_run: false,
                loader: Box::new(FileSystemLoader),
                ..Default::default()
            },
        }
    }

    /// Configures the options for the workspace.
    ///
    /// # Example:
    ///
    /// ```
    /// use lume_driver::test_support::workspace;
    ///
    /// let root = std::path::PathBuf::new();
    /// let _builder = workspace(root)
    ///     .with_option(|opts| opts.enable_incremental = true);
    /// ```
    pub fn with_option<F: FnOnce(&mut Options)>(mut self, f: F) -> Self {
        f(&mut self.config.options);
        self
    }

    /// Configures the compiler for the workspace.
    ///
    /// # Example:
    ///
    /// ```
    /// use lume_driver::test_support::workspace;
    ///
    /// let root = std::path::PathBuf::new();
    /// let _builder = workspace(root)
    ///     .with_config(|opts| opts.dry_run = true);
    /// ```
    pub fn with_config<F: FnOnce(&mut Config)>(mut self, f: F) -> Self {
        f(&mut self.config);
        self
    }

    /// Adds a file to the workspace with the given content.
    ///
    /// Like other filesystem operations on the builder, this method operates on
    /// a virtual file system and does not actually create a file on disk.
    ///
    /// # Example:
    ///
    /// ```
    /// use lume_driver::test_support::workspace;
    ///
    /// let root = std::path::PathBuf::new();
    /// let _builder = workspace(root)
    ///     .with_file("Arcfile", "[package]\nname = \"my-package\"")
    ///     .with_file("src/main.rs", "fn main() {}");
    /// ```
    pub fn with_file<P: AsRef<Path>, C: Into<String>>(mut self, path: P, content: C) -> Self {
        self.vfs.files.insert(self.vfs.root.join(path), content.into());
        self
    }

    /// Creates a new driver instance from the workspace builder.
    ///
    /// # Example:
    ///
    /// ```
    /// use lume_driver::test_support::workspace;
    /// use lume_errors::{DiagCtx, DiagCtxHandle};
    ///
    /// let dcx = DiagCtx::new();
    /// let root = std::path::PathBuf::new();
    /// let _ = dcx.with(|handle| workspace(root).driver(handle));
    /// ```
    pub fn driver(mut self, dcx: DiagCtxHandle) -> Result<Driver> {
        let root = self.vfs.root.clone();
        self.config.loader = Box::new(self.vfs);

        Driver::from_root(&root, self.config, dcx)
    }

    /// Creates a new pipeline instance from the workspace builder.
    ///
    /// # Example:
    ///
    /// ```
    /// use lume_driver::test_support::workspace;
    /// use lume_errors::{DiagCtx, DiagCtxHandle};
    ///
    /// let dcx = DiagCtx::new();
    /// let root = std::path::PathBuf::new();
    /// let _ = dcx.with(|handle| workspace(root).pipeline(handle));
    /// ```
    pub fn pipeline(self, dcx: DiagCtxHandle) -> Result<Pipeline> {
        self.driver(dcx).map(|driver| driver.to_pipeline())
    }

    /// Checks the workspace for errors without compiling anything.
    ///
    /// Reported errors are emitted to the given diagnostic context handle.
    ///
    /// # Example:
    ///
    /// ```
    /// use lume_driver::test_support::workspace;
    /// use lume_errors::{DiagCtx, DiagCtxHandle};
    ///
    /// let dcx = DiagCtx::new();
    /// let root = std::path::PathBuf::new();
    /// let _ = dcx.with(|handle| workspace(root).check(handle));
    /// ```
    pub fn check(self, dcx: DiagCtxHandle) -> Result<()> {
        let driver = self.driver(dcx.clone())?;

        if let Err(err) = driver.check() {
            dcx.emit_and_push(err);
        }

        Ok(())
    }

    /// Builds the workspace for errors without compiling anything.
    ///
    /// Reported errors are emitted to the given diagnostic context handle.
    ///
    /// # Example:
    ///
    /// ```
    /// use lume_driver::test_support::workspace;
    /// use lume_errors::{DiagCtx, DiagCtxHandle};
    ///
    /// let dcx = DiagCtx::new();
    /// let root = std::path::PathBuf::new();
    /// let _ = dcx.with(|handle| workspace(root).check(handle));
    /// ```
    pub fn build(self, dcx: DiagCtxHandle) -> Result<PathBuf> {
        let driver = self.driver(dcx.clone())?;

        match driver.build() {
            Ok(executable) => Ok(executable.binary),
            Err(err) => {
                dcx.emit_and_push(err);
                dcx.ensure_untainted()?;

                unreachable!()
            }
        }
    }
}

pub struct VirtualFileSystem {
    root: PathBuf,
    files: IndexMap<PathBuf, String>,

    /// Whether to forward failed requests to a "real" file loader.
    forward: Option<Box<dyn FileLoader>>,
}

impl VirtualFileSystem {
    pub fn new<P: AsRef<Path>>(root: P) -> Self {
        Self {
            root: root.as_ref().to_path_buf(),
            files: IndexMap::new(),
            forward: Some(Box::new(FileSystemLoader)),
        }
    }
}

impl FileLoader for VirtualFileSystem {
    fn exists(&self, path: &Path) -> bool {
        let exists = self.files.contains_key(&self.root.join(path));

        if !exists && let Some(forward) = &self.forward {
            return forward.exists(path);
        }

        exists
    }

    fn read(&self, path: &Path) -> std::io::Result<String> {
        let result = self.files.get(&self.root.join(path)).cloned().ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("file not found: {}", path.display()),
            )
        });

        if result.is_err()
            && let Some(forward) = &self.forward
        {
            return forward.read(path);
        }

        result
    }

    fn read_dir(&self, path: &Path) -> std::io::Result<Vec<PathBuf>> {
        let entries = self
            .files
            .keys()
            .filter(|file| file.starts_with(path))
            .cloned()
            .collect::<Vec<_>>();

        if entries.is_empty()
            && let Some(forward) = &self.forward
        {
            return forward.read_dir(path);
        }

        Ok(entries)
    }
}

#[cfg(test)]
mod tests {
    use lume_errors::DiagCtx;

    use super::*;

    #[test]
    fn test_empty_workspace() {
        let workspace = workspace(PathBuf::from("/lume-test-support/"));

        let dcx = DiagCtx::new();
        if let Err(err) = dcx.with(|handle| workspace.check(handle)) {
            dcx.emit(err);
        }

        lume_errors_test::assert_dcx_snapshot!(dcx);
    }

    #[test]
    fn test_single_package() {
        let workspace = workspace(std::env::current_dir().unwrap())
            .with_config(|config| config.dry_run = true)
            .with_file(
                "Arcfile",
                r#"
                    [package]
                    name = "foo"
                    version = "1.0.0"
                    lume_version = "^0"
                "#,
            )
            .with_file("src/main.lm", "fn main() {}");

        let dcx = DiagCtx::new();
        if let Err(err) = dcx.with(|handle| workspace.build(handle)) {
            dcx.emit(err);
        }

        lume_errors_test::assert_dcx_snapshot!(dcx);
    }
}
