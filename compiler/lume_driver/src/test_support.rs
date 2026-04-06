use std::path::{Path, PathBuf};

use lume_errors::{DiagCtxHandle, Result};
use lume_session::{FileSystemLoader, Options, VirtualFileSystem};

use crate::{Config, Driver, Pipeline};

pub type IO = VirtualFileSystem<FileSystemLoader>;

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
    config: Config<IO>,
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
            config: Config {
                options: Options {
                    enable_incremental: false,
                    ..Default::default()
                },
                dry_run: false,
                io: VirtualFileSystem::new(root, FileSystemLoader),
                source_overrides: None,
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
    pub fn with_config<F: FnOnce(&mut Config<IO>)>(mut self, f: F) -> Self {
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
        let path = self.config.io.root().join(path);
        self.config.io.write_mapped(path, content.into());

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
    pub fn driver(self, dcx: DiagCtxHandle) -> Result<Driver<IO>> {
        let root = self.config.io.root().to_path_buf();

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
