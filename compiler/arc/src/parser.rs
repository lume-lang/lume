use std::path::{Path, PathBuf};
use std::sync::Arc;

use error_snippet::{Label, WithSource};
use lume_errors::{Result, SimpleDiagnostic};
use lume_session::FileLoader;
use lume_span::{PackageId, SourceFile};
use semver::{Version, VersionReq};
use url::Url;

use crate::DEFAULT_ARCFILE;
use crate::errors::*;
use crate::manifest::*;

pub(crate) struct PackageParser {
    /// Absolute path to the package's Arcfile.
    path: PathBuf,

    /// Source file of the project's Arcfile.
    source: Arc<SourceFile>,

    /// Defines the current Lume version.
    current_lume_version: Version,
}

impl PackageParser {
    /// Creates a new [`PackageParser`] instance using the given `Arcfile`
    /// source file path.
    ///
    /// # Errors
    ///
    /// This method may fail if the given path is unreadable or
    /// otherwise inaccessible.
    fn new<L: FileLoader>(path: &Path, loader: &L) -> Result<Self> {
        let content = match loader.read(path) {
            Ok(content) => content,
            Err(err) => {
                return Err(ArcfileIoError { inner: err.into() }.into());
            }
        };

        let file_name: String = match path.file_name() {
            Some(os_str) => os_str.to_string_lossy().into_owned(),
            None => DEFAULT_ARCFILE.into(),
        };

        let source = SourceFile::new(PackageId::empty(), file_name, content);

        Ok(Self::from_source(path, Arc::new(source)))
    }

    /// Creates a new [`PackageParser`] instance using the given `Arcfile`
    /// source file.
    ///
    /// # Errors
    ///
    /// This method may fail if the given `Arcfile` is improperly formatted or
    /// otherwise invalid.
    pub fn from_source(path: &Path, source: Arc<SourceFile>) -> Self {
        Self {
            path: path.to_path_buf(),
            source,
            current_lume_version: Self::current_lume_version(),
        }
    }

    /// Creates a new [`PackageParser`] instance by locating the `Arcfile` in
    /// the given root directory.
    ///
    /// # Errors
    ///
    /// This method may fail if:
    /// - the given path has no `Arcfile` stored within it,
    /// - the located `Arcfile` doesn't refer to a file
    /// - or if the given `Arcfile` is otherwise invalid
    pub fn locate<L: FileLoader>(root: &Path, loader: &L) -> Result<Manifest> {
        let url = normalize_path_url(root)?;

        if url.scheme() != "file" {
            return Err(error_snippet::SimpleDiagnostic::new(format!(
                "only file:// URIs are supported, found {}",
                url.scheme()
            ))
            .into());
        }

        let root = url.to_file_path().unwrap();
        let path = root.join(DEFAULT_ARCFILE);

        if !loader.exists(&path) {
            return Err(ArcfileMissing {
                dir: root.display().to_string(),
            }
            .into());
        }

        let mut parser = Self::new(&path, loader)?;
        let mut manifest = parser.parse()?;

        if !manifest.package.no_std {
            let required_version = manifest
                .package
                .lume_version
                .clone()
                .map_or_else(|| VersionReq::STAR, |req| req.into_inner());

            let Some(std_path) = lume_assets::toolchain_std_path()? else {
                return Err(SimpleDiagnostic::new("could not locate standard library for package")
                    .with_help("are there an active Lume toolchain?")
                    .with_label(Label::note(
                        None,
                        manifest.package.name.span(),
                        format!("error occured in `{}`", manifest.package.name),
                    ))
                    .with_source(parser.source.clone())
                    .into());
            };

            manifest.dependencies.insert(String::from("std"), ManifestDependency {
                source: ManifestDependencySource::Local(FileDependency {
                    path: std_path.display().to_string(),
                }),
                required_version,
            });
        }

        Ok(manifest)
    }

    /// Parses the [`Manifest`] instance from the input source code.
    fn parse(&mut self) -> Result<Manifest> {
        let mut manifest = match toml::from_str::<Manifest>(&self.source.content) {
            Ok(mut manifest) => {
                manifest.path = self.path.parent().unwrap().to_path_buf();
                manifest
            }
            Err(err) => {
                let source_label = Label::error(None, err.span().unwrap_or_default(), err.message());

                return Err(SimpleDiagnostic::new("failed to parse Arcfile")
                    .with_label(source_label)
                    .with_source(self.source.clone())
                    .into());
            }
        };

        for dependency in manifest.dependencies.values_mut() {
            match &mut dependency.source {
                ManifestDependencySource::Local(FileDependency { path }) => {
                    let pathbuf = PathBuf::from(path.clone());

                    if pathbuf.is_relative() {
                        let absolute_path = manifest.path.join(&pathbuf).canonicalize().unwrap();

                        *path = absolute_path.display().to_string();
                    }
                }
                ManifestDependencySource::Git { .. } => {}
            }
        }

        self.verify_lume_version(&manifest)?;

        Ok(manifest)
    }

    /// Verifies that the current Lume compiler version is compatible
    /// with the one required by the [`Manifest`].
    pub(crate) fn verify_lume_version(&self, manifest: &Manifest) -> Result<()> {
        let Some(required_version) = manifest.package.lume_version.clone() else {
            return Ok(());
        };

        let current_lume_version = self.current_lume_version.clone();

        if !required_version.get_ref().matches(&current_lume_version) {
            return Err(ArcfileIncompatibleLumeVersion {
                source: self.source.clone(),
                range: required_version.span().clone(),
                current: current_lume_version,
                required: required_version.into_inner(),
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

fn normalize_path_url(path: &Path) -> Result<Url> {
    let path_str = path.as_os_str().to_str().unwrap();

    if let Ok(url) = url::Url::parse(path_str) {
        return Ok(url);
    }

    if let Ok(url) = url::Url::parse(&format!("file://{path_str}")) {
        return Ok(url);
    }

    Err(SimpleDiagnostic::new(format!("invalid file URI: {}", path.display())).into())
}
