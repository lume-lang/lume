use std::fmt::Display;
use std::path::{Path, PathBuf};

use error_snippet::{Result, SimpleDiagnostic};
use lume_errors::MapDiagnostic;

use crate::{cmd, fs};

#[derive(clap::Parser, clap::ValueEnum, Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Profile {
    Debug,

    #[default]
    Release,
}

impl Display for Profile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Profile::Debug => write!(f, "debug"),
            Profile::Release => write!(f, "release"),
        }
    }
}

/// Options for building the Lume compiler and runtime.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuildOptions {
    pub profile: Profile,
}

impl Default for BuildOptions {
    fn default() -> Self {
        Self {
            profile: Profile::Release,
        }
    }
}

/// Represents a requested version from the user, which should be clone and
/// compiled.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TargetVersion {
    /// References the latest version of the compiler, within the given branch.
    Branch(String),

    /// References a specific version of the compiler, defined by it's Git tag.
    Tag(semver::Version),

    /// References a specific version of the compiler, defined by it's Git
    /// commit hash.
    Commit(String),
}

impl TargetVersion {
    pub fn references_branch(&self) -> bool {
        matches!(self, Self::Branch(_) | Self::Tag(_))
    }

    pub fn is_commit(&self) -> bool {
        matches!(self, Self::Commit(_))
    }
}

impl Display for TargetVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Branch(branch) => write!(f, "{branch}"),
            Self::Tag(version) => write!(f, "{version}"),
            Self::Commit(commit) => write!(f, "{commit}"),
        }
    }
}

/// Attempts to build the Lume compiler, which exists in the given compiler
/// root directory.
pub fn compile_compiler(cwd: &Path, opts: &BuildOptions) -> Result<()> {
    if !crate::is_binary_installed("cargo") {
        return Err(SimpleDiagnostic::new("failed to build compiler: cargo is not installed").into());
    }

    // Git will fail to clone the repository if it already exists.
    if !cwd.join("Cargo.toml").exists() {
        return Err(
            SimpleDiagnostic::new("failed to build compiler: destination folder is not valid Rust project").into(),
        );
    }

    let profile_arg = match opts.profile {
        Profile::Release => "--profile=release",
        Profile::Debug => "--profile=dev",
    };

    cmd::execute_cwd("cargo", ["build", profile_arg, "--package", "lume_runtime"], cwd)?;
    cmd::execute_cwd("cargo", ["build", profile_arg, "--bin", "lume"], cwd)?;

    Ok(())
}

#[cfg(target_env = "msvc")]
static LIB_RUNTIME_NAME: &str = "liblume_runtime.lib";

#[cfg(not(target_env = "msvc"))]
static LIB_RUNTIME_NAME: &str = "liblume_runtime.a";

/// Copies the compiled artifacts from the compiler root directory to the given
/// destination directory.
pub fn copy_artifacts(compiler_root: &Path, artifact_root: &Path, opts: &BuildOptions) -> Result<()> {
    let source_dir = compiler_root.join("target").join(opts.profile.to_string());

    let bin_target_dir = artifact_root.join("bin");
    fs::create_dir(&bin_target_dir)?;

    let lib_target_dir = artifact_root.join("lib");
    fs::create_dir(&lib_target_dir)?;

    let std_target_dir = artifact_root.join("std");
    fs::create_dir(&std_target_dir)?;

    fs::copy(source_dir.join("lume"), bin_target_dir.join("lume"))?;
    fs::copy(source_dir.join(LIB_RUNTIME_NAME), lib_target_dir.join(LIB_RUNTIME_NAME))?;
    fs::copy(compiler_root.join("std"), std_target_dir)?;
    fs::copy(compiler_root.join("LICENSE.md"), artifact_root.join("LICENSE.md"))?;

    Ok(())
}

/// Links the given toolchain to the current toolchain link path, causing it to
/// be used by default for subsequent compiler invocations.
pub fn link_toolchain(artifact_root: &Path) -> Result<()> {
    let current_link_path = lume_assets::current_toolchain_linkpath()?;

    let previous = if current_link_path.exists() {
        let link = std::fs::read_link(&current_link_path).ok();

        // `std::fs::hard_link` will fail if a file already exists.
        std::fs::remove_file(&current_link_path).map_cause("could not remove previous toolchain link")?;

        link
    } else {
        None
    };

    if let Err(err) = fs::link(artifact_root, &current_link_path) {
        // If the toolchain link failed, try to link the previous toolchain
        if let Some(previous) = previous {
            fs::link(previous, &current_link_path)?;
        }

        return Err(err);
    }

    // If the links in the home directory don't already exist, link them.
    crate::home::add_links_if_needed()?;

    Ok(())
}

/// Gets the currently active toolchain.
pub fn active_toolchain() -> Option<PathBuf> {
    let current_link_path = lume_assets::current_toolchain_linkpath().ok()?;

    std::fs::read_link(&current_link_path).ok()
}

/// Unlinks the current toolchain link.
pub fn unlink_toolchain() -> Result<bool> {
    let current_link_path = lume_assets::current_toolchain_linkpath()?;

    if current_link_path.exists() {
        std::fs::remove_file(&current_link_path).map_cause("could not remove toolchain link")?;

        Ok(true)
    } else {
        Ok(false)
    }
}

/// Gets the list of downloaded toolchains.
///
/// Downloaded toolchains are those that exist within the download directory.
pub fn downloaded_toolchains() -> Result<Vec<String>> {
    let mut toolchains = Vec::new();
    let Ok(dir_iter) = download_directory()?.read_dir() else {
        return Ok(Vec::new());
    };

    for dir in dir_iter {
        let Ok(dir) = dir else { continue };
        let name = dir.file_name().to_string_lossy().to_string();

        toolchains.push(name);
    }

    Ok(toolchains)
}

/// Gets the list of installed toolchains.
///
/// Installed toolchains are those that exist within the toolchain artifact
/// directory.
pub fn installed_toolchains() -> Result<Vec<String>> {
    let mut toolchains = Vec::new();
    let Ok(dir_iter) = artifact_directory()?.read_dir() else {
        return Ok(Vec::new());
    };

    for dir in dir_iter {
        let Ok(dir) = dir else { continue };
        let name = dir.file_name().to_string_lossy().to_string();

        toolchains.push(name);
    }

    Ok(toolchains)
}

/// Attempts to determine the directory where all toolchains would be
/// downloaded and/or cloned into.
///
/// Depending on the directory returned, the directory might not exist.
pub fn download_directory() -> Result<PathBuf> {
    Ok(lume_assets::toolchain_base_path()?.join("downloads"))
}

/// Attempts to determine the directory where the given toolchain would be
/// downloaded and/or cloned into.
///
/// Depending on the directory returned, the directory might not exist.
pub fn download_directory_for<V>(version: V) -> Result<PathBuf>
where
    V: ToString,
{
    let toolchain = version.to_string();
    let toolchain_dir = download_directory()?.join(&toolchain);

    Ok(toolchain_dir)
}

/// Attempts to determine the directory where all toolchains would
/// have their compiled artifacts placed into.
///
/// Depending on the directory returned, the directory might not exist.
pub fn artifact_directory() -> Result<PathBuf> {
    Ok(lume_assets::toolchain_base_path()?.join("artifacts"))
}

/// Attempts to determine the directory where the given toolchain would
/// have it's compiled artifacts placed into.
///
/// Depending on the directory returned, the directory might not exist.
pub fn artifact_directory_for<V>(version: V) -> Result<PathBuf>
where
    V: ToString,
{
    let toolchain = version.to_string();
    let toolchain_dir = artifact_directory()?.join(&toolchain);

    Ok(toolchain_dir)
}
