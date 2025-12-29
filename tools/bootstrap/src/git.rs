use std::path::{Path, PathBuf};
use std::sync::LazyLock;

use lume_errors::{IntoDiagnostic, MapDiagnostic, Result, SimpleDiagnostic};
use regex::bytes::Regex;
use semver::{Version, VersionReq};

use crate::{cmd, fs, toolchain};

/// Defines the URL of the Lume Git repository, which is used to clone the
/// compiler.
pub const LUME_GIT_REPOSITORY: &str = "https://github.com/lume-lang/lume.git";

/// Attempts to clone the Lume compiler with the specified version.
pub fn clone(version: &toolchain::TargetVersion) -> Result<PathBuf> {
    if !crate::is_binary_installed("git") {
        return Err(SimpleDiagnostic::new("failed to clone repository: git is not installed").into());
    }

    let dest_directory = toolchain::download_directory_for(version)?;

    // Git will fail to clone the repository if it already exists.
    if dest_directory.exists() {
        if dest_directory.join(".git").exists() {
            return Ok(dest_directory);
        }

        if let Err(err) = std::fs::remove_dir_all(&dest_directory) {
            return Err(SimpleDiagnostic::new(
                "failed to clone repository: destination folder already exists and cannot be deleted",
            )
            .add_cause(err.into_diagnostic())
            .into());
        }
    }

    fs::create_dir(&dest_directory)?;

    let git_version = git_version()?;

    if VersionReq::parse(">=2.49.1").unwrap().matches(&git_version) {
        git_clone_revision(version, &dest_directory)?;
    } else if version.references_branch() {
        git_clone_single_branch(version, &dest_directory)?;
    } else if version.is_commit() {
        git_sparse_checkout(version, &dest_directory)?;
    } else {
        return Err(SimpleDiagnostic::new(
            "failed to clone repository: could not determine cloning method given Git version",
        )
        .into());
    }

    Ok(dest_directory)
}

/// Gets the current version of Git installed on the system.
fn git_version() -> Result<Version> {
    static VERSION_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\b\d+\.\d+\.\d+\b").unwrap());

    let output = cmd::execute("git", ["--version"])?;

    let version = VERSION_REGEX.find(&output.stdout);
    let version = match version {
        Some(value_match) => str::from_utf8(value_match.as_bytes()).expect("non-UTF8 git version"),
        None => {
            return Err(SimpleDiagnostic::new(format!(
                "failed to parse git version: {}",
                str::from_utf8(&output.stdout).unwrap()
            ))
            .into());
        }
    };

    Version::parse(version).map_diagnostic()
}

/// Clones the repository with the `--revision` argument, which as added in Git
/// 2.49.1.
fn git_clone_revision(version: &toolchain::TargetVersion, dest: &Path) -> Result<()> {
    let git_clone_args = match version {
        toolchain::TargetVersion::Branch(branch) => format!("refs/heads/{branch}"),
        toolchain::TargetVersion::Tag(tag) => format!("refs/tags/v{tag}"),
        toolchain::TargetVersion::Commit(commit) => commit.to_owned(),
    };

    let mut args = vec!["clone", "--depth=1", git_clone_args.as_str(), LUME_GIT_REPOSITORY];
    args.push(dest.to_str().unwrap());

    cmd::execute("git", args)?;

    Ok(())
}

/// Clones the repository with a single branch, using the `--single-branch`
/// argument.
fn git_clone_single_branch(version: &toolchain::TargetVersion, dest: &Path) -> Result<()> {
    let git_clone_args = match version {
        toolchain::TargetVersion::Branch(branch) => format!("--branch={branch}"),
        toolchain::TargetVersion::Tag(tag) => format!("--branch={tag}"),
        toolchain::TargetVersion::Commit(_) => panic!("git_clone_single_branch: cloning a commit is not supported"),
    };

    cmd::execute("git", [
        "clone",
        "--depth=1",
        "--single-branch",
        git_clone_args.as_str(),
        LUME_GIT_REPOSITORY,
        dest.to_str().unwrap(),
    ])?;

    Ok(())
}

/// Fetches a specific commit via sparse checkout.
fn git_sparse_checkout(version: &toolchain::TargetVersion, dest: &Path) -> Result<()> {
    cmd::execute_cwd("git", ["init"], dest)?;
    cmd::execute_cwd("git", ["remote", "add", "origin", LUME_GIT_REPOSITORY], dest)?;
    cmd::execute_cwd("git", ["fetch", "origin", version.to_string().as_str()], dest)?;
    cmd::execute_cwd("git", ["checkout", "FETCH_HEAD"], dest)?;

    Ok(())
}
