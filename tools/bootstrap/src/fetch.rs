use std::fmt::Display;
use std::path::{Path, PathBuf};

use lume_errors::{Result, SimpleDiagnostic};

use crate::{cmd, fs, toolchain};

/// Defines the URL of the Lume Git repository, which is used to download
/// artifacts from.
pub const LUME_GIT_REPOSITORY: &str = "https://github.com/lume-lang/lume";

fn binbuild_archive_name<V: Display>(version: &V) -> String {
    format!("lume-v{version}_{}-{}", std::env::consts::OS, std::env::consts::ARCH)
}

fn binbuild_archive_url<V: Display>(version: &V) -> String {
    let archive_name = binbuild_archive_name(version);
    let archive_file_name = format!("{archive_name}.tar.gz");

    format!("{LUME_GIT_REPOSITORY}/releases/download/v{version}/{archive_file_name}")
}

/// Attempts to determine whether the given version has any binary builds
/// available.
pub fn is_binbuild_available<V: Display>(version: &V) -> Result<bool> {
    let archive_url = binbuild_archive_url(version);

    let Some(fetcher) = fetcher() else {
        return Err(SimpleDiagnostic::new("failed to fetch Lume: neither wget nor curl is installed").into());
    };

    let exists = match fetcher {
        Fetcher::Curl => cmd::execute("curl", ["--fail", "--head", &archive_url]).is_ok(),
        Fetcher::Wget => cmd::execute("wget", ["--method=HEAD", &archive_url]).is_ok(),
    };

    Ok(exists)
}

/// Attempts to clone the Lume compiler with the specified version.
pub fn fetch(version: &semver::Version) -> Result<PathBuf> {
    let Some(fetcher) = fetcher() else {
        return Err(SimpleDiagnostic::new("failed to download Lume: neither wget nor curl is installed").into());
    };

    if !crate::is_binary_installed("tar") {
        return Err(SimpleDiagnostic::new("failed to download Lume: tar is not installed").into());
    }

    let download_dir = toolchain::download_directory()?;
    fs::create_dir(&download_dir)?;

    let archive_name = binbuild_archive_name(version);
    let archive_file_name = format!("{archive_name}.tar.gz");

    let archive_path = download_dir.join(&archive_file_name);
    let archive_url = binbuild_archive_url(version);

    let extraction_dir = download_dir.join(&archive_name);
    fs::create_dir(&extraction_dir)?;

    if extraction_dir.exists() && extraction_dir.join("LICENSE.md").exists() {
        return Ok(extraction_dir);
    }

    match fetcher {
        Fetcher::Curl => {
            cmd::execute("curl", ["-L", "-o", archive_path.to_str().unwrap(), &archive_url])?;
        }
        Fetcher::Wget => {
            cmd::execute("wget", ["-O", archive_path.to_str().unwrap(), &archive_url])?;
        }
    }

    // Extract the archive to the same directory
    cmd::execute_cwd(
        "tar",
        [
            "-x",
            "-f",
            archive_path.to_str().unwrap(),
            "-C",
            extraction_dir.to_str().unwrap(),
        ],
        &download_dir,
    )?;

    Ok(extraction_dir)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Fetcher {
    Curl,
    Wget,
}

fn fetcher() -> Option<Fetcher> {
    if crate::is_binary_installed("curl") {
        Some(Fetcher::Curl)
    } else if crate::is_binary_installed("wget") {
        Some(Fetcher::Wget)
    } else {
        None
    }
}

#[cfg(target_env = "msvc")]
static LIB_RUNTIME_NAME: &str = "liblume_runtime.lib";

#[cfg(not(target_env = "msvc"))]
static LIB_RUNTIME_NAME: &str = "liblume_runtime.a";

/// Copies the compiled artifacts from the downloaded root directory to the
/// given destination directory.
pub fn copy_artifacts(download_root: &Path, artifact_root: &Path) -> Result<()> {
    let bin_target_dir = artifact_root.join("bin");
    fs::create_dir(&bin_target_dir)?;

    let lib_target_dir = artifact_root.join("lib");
    fs::create_dir(&lib_target_dir)?;

    let std_target_dir = artifact_root.join("std");
    fs::create_dir(&std_target_dir)?;

    fs::copy(download_root.join("lume"), bin_target_dir.join("lume"))?;
    fs::copy(
        download_root.join(LIB_RUNTIME_NAME),
        lib_target_dir.join(LIB_RUNTIME_NAME),
    )?;
    fs::copy(download_root.join("std"), std_target_dir)?;
    fs::copy(download_root.join("LICENSE.md"), artifact_root.join("LICENSE.md"))?;

    Ok(())
}
