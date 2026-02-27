use std::fmt::Display;
use std::path::{Path, PathBuf};

use lume_errors::{MapDiagnostic, Result, SimpleDiagnostic};

use crate::git::{LUME_GIT_ORG, LUME_GIT_REPO};
use crate::toolchain::TargetVersion;
use crate::{cmd, fs, toolchain};

fn binbuild_archive_name<V: Display>(version: &V) -> String {
    format!("lume-v{version}_{}-{}", std::env::consts::OS, std::env::consts::ARCH)
}

fn binbuild_archive_url<V: Display>(version: &V) -> String {
    let archive_name = binbuild_archive_name(version);
    let archive_file_name = format!("{archive_name}.tar.gz");

    format!("https://github.com/{LUME_GIT_ORG}/{LUME_GIT_REPO}/releases/download/v{version}/{archive_file_name}")
}

/// Attempts to determine the matching binary build version of the given target
/// version.
///
/// If the functions returns [`Ok(None)`], no binary builds are associated with
/// the given version.
pub fn binbuild_version_of(version: &TargetVersion) -> Result<Option<semver::Version>> {
    let tag = match &version {
        TargetVersion::Tag(tag) => tag.clone(),
        TargetVersion::Branch(branch) if branch == "main" => crate::fetch::get_latest_release()?,
        _ => return Ok(None),
    };

    let archive_url = binbuild_archive_url(&tag);

    match ureq::head(&archive_url).call() {
        Ok(_) => Ok(Some(tag)),
        Err(ureq::Error::StatusCode(404)) => Ok(None),
        Err(err) => Err(SimpleDiagnostic::new("failed to fetch Lume release")
            .add_cause(err.into_io())
            .into()),
    }
}

/// Attempts to get the latest release of the Lume compiler, as a semver
/// version.
pub fn get_latest_release() -> Result<semver::Version> {
    #[derive(serde::Deserialize)]
    struct GithubRelease {
        tag_name: String,
    }

    let request_uri = format!("https://api.github.com/repos/{LUME_GIT_ORG}/{LUME_GIT_REPO}/releases/latest");

    let mut response_body = ureq::get(&request_uri)
        .header("Accept", "application/vnd.github+json")
        .call()
        .map_cause("failed to get latest Lume version")?
        .into_body();

    let release = response_body
        .read_json::<GithubRelease>()
        .map_cause("failed to deserialize release response")?;

    let tag_version = release.tag_name.strip_prefix('v').unwrap_or(&release.tag_name);

    Ok(semver::Version::parse(tag_version).unwrap())
}

/// Attempts to clone the Lume compiler with the specified version.
pub fn fetch(version: &semver::Version) -> Result<PathBuf> {
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

    let mut file_writer = std::fs::File::options()
        .create(true)
        .write(true)
        .truncate(true)
        .open(&archive_path)
        .map_cause(format!("could not open destination file: {}", archive_path.display()))?;

    let mut reader = ureq::get(&archive_url)
        .call()
        .map_cause(format!("failed to fetch archive `{archive_file_name}`"))?
        .into_body()
        .into_reader();

    std::io::copy(&mut reader, &mut file_writer)
        .map_cause(format!("failed to write archive {}", archive_path.display()))?;

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
