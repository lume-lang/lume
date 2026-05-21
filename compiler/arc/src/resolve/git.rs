use std::path::PathBuf;
use std::process::{Command, Stdio};

use error_snippet::{IntoDiagnostic, Result, SimpleDiagnostic};
use lume_errors::MapDiagnostic;
use url::Url;

use crate::local_cache_dir;
use crate::manifest::GitDependency;

/// Determines if Git is installed on the system.
pub(crate) fn is_git_installed() -> bool {
    let Ok(output) = Command::new("git").arg("--version").output() else {
        return false;
    };

    output.status.success()
}

/// Clones the repository defined in `dependency` to a local folder
/// and returns it's path.
pub(crate) fn clone_repository(repository: &str, rev: &str, dir: Option<&str>) -> Result<PathBuf> {
    if !is_git_installed() {
        return Err(SimpleDiagnostic::new("failed to clone repository: git is not installed").into());
    }

    let repository_url = Url::parse(repository).map_diagnostic()?;

    let repository_path = PathBuf::from(repository_url.path());
    let repository_name = match repository_path.components().next_back() {
        Some(seg) => seg.as_os_str().to_string_lossy().to_string(),
        None => {
            return Err(SimpleDiagnostic::new(format!(
                "failed to clone repository: could not find repository name from {}",
                repository_url.path()
            ))
            .into());
        }
    };

    let repository_folder_name = format!("{repository_name}{rev}");

    let local_cache_dir = local_cache_dir();
    let local_directory = local_cache_dir.join(repository_folder_name);

    if !local_cache_dir.exists()
        && let Err(err) = std::fs::create_dir_all(&local_cache_dir)
    {
        return Err(
            SimpleDiagnostic::new("failed to clone repository: could not create local directory for clone")
                .add_cause(err.into_diagnostic())
                .into(),
        );
    }

    let requested_path = if let Some(sub_directory) = dir {
        local_directory.join(sub_directory)
    } else {
        local_directory.clone()
    };

    // Git will fail to clone the repository if it already exists.
    if local_directory.exists() {
        if local_directory.join(".git").exists() {
            return Ok(requested_path);
        }

        if let Err(err) = std::fs::remove_dir_all(&local_directory) {
            return Err(SimpleDiagnostic::new(
                "failed to clone repository: destination folder already exists and cannot be deleted",
            )
            .add_cause(err.into_diagnostic())
            .into());
        }
    }

    let mut cmd = Command::new("git");

    cmd.args(["clone", "--depth", "1"]);
    cmd.args(["--revision", rev]);
    cmd.arg(repository_url.as_str());
    cmd.arg(&local_directory);

    cmd.stdin(Stdio::null());
    cmd.stdout(Stdio::null());
    cmd.stderr(Stdio::piped());

    tracing::debug!("cloning repository {repository_url}");
    tracing::trace!("{cmd:?}");

    let process = cmd.spawn().map_err(|err| {
        Into::<error_snippet::Error>::into(SimpleDiagnostic::new(format!("failed to clone repository: {err}")))
    })?;

    let output = process
        .wait_with_output()
        .map_err(|err| Into::<error_snippet::Error>::into(SimpleDiagnostic::new(format!("git time-out: {err}"))))?;

    if !output.status.success() {
        return Err(SimpleDiagnostic::new(format!(
            "git exited with status code {}\n\n{}",
            output.status.code().unwrap_or_default(),
            String::from_utf8_lossy(&output.stderr).trim()
        ))
        .into());
    }

    if let Some(sub_directory) = dir {
        if !requested_path.exists() {
            return Err(SimpleDiagnostic::new(format!(
                "failed to clone repository: no sub-directory named {sub_directory} exists in repository"
            ))
            .into());
        }

        return Ok(requested_path);
    }

    Ok(local_directory)
}

/// Gets the Git SHA-256 revision of the given Git dependency.
pub(crate) fn revision_of(source: &GitDependency) -> Result<String> {
    if let Some(rev) = &source.rev {
        return Ok(rev.clone());
    }

    if !is_git_installed() {
        return Err(SimpleDiagnostic::new("failed to clone repository: git is not installed").into());
    }

    let mut cmd = Command::new("git");
    cmd.args(["ls-remote", "--quiet", &source.repository]);

    match (source.tag.as_ref(), source.branch.as_ref()) {
        (Some(tag), None) => {
            cmd.args(["--tags", tag]);
        }
        (None, Some(branch)) => {
            cmd.args(["--branches", branch]);
        }
        (None, None) => {}
        (_, _) => {
            return Err(
                SimpleDiagnostic::new("only one of `revision`, `tag` and `branch` can be specified at once").into(),
            );
        }
    }

    cmd.stdin(Stdio::null());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());

    tracing::debug!("getting revision of {}", source.repository);
    tracing::trace!("{cmd:?}");

    let process = cmd.spawn().map_err(|err| {
        Into::<error_snippet::Error>::into(SimpleDiagnostic::new(format!("failed to list repository: {err}")))
    })?;

    let output = process
        .wait_with_output()
        .map_err(|err| Into::<error_snippet::Error>::into(SimpleDiagnostic::new(format!("git time-out: {err}"))))?;

    if !output.status.success() {
        return Err(SimpleDiagnostic::new(format!(
            "git exited with status code {}\n\n{}",
            output.status.code().unwrap_or_default(),
            String::from_utf8_lossy(&output.stderr).trim()
        ))
        .into());
    }

    let stdout = String::from_utf8_lossy(&output.stdout);

    match stdout.find('\t') {
        Some(idx) => Ok(stdout[..idx].trim().to_string()),
        None => Err(SimpleDiagnostic::new(format!(
            "git exited with empty listing\n\n{}",
            String::from_utf8_lossy(&output.stderr).trim()
        ))
        .into()),
    }
}
