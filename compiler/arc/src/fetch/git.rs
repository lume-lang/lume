use std::path::PathBuf;
use std::process::{Command, Stdio};

use error_snippet::{IntoDiagnostic, Result, SimpleDiagnostic};
use lume_errors::MapDiagnostic;
use url::Url;

use crate::fetch::{DependencyFetcher, LOCAL_CACHE_DIR};
use crate::parser::{ManifestDependency, ManifestDependencySource};

/// Determines if Git is installed on the system.
fn is_git_installed() -> bool {
    let Ok(output) = Command::new("git").arg("--version").output() else {
        return false;
    };

    output.status.success()
}

/// Defines a [`DependencyFetcher`] which handles dependencies which live
/// inside of a Git repository, either local or remote.
pub struct GitDependencyFetcher;

impl DependencyFetcher for GitDependencyFetcher {
    fn fetch(&self, dependency: &ManifestDependency) -> Result<PathBuf> {
        let ManifestDependencySource::Git { repository, dir, .. } = &dependency.source else {
            return Err(SimpleDiagnostic::new(format!(
                "unsupported path scheme: only Git URLs are supported, found {dependency}"
            ))
            .into());
        };

        if !is_git_installed() {
            return Err(SimpleDiagnostic::new("failed to clone repository: git is not installed").into());
        }

        let repository_url = Url::parse(repository).map_diagnostic()?;
        let git_clone_args = git_clone_dependency_filter(dependency)?;

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

        let local_directory = LOCAL_CACHE_DIR.join(repository_name);

        if !local_directory.exists()
            && let Err(err) = std::fs::create_dir_all(&local_directory)
        {
            return Err(SimpleDiagnostic::new(
                "failed to clone repository: could not create local directory for clone",
            )
            .add_cause(err.into_diagnostic())
            .into());
        }

        let mut cmd = Command::new("git");

        cmd.args(["clone", "--depth", "1"]);
        cmd.args(git_clone_args);
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
            let absolute_path = local_directory.join(sub_directory);

            if !absolute_path.exists() {
                return Err(SimpleDiagnostic::new(format!(
                    "failed to clone repository: no sub-directory named {sub_directory} exists in repository"
                ))
                .into());
            }

            return Ok(absolute_path);
        }

        Ok(local_directory)
    }
}

/// Returns a list of extra arguments to pass to `git clone`, corresponding
/// to the options passed (such as `rev`, `tag`, `branch`).
///
/// # Errors
///
/// This function will return an error if more than one filter is applied.
fn git_clone_dependency_filter(dependency: &ManifestDependency) -> Result<Vec<String>> {
    let ManifestDependencySource::Git { rev, tag, branch, .. } = &dependency.source else {
        return Ok(Vec::new());
    };

    match (rev, tag, branch) {
        (Some(rev), None, None) => Ok(vec![String::from("--revision"), rev.clone()]),
        (None, Some(tag), None) => Ok(vec![String::from("--branch"), tag.clone()]),
        (None, None, Some(branch)) => Ok(vec![String::from("--branch"), branch.clone()]),
        (None, None, None) => Ok(Vec::new()),
        (_, _, _) => {
            Err(SimpleDiagnostic::new("only one of `revision`, `tag` and `branch` can be specified at once").into())
        }
    }
}
