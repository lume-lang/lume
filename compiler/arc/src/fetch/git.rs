use std::path::PathBuf;
use std::process::{Command, Stdio};

use error_snippet::{IntoDiagnostic, Result, SimpleDiagnostic};
use semver::VersionReq;

use crate::deps::DependencyPath;
use crate::fetch::{DependencyFetcher, LOCAL_CACHE_DIR};

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
    fn fetch(&self, path: DependencyPath, _: &VersionReq) -> Result<PathBuf> {
        let DependencyPath::Url(url) = path else {
            return Err(SimpleDiagnostic::new(format!(
                "unsupported path scheme: only Git URLs are supported, found {}",
                path.protocol()
            ))
            .into());
        };

        if !is_git_installed() {
            return Err(SimpleDiagnostic::new("failed to clone repository: git is not installed").into());
        }

        let repository_path = PathBuf::from(url.path());
        let repository_name = match repository_path.components().next_back() {
            Some(seg) => seg.as_os_str().to_string_lossy().to_string(),
            None => {
                return Err(SimpleDiagnostic::new(format!(
                    "failed to clone repository: could not find repository name from {}",
                    url.path()
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
        cmd.arg(url.as_str());
        cmd.arg(&local_directory);

        cmd.stdin(Stdio::null());
        cmd.stdout(Stdio::null());
        cmd.stderr(Stdio::null());

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

        Ok(local_directory)
    }
}
