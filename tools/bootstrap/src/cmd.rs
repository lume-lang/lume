use std::ffi::OsStr;
use std::path::Path;
use std::process::Command;

use lume_errors::{MapDiagnostic, Result, SimpleDiagnostic};

/// Executes the given command, along with the defined arguments.
///
/// The given closure is called with the command before it is executed, allowing
/// other changes before invoking it.
///
/// # Errors
///
/// Returns [`Err`] if the command failed to spawn, the command timed out, or
/// the command exited with a non-zero status code.
pub fn execute_with<B, A, S, F>(binary: B, args: A, f: F) -> Result<Option<i32>>
where
    B: AsRef<OsStr>,
    A: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
    F: FnOnce(&mut Command),
{
    let binary_str = binary.as_ref().to_string_lossy().to_string();

    let mut cmd = Command::new(binary);
    cmd.args(args);

    cmd.stdin(std::process::Stdio::null());
    cmd.stdout(std::process::Stdio::null());
    cmd.stderr(std::process::Stdio::piped());

    f(&mut cmd);

    let process = cmd.spawn().map_diagnostic()?;
    let output = process
        .wait_with_output()
        .map_cause(format!("{binary_str} command time-out"))?;

    if !output.status.success() {
        return Err(SimpleDiagnostic::new(format!(
            "{binary_str} exited with status code {}\n\n{}",
            output.status.code().unwrap_or_default(),
            String::from_utf8_lossy(&output.stderr).trim()
        ))
        .into());
    }

    Ok(output.status.code())
}

/// Executes the given command, along with the defined arguments, inside the
/// given working directory.
///
/// # Errors
///
/// Returns [`Err`] if the command failed to spawn, the command timed out, or
/// the command exited with a non-zero status code.
pub fn execute_cwd<B, A, S, C>(binary: B, args: A, cwd: C) -> Result<Option<i32>>
where
    B: AsRef<OsStr>,
    A: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
    C: AsRef<Path>,
{
    execute_with(binary, args, |cmd| {
        cmd.current_dir(cwd);
    })
}

/// Executes the given command, along with the defined arguments.
///
/// # Errors
///
/// Returns [`Err`] if the command failed to spawn, the command timed out, or
/// the command exited with a non-zero status code.
pub fn execute<B, A, S>(binary: B, args: A) -> Result<Option<i32>>
where
    B: AsRef<OsStr>,
    A: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    execute_with(binary, args, |_cmd| {})
}
