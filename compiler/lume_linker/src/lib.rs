use std::{
    path::PathBuf,
    process::{Command, Stdio},
};

use error_snippet::{IntoDiagnostic, SimpleDiagnostic};
use lume_codegen::CodegenObjects;
use lume_errors::Result;
use lume_session::LinkerPreference;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Linker {
    Clang,
    Gcc,
}

impl Linker {
    fn command(self) -> &'static str {
        match self {
            Self::Clang => "clang",
            Self::Gcc => "gcc",
        }
    }
}

fn is_command_available(program: &str) -> bool {
    Command::new(program)
        .arg("--help")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .and_then(|mut proc| proc.wait())
        .is_ok_and(|exit| exit.success())
}

fn ensure_command_available(program: &str) -> Result<()> {
    if is_command_available(program) {
        Ok(())
    } else {
        Err(SimpleDiagnostic::new(format!("could not find linker executable '{program}'")).into())
    }
}

fn detect_linker() -> Result<Linker> {
    if is_command_available("clang") {
        return Ok(Linker::Clang);
    }

    if is_command_available("gcc") {
        return Ok(Linker::Gcc);
    }

    Err(SimpleDiagnostic::new("could not find any supported linker").into())
}

/// Links the generated object files into a single executable file, which can be
/// executed directly on the host machine.
///
/// # Errors
///
/// Returns `Err` if the linker binary could not be found, the linker fails to invoke or fails
/// to generate a valid output binary.
#[allow(clippy::missing_panics_doc)]
pub fn link_objects(objects: &CodegenObjects, output: PathBuf, linker: Option<LinkerPreference>) -> Result<()> {
    let linker = match linker {
        Some(LinkerPreference::Clang) => Linker::Clang,
        Some(LinkerPreference::Gcc) => Linker::Gcc,
        None => detect_linker()?,
    };

    ensure_command_available(linker.command())?;

    // Make sure the parent directory exists before attempting to
    // run any commands
    std::fs::create_dir_all(output.parent().unwrap()).map_err(IntoDiagnostic::into_diagnostic)?;

    let mut cmd = Command::new(linker.command());

    for obj in &objects.objects {
        cmd.arg(obj.path.clone());
    }

    cmd.stdin(Stdio::null());
    cmd.stdout(Stdio::null());
    cmd.stderr(Stdio::piped());

    cmd.arg("-o");
    cmd.arg(output);

    let process = cmd.spawn().map_err(|err| {
        Into::<error_snippet::Error>::into(SimpleDiagnostic::new(format!("could not invoke linker: {err}")))
    })?;

    let output = process
        .wait_with_output()
        .map_err(|err| Into::<error_snippet::Error>::into(SimpleDiagnostic::new(format!("linker time-out: {err}"))))?;

    if !output.status.success() {
        return Err(SimpleDiagnostic::new(format!(
            "linker exited with status code {}\n\n{}",
            output.status.code().unwrap_or_default(),
            String::from_utf8_lossy(&output.stderr).trim()
        ))
        .into());
    }

    Ok(())
}
