use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::Arc;

use error_snippet::SimpleDiagnostic;
use lume_errors::{MapDiagnostic, Result};
use lume_session::{GlobalCtx, LinkerPreference, Options};

#[derive(Debug, Clone)]
pub struct ObjectLocation {
    pub name: String,
    pub path: PathBuf,
}

/// Writes the object files of the given codegen result to disk in
/// the current workspace directory.
pub fn write_object_files(gcx: &Arc<GlobalCtx>, objects: Vec<(String, Vec<u8>)>) -> Result<Vec<ObjectLocation>> {
    std::fs::create_dir_all(gcx.obj_path()).map_diagnostic()?;
    std::fs::create_dir_all(gcx.obj_bc_path()).map_diagnostic()?;

    objects
        .into_iter()
        .map(|(name, bytecode)| {
            use std::io::Write;

            let output_file_path = gcx.obj_bc_path().join(format!("{name}.o"));
            let mut output_file = std::fs::File::create(&output_file_path).map_diagnostic()?;

            output_file.write_all(&bytecode).map_diagnostic()?;

            Ok(ObjectLocation {
                name,
                path: output_file_path,
            })
        })
        .collect::<Result<Vec<ObjectLocation>>>()
}

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
/// Returns `Err` if the linker binary could not be found, the linker fails to
/// invoke or fails to generate a valid output binary.
#[allow(clippy::missing_panics_doc)]
pub fn link_objects(objects: Vec<ObjectLocation>, output: &PathBuf, opts: &Options) -> Result<()> {
    let linker = match opts.linker {
        Some(LinkerPreference::Clang) => Linker::Clang,
        Some(LinkerPreference::Gcc) => Linker::Gcc,
        None => detect_linker()?,
    };

    ensure_command_available(linker.command())?;

    // Make sure the parent directory exists before attempting to
    // run any commands
    std::fs::create_dir_all(output.parent().unwrap()).map_diagnostic()?;

    let mut cmd = Command::new(linker.command());

    for obj in objects {
        cmd.arg(obj.path.clone());
    }

    // GCC will not link correctly if the runtime library is presented in the
    // beginning of the argument list, so we must add all source object files
    // first.
    cmd.arg(determine_runtime_path(opts)?);

    // Target specific linker options.
    if cfg!(target_os = "linux") {
        cmd.arg("-ldl");
        cmd.arg("-lm");
        cmd.arg("-lpthread");
    }

    if cfg!(target_os = "macos") {
        for name in ["Security", "CoreFoundation", "IOKit"] {
            cmd.arg("-framework");
            cmd.arg(name);
        }
    }

    cmd.stdin(Stdio::null());
    cmd.stdout(Stdio::null());
    cmd.stderr(Stdio::piped());

    cmd.arg("-o");
    cmd.arg(output);

    lume_trace::info!("linker command: {cmd:?}");

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

#[cfg(target_env = "msvc")]
static LIB_RUNTIME_NAME: &str = "liblume_runtime.lib";

#[cfg(not(target_env = "msvc"))]
static LIB_RUNTIME_NAME: &str = "liblume_runtime.a";

/// Determines the full path of the runtime library.
fn determine_runtime_path(opts: &Options) -> Result<PathBuf> {
    if let Some(defined_path) = &opts.runtime_path {
        return if defined_path.is_absolute() {
            Ok(defined_path.clone())
        } else {
            let cwd = std::env::current_dir().expect("could not get working directory");

            Ok(cwd.join(defined_path))
        };
    }

    lume_assets::asset_file_path(LIB_RUNTIME_NAME)
}
