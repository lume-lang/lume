use std::path::PathBuf;
use std::process::{Command, Stdio};

use error_snippet::{IntoDiagnostic, SimpleDiagnostic};
use lume_codegen::CodegenObjects;
use lume_errors::Result;
use lume_session::{LinkerPreference, Options};
use rust_embed::Embed;

#[cfg(all(target_family = "windows", target_env = "msvc"))]
const LIB_RUNTIME_NAME: &str = "liblumert.lib";

#[cfg(not(all(target_family = "windows", target_env = "msvc")))]
const LIB_RUNTIME_NAME: &str = "liblumert.a";

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
pub fn link_objects(objects: &CodegenObjects, output: &PathBuf, opts: &Options) -> Result<()> {
    let linker = match opts.linker {
        Some(LinkerPreference::Clang) => Linker::Clang,
        Some(LinkerPreference::Gcc) => Linker::Gcc,
        None => detect_linker()?,
    };

    ensure_command_available(linker.command())?;

    let runtime_path = determine_runtime_path(opts)?;

    // Make sure the parent directory exists before attempting to
    // run any commands
    std::fs::create_dir_all(output.parent().unwrap()).map_err(IntoDiagnostic::into_diagnostic)?;

    let mut cmd = Command::new(linker.command());

    for obj in &objects.objects {
        cmd.arg(obj.path.clone());
    }

    // GCC will not link correctly if the runtime library is presented in the beginning
    // of the argument list, so we must add all source object files first.
    cmd.arg(runtime_path);

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

    // If `CARGO` is set, we know we are being run as part of a `cargo run` command
    // which only happens inside of the source tree. Otherwise, we're likely
    // outside the tree and we need to look for the runtime in the system directories.
    let is_dev_build = std::env::var_os("CARGO").is_some();

    if is_dev_build {
        determine_dev_runtime_path()
    } else {
        determine_release_runtime_path()
    }
}

/// Determines the full path of the runtime library within the source tree of the Lume compiler
fn determine_dev_runtime_path() -> Result<PathBuf> {
    let profile_name = profile_name();

    let root_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../")
        .canonicalize()
        .map_err(|err| {
            Box::new(
                SimpleDiagnostic::new("could not determine root project directory").add_cause(err.into_diagnostic()),
            ) as error_snippet::Error
        })?;

    Ok(root_dir.join("target").join(profile_name).join(LIB_RUNTIME_NAME))
}

#[derive(Embed)]
#[folder = "$CARGO_MANIFEST_DIR/../../target"]
#[include = "debug/liblumert.{a,lib}"]
#[include = "release/liblumert.{a,lib}"]
struct RuntimeLibrary;

/// Determines the full path of the runtime library in the system library directory.
fn determine_release_runtime_path() -> Result<PathBuf> {
    let data_dir = determine_data_dir()?;
    let system_runtime_dir = data_dir.join(env!("CARGO_PKG_VERSION"));
    let system_runtime_path = system_runtime_dir.join(LIB_RUNTIME_NAME);

    if system_runtime_path.exists() {
        return Ok(system_runtime_path);
    }

    // Ensure the data directory exists before writing anything to disk
    std::fs::create_dir_all(system_runtime_dir).map_err(IntoDiagnostic::into_diagnostic)?;

    let library_file_name = format!("{}/{}", profile_name(), LIB_RUNTIME_NAME);

    let Some(runtime_file) = RuntimeLibrary::get(&library_file_name) else {
        return Err(SimpleDiagnostic::new("could not find runtime library in binary")
            .with_help("was the library built correctly?")
            .with_help("use `cargo build --release --workspace` to build the runtime")
            .into());
    };

    // Write the runtime library to disk so the linker can use it.
    std::fs::write(&system_runtime_path, runtime_file.data).map_err(IntoDiagnostic::into_diagnostic)?;

    Ok(system_runtime_path)
}

/// Determines the directory where the system runtime library would be stored.
fn determine_data_dir() -> Result<PathBuf> {
    if let Some(dir) = dirs::data_dir()
        && dir.exists()
    {
        return Ok(dir.join("lume"));
    }

    if let Some(dir) = dirs::data_local_dir()
        && dir.exists()
    {
        return Ok(dir.join("lume"));
    }

    if let Some(dir) = dirs::cache_dir()
        && dir.exists()
    {
        return Ok(dir.join("lume"));
    }

    if let Some(home_dir) = dirs::home_dir() {
        return Ok(home_dir.join(".lume"));
    }

    Err(SimpleDiagnostic::new("could not determine where to place runtime library").into())
}

fn profile_name() -> &'static str {
    #[cfg(debug_assertions)]
    {
        "debug"
    }

    #[cfg(not(debug_assertions))]
    {
        "release"
    }
}
