use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;

use error_snippet::{IntoDiagnostic, SimpleDiagnostic};
use lume_errors::Result;
use lume_session::{GlobalCtx, Options};

#[cfg(all(target_family = "windows", target_env = "msvc"))]
const BIN_RUNNER_NAME: &str = "lume_runner.exe";

#[cfg(not(all(target_family = "windows", target_env = "msvc")))]
const BIN_RUNNER_NAME: &str = "lume_runner";

/// Writes the object files of the given codegen result to disk in
/// the current workspace directory.
pub fn fuse_binary_file(gcx: &Arc<GlobalCtx>, mir: lume_mir::ModuleMap, dest: &PathBuf) -> Result<()> {
    use error_snippet::IntoDiagnostic;

    std::fs::create_dir_all(gcx.bin_path()).map_err(IntoDiagnostic::into_diagnostic)?;

    let runner_path = determine_runner_path(&gcx.session.options)?;
    std::fs::copy(&runner_path, dest).map_err(IntoDiagnostic::into_diagnostic)?;

    let serialized_mir = postcard::to_allocvec(&mir).expect("could not serialize MIR map");
    let mut binary_path = std::fs::File::options()
        .append(true)
        .open(dest)
        .map_err(IntoDiagnostic::into_diagnostic)?;

    // Write the MIR itself...
    binary_path
        .write_all(&serialized_mir)
        .map_err(IntoDiagnostic::into_diagnostic)?;

    // ...and finish with the length of the MIR, so we know the span
    // which the MIR occupies within the binary, when we have to run it.
    binary_path
        .write_all(&(serialized_mir.len() as u64).to_ne_bytes())
        .map_err(IntoDiagnostic::into_diagnostic)?;

    #[cfg(all(not(target_os = "hermit"), unix))]
    {
        use std::os::unix::fs::PermissionsExt;

        // Allow the owner to read and execute the file.
        let file_mode = 0o500_u32;

        let mut perms = binary_path.metadata()?.permissions();
        perms.set_mode(perms.mode() | file_mode);

        // No need to error if we can't set the persmissions, so
        // we just ignore it here.
        let _ = binary_path.set_permissions(perms);
    }

    Ok(())
}

/// Determines the full path of the runner binary.
fn determine_runner_path(opts: &Options) -> Result<PathBuf> {
    if let Some(defined_path) = &opts.runner_path {
        return if defined_path.is_absolute() {
            Ok(defined_path.clone())
        } else {
            let cwd = std::env::current_dir().expect("could not get working directory");

            Ok(cwd.join(defined_path))
        };
    }

    // If `CARGO` is set, we know we are being run as part of a `cargo run` command
    // which only happens inside of the source tree. Otherwise, we're likely
    // outside the tree and we need to look for the runner in the system
    // directories.
    let is_dev_build = std::env::var_os("CARGO").is_some();

    if is_dev_build {
        determine_dev_runner_path()
    } else {
        determine_release_runner_path()
    }
}

/// Determines the full path of the runner binary within the source tree of the
/// Lume compiler
fn determine_dev_runner_path() -> Result<PathBuf> {
    let profile_name = profile_name();

    let root_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../")
        .canonicalize()
        .map_err(|err| {
            Box::new(
                SimpleDiagnostic::new("could not determine root project directory").add_cause(err.into_diagnostic()),
            ) as error_snippet::Error
        })?;

    let library_path = root_dir.join("target").join(profile_name).join(BIN_RUNNER_NAME);
    if !library_path.exists() {
        return Err(SimpleDiagnostic::new("could not find runner binary in project")
            .with_help("was the library built correctly?")
            .with_help("use `cargo build --release --workspace` to build the runner")
            .into());
    }

    Ok(library_path)
}

#[cfg(debug_assertions)]
static RUNNER_BINARY: &[u8] = include_bytes!("../../../target/debug/lume_runner");

#[cfg(not(debug_assertions))]
static RUNNER_BINARY: &[u8] = include_bytes!("../../../target/release/lume_runner");

/// Determines the full path of the runtime library in the system library
/// directory.
fn determine_release_runner_path() -> Result<PathBuf> {
    let data_dir = determine_data_dir()?;
    let system_runtime_dir = data_dir.join(env!("CARGO_PKG_VERSION"));
    let system_runtime_path = system_runtime_dir.join(BIN_RUNNER_NAME);

    if system_runtime_path.exists() {
        return Ok(system_runtime_path);
    }

    // Ensure the data directory exists before writing anything to disk
    std::fs::create_dir_all(system_runtime_dir).map_err(IntoDiagnostic::into_diagnostic)?;

    // Write the runtime library to disk so the linker can use it.
    std::fs::write(&system_runtime_path, RUNNER_BINARY).map_err(IntoDiagnostic::into_diagnostic)?;

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
