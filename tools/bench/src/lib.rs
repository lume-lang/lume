use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use lume_errors::{DiagCtx, MapDiagnostic, Result, SimpleDiagnostic};

/// Compiles the given source file into a Lume binary executable and returns the
/// path to the executable.
///
/// # Arguments
///
/// * `path` - The path to the source file to compile (does not need to exist).
/// * `content` - The content of the source file.
/// * `dcx` - The diagnostic context to use for reporting errors.
///
/// # Returns
///
/// A `Result` containing the path to the compiled binary executable, or an
/// error if the compilation fails.
pub(crate) fn compile_source_file(path: &Path, content: String, dcx: DiagCtx) -> Result<PathBuf> {
    let package_name = path.file_name().unwrap().display().to_string();
    let package_name = package_name.trim_end_matches(".lm");

    let package = build_stage::PackageBuilder::new(package_name)
        .with_root(path.parent().unwrap())
        .with_source(path.file_name().unwrap(), content)
        .with_standard_library()
        .finish();

    let options = lume_session::Options {
        optimize: lume_session::OptimizationLevel::O1,
        enable_incremental: false,
        ..Default::default()
    };

    let manifold_driver = build_stage::ManifoldDriver::with_options(package, dcx.clone(), options);
    manifold_driver.link()
}

/// Compiles the source file at the given path and returns the path to the
/// compiled binary executable.
pub fn compile(path: &Path) -> Result<PathBuf> {
    let dcx = DiagCtx::new();
    let file_content = std::fs::read_to_string(path).map_diagnostic()?;

    let binary_path = compile_source_file(path, file_content, dcx.clone())?;
    dcx.ensure_untainted()?;

    Ok(binary_path)
}

/// Runs the benchmark which is located at the given path.
pub fn run(binary_path: &Path) -> Result<i32> {
    let mut cmd = Command::new(binary_path);
    cmd.stdin(Stdio::null());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::inherit());

    let process = match cmd.spawn() {
        Ok(process) => process,
        Err(err) => {
            return Err(SimpleDiagnostic::new("could not invoke benchmark binary")
                .add_cause(err)
                .into());
        }
    };

    let output = match process.wait_with_output() {
        Ok(output) => output,
        Err(err) => {
            return Err(SimpleDiagnostic::new("benchmark binary time-out").add_cause(err).into());
        }
    };

    output
        .status
        .code()
        .ok_or_else(|| SimpleDiagnostic::new("benchmark was terminated").into())
}
