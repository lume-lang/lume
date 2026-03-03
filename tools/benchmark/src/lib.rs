use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use lume_errors::{DiagCtx, MapDiagnostic, Result, SimpleDiagnostic};

pub extern crate divan;

pub use lume_benchmark_macros::*;
pub use proc_macro2;
pub use quote::{format_ident, quote as source};

pub type Source = proc_macro2::TokenStream;

/// Compiles the source file at the given path and returns the path to the
/// compiled binary executable.
pub fn compile(path: &Path) -> Result<PathBuf> {
    let dcx = DiagCtx::new();
    dcx.panic_on_error();

    let file_name = path.file_name().unwrap();
    let file_base = file_name.to_str().unwrap().split('.').next().unwrap();
    let file_content = std::fs::read_to_string(path).map_diagnostic()?;

    let binary_path = lume_driver::test_support::workspace(path.parent().unwrap())
        .with_option(|opts| opts.enable_incremental = false)
        .with_option(|opts| {
            // Giving each test it's own output directory.
            //
            // This is to avoid race conditions between tests where some packages have the
            // same name (for example, `std`). If not defined, multiple threads might try to
            // write `bc/std.o`, which will cause the linkers to throw errors.
            let relative_dir = PathBuf::from(format!("obj/{file_base}/"));
            opts.output_directory = Some(relative_dir);
        })
        .with_file(
            "Arcfile",
            format!(
                r#"
                [package]
                name = "{file_base}"
                version = "1.0.0"
                lume_version = "^0"
            "#
            ),
        )
        .with_file(PathBuf::from("src").join(file_name), &file_content)
        .build(dcx.handle());

    if let Ok(path) = binary_path {
        Ok(path)
    } else {
        let mut renderer = lume_errors::GraphicalRenderer::new();
        renderer.use_colors = false;
        renderer.highlight_source = false;

        let buffer = dcx.render_buffer(&mut renderer).unwrap_or(String::from("<empty>"));
        Err(SimpleDiagnostic::new(format!("failed to compile benchmark:\n{buffer}")).into())
    }
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

#[doc(hidden)]
pub mod __private {
    use std::hash::Hasher;

    use super::*;

    pub fn prepare_benchmark<T: AsRef<str>>(tmpdir: T, name: &str, source_code: String) -> PathBuf {
        let hash = {
            let mut hasher = std::hash::DefaultHasher::new();
            hasher.write(source_code.as_bytes());

            hasher.finish()
        };

        let benchmark_directory = format!("{name}_{hash}");

        let output_directory = Path::new(tmpdir.as_ref())
            .to_path_buf()
            .join("benches")
            .join(benchmark_directory);

        let main_source_path = output_directory.join("main.lm");

        std::fs::create_dir_all(&output_directory).unwrap();
        std::fs::write(&main_source_path, source_code).unwrap();

        compile(&main_source_path).unwrap()
    }
}
