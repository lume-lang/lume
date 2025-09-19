use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::{Arc, LazyLock};

use build_stage::ManifoldDriver;
use error_snippet::{IntoDiagnostic, SimpleDiagnostic};
use lume_errors::{DiagCtx, Result};
use lume_span::{PackageId, SourceFile};
use owo_colors::OwoColorize;
use regex::Regex;

use crate::TestResult;

pub(crate) struct TestCase {
    source_path: PathBuf,
    binary_path: PathBuf,
    file_content: String,
}

pub(crate) fn run_test(path: PathBuf) -> Result<TestResult> {
    let mut stdout_path = path.clone();
    stdout_path.set_extension("stdout");

    let file_content = std::fs::read_to_string(&path).map_err(IntoDiagnostic::into_diagnostic)?;
    let binary_path = compile(&path, file_content.clone())?;

    let test_case = TestCase {
        source_path: path.clone(),
        binary_path,
        file_content,
    };

    let mut cmd = Command::new(&test_case.binary_path);
    cmd.stdin(Stdio::null());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::inherit());

    let process = cmd.spawn().map_err(|err| {
        Into::<error_snippet::Error>::into(SimpleDiagnostic::new(format!("could not invoke test binary: {err}")))
    })?;

    let output = process
        .wait_with_output()
        .map_err(|err| Into::<error_snippet::Error>::into(SimpleDiagnostic::new(format!("binary time-out: {err}"))))?;

    let stdout = String::from_utf8_lossy(&output.stderr).trim().to_string();

    if let Some(expected_return_code) = determine_expected_result_code(&test_case)
        && let Some(return_code) = output.status.code()
        && expected_return_code as i32 != return_code
    {
        let write_failure_report = Box::new(move || {
            let mut f = Vec::new();

            writeln!(
                &mut f,
                "Source file:    {}",
                test_case.source_path.display().cyan().underline()
            )
            .unwrap();

            writeln!(&mut f, "Expected return code:   {}", expected_return_code.yellow()).unwrap();
            writeln!(&mut f, "Actual return code:     {}", return_code.red()).unwrap();
            writeln!(&mut f).unwrap();

            String::from_utf8_lossy(&f).to_string()
        });

        return Ok(TestResult::Failure { write_failure_report });
    }

    if stdout.is_empty() && !stdout_path.exists() {
        return Ok(TestResult::Success);
    }

    crate::diff::diff_output_of(stdout, path, stdout_path)
}

fn compile(path: &Path, content: String) -> Result<PathBuf> {
    let file_name = Path::new(path.file_name().unwrap());
    let source_file = SourceFile::new(PackageId::empty(), file_name, content);

    let mut package_name = path.to_path_buf();
    package_name.set_extension("");

    let mut stub_package = build_stage::stub_package_with(|pkg| pkg.add_source(Arc::new(source_file)));
    stub_package.name = package_name.file_name().unwrap().display().to_string();
    stub_package.path = path.parent().unwrap().to_path_buf();
    stub_package.add_std_sources();

    let dcx = DiagCtx::new();
    let manifold_driver = ManifoldDriver::new(stub_package, dcx.clone());

    manifold_driver.compile()
}

fn determine_expected_result_code(test_case: &TestCase) -> Option<u8> {
    static RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^//#\s*test-return\s*=\s*(\d+)").unwrap());

    let Some(captures) = RE.captures(&test_case.file_content) else {
        return None;
    };

    captures[1].parse::<u8>().ok()
}
