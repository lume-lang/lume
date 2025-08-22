use std::path::{Path, PathBuf};
use std::sync::{Arc, LazyLock};

use build_stage::ManifoldDriver;
use error_snippet::IntoDiagnostic;
use lume_errors::{DiagCtx, Result};
use lume_span::{PackageId, SourceFile};
use owo_colors::OwoColorize;
use regex::Regex;

use crate::diff::normalize_output;
use crate::{TestFailureCallback, TestResult};

pub(crate) struct TestCase {
    path: PathBuf,
    stderr_path: PathBuf,

    file_content: String,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum TestOutcome {
    Failure,
    Success,
}

pub(crate) fn run_test(path: PathBuf) -> Result<TestResult> {
    let mut stderr_path = path.clone();
    stderr_path.set_extension("stderr");

    let mut stderr_new_path = path.clone();
    stderr_new_path.set_extension("stderr.new");

    let file_content = std::fs::read_to_string(&path).map_err(IntoDiagnostic::into_diagnostic)?;

    let test_case = TestCase {
        path,
        stderr_path,
        file_content,
    };

    let stderr_output = build_test_file(&test_case);
    let expected_test_outcome = determine_test_outcome(&test_case);

    match expected_test_outcome {
        TestOutcome::Failure => {
            if stderr_output.trim().is_empty() {
                let write_failure_report: TestFailureCallback = Box::new(move || {
                    format!(
                        "Expected failure, found success:  {}\n{stderr_output}",
                        test_case.path.display().cyan().underline()
                    )
                });

                return Ok(TestResult::Failure { write_failure_report });
            }

            crate::diff::diff_output_of(stderr_output, test_case.path, test_case.stderr_path)
        }
        TestOutcome::Success => {
            if stderr_output.trim().is_empty() {
                return Ok(TestResult::Success);
            }

            let write_failure_report: TestFailureCallback = Box::new(move || {
                format!(
                    "Expected success, found failure:  {}\n{stderr_output}",
                    test_case.path.display().cyan().underline()
                )
            });

            Ok(TestResult::Failure { write_failure_report })
        }
    }
}

fn determine_test_outcome(test_case: &TestCase) -> TestOutcome {
    static RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^//#\s*test-outcome\s*=\s*(failure|success)").unwrap());

    let Some(captures) = RE.captures(&test_case.file_content) else {
        return TestOutcome::Failure;
    };

    match &captures[1] {
        "failure" => TestOutcome::Failure,
        "success" => TestOutcome::Success,
        _ => panic!("unknown directory: test-outcome must be either 'failure' or 'success'"),
    }
}

fn build_test_file(test_case: &TestCase) -> String {
    let file_name = Path::new(test_case.path.file_name().unwrap());

    let source_file = SourceFile::new(PackageId::empty(), file_name, test_case.file_content.clone());
    let mut stub_package = build_stage::stub_package_with(|pkg| pkg.add_source(Arc::new(source_file)));
    stub_package.add_std_sources();

    let dcx = DiagCtx::new();
    let manifold_driver = ManifoldDriver::new(stub_package, dcx.clone());

    if let Err(err) = manifold_driver.type_check() {
        dcx.emit(err);
    }

    if let Err(err) = dcx.ensure_untainted() {
        dcx.emit(err);
    }

    render_dcx_output(&dcx)
}

fn render_dcx_output(dcx: &DiagCtx) -> String {
    let mut renderer = error_snippet::GraphicalRenderer::new();
    renderer.use_colors = false;
    renderer.highlight_source = false;

    owo_colors::set_override(false);
    let buffer = dcx.render_buffer(&mut renderer).unwrap_or_default();

    normalize_output(&buffer)
}
