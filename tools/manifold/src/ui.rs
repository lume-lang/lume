use std::path::PathBuf;
use std::sync::LazyLock;

use lume_errors::{DiagCtx, MapDiagnostic, Result};
use owo_colors::OwoColorize;
use regex::Regex;

use crate::diff::render_dcx_output;
use crate::{TestFailureCallback, TestPath, TestResult};

pub(crate) struct TestCase {
    /// Absolute path to the test file itself.
    path: TestPath,

    /// Absolute path to the `.stderr` output.
    stderr_path: PathBuf,

    /// List of all the source files within the test.
    files: Vec<String>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum TestOutcome {
    Failure,
    Success,
}

pub(crate) fn run_test(path: TestPath) -> Result<TestResult> {
    let mut stderr_path = path.absolute.0.clone();
    stderr_path.set_extension("stderr");

    let mut stderr_new_path = path.absolute.0.clone();
    stderr_new_path.set_extension("stderr.new");

    let file_content = std::fs::read_to_string(&*path.absolute).map_diagnostic()?;
    let expected_test_outcome = determine_test_outcome(&file_content);

    let files = split_test_files(file_content);

    let test_case = TestCase {
        path,
        stderr_path,
        files,
    };

    let stderr_output = build_test_file(&test_case);

    match expected_test_outcome {
        TestOutcome::Failure => {
            if stderr_output.trim().is_empty() {
                let write_failure_report: TestFailureCallback = Box::new(move || {
                    format!(
                        "Expected failure, found success:  {}\n{stderr_output}",
                        test_case.path.relative.display().cyan().underline()
                    )
                });

                return Ok(TestResult::Failure { write_failure_report });
            }

            crate::diff::diff_output_of(stderr_output, test_case.path.relative.0, test_case.stderr_path)
        }
        TestOutcome::Success => {
            if stderr_output.trim().is_empty() {
                return Ok(TestResult::Success);
            }

            let write_failure_report: TestFailureCallback = Box::new(move || {
                format!(
                    "Expected success, found failure:  {}\n{stderr_output}",
                    test_case.path.relative.display().cyan().underline()
                )
            });

            Ok(TestResult::Failure { write_failure_report })
        }
    }
}

fn split_test_files(content: String) -> Vec<String> {
    static RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"//#\s*source-file").unwrap());

    RE.split(&content).map(ToString::to_string).collect()
}

fn determine_test_outcome(content: &str) -> TestOutcome {
    static RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^//#\s*test-outcome\s*=\s*(failure|success)").unwrap());

    let Some(captures) = RE.captures(content) else {
        return TestOutcome::Failure;
    };

    match &captures[1] {
        "failure" => TestOutcome::Failure,
        "success" => TestOutcome::Success,
        _ => panic!("unknown directory: test-outcome must be either 'failure' or 'success'"),
    }
}

fn build_test_file(test_case: &TestCase) -> String {
    let dcx = DiagCtx::new();
    let use_segmented_names = test_case.files.len() > 1;

    let mut builder = lume_driver::test_support::workspace(test_case.path.root.0.clone())
        .with_option(|opts| opts.enable_incremental = false)
        .with_file(
            "Arcfile",
            r#"
                [package]
                name = "<manifold-test>"
                version = "1.0.0"
                lume_version = "^0"

                allow_unsafe = true
            "#,
        );

    for (idx, content) in test_case.files.iter().enumerate() {
        let test_name = test_case.path.relative.file_name().unwrap();
        let file_base = test_name.to_str().unwrap().split('.').next().unwrap();

        let file_name = if use_segmented_names {
            PathBuf::from(&format!("{file_base}_{idx}.lm"))
        } else {
            PathBuf::from(test_name)
        };

        builder = builder.with_file(file_name, content);
    }

    if let Err(err) =
        || -> Result<lume_driver::TypeChecked> { builder.pipeline(dcx.handle())?.lower_to_hir()?.type_check() }()
    {
        dcx.emit(err);
    }

    if let Err(err) = dcx.ensure_untainted() {
        dcx.emit(err);
    }

    render_dcx_output(&dcx)
}
