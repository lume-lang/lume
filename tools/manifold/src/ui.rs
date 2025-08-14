use std::fmt;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::{Arc, LazyLock};

use build_stage::ManifoldDriver;
use error_snippet::IntoDiagnostic;
use lume_errors::{DiagCtx, Result};
use lume_span::{PackageId, SourceFile};
use owo_colors::{OwoColorize, Style};
use regex::Regex;
use similar::{ChangeTag, TextDiff};

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

    let mut stderr_new_path = stderr_path.clone();
    stderr_new_path.add_extension("new");

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

            if test_case.stderr_path.is_file() {
                let expected_output =
                    std::fs::read_to_string(&test_case.stderr_path).map_err(IntoDiagnostic::into_diagnostic)?;

                let expected_output = normalize_output(&expected_output);

                let diff = TextDiff::from_lines(&expected_output, &stderr_output);

                #[allow(clippy::float_cmp)]
                if diff.ratio() == 1.0 {
                    return Ok(TestResult::Success);
                }

                let write_failure_report: TestFailureCallback =
                    Box::new(move || print_diff_output(&test_case, &stderr_output, &expected_output).unwrap());

                Ok(TestResult::Failure { write_failure_report })
            } else {
                std::fs::write(&stderr_new_path, stderr_output).map_err(IntoDiagnostic::into_diagnostic)?;

                let write_failure_report: TestFailureCallback =
                    Box::new(move || format!("Review needed:  {}", stderr_new_path.display().cyan().underline()));

                Ok(TestResult::Failure { write_failure_report })
            }
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
    let stub_package = build_stage::stub_package_with(|pkg| pkg.add_source(Arc::new(source_file)));

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

    let buffer = dcx.render_buffer(&mut renderer).unwrap_or_default();

    normalize_output(&buffer)
}

struct Line(Option<usize>);

impl fmt::Display for Line {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            None => write!(f, "    "),
            Some(idx) => write!(f, "{:<4}", idx + 1),
        }
    }
}

fn normalize_output(value: &str) -> String {
    value
        .lines()
        .map(str::trim_end)
        .collect::<Vec<_>>()
        .join("\n")
        .trim()
        .to_string()
}

fn print_diff_output(test_case: &TestCase, actual: &str, expected: &str) -> std::io::Result<String> {
    let mut f = Vec::new();

    writeln!(
        &mut f,
        "Source file:    {}",
        test_case.path.display().cyan().underline()
    )?;

    writeln!(
        &mut f,
        "Snapshot file:  {}",
        test_case.stderr_path.display().cyan().underline()
    )?;

    writeln!(&mut f)?;

    let diff = TextDiff::from_lines(expected, actual);

    for (idx, group) in diff.grouped_ops(3).iter().enumerate() {
        if idx > 0 {
            writeln!(&mut f, "{:-^1$}", "-", 80)?;
        }

        for op in group {
            for change in diff.iter_inline_changes(op) {
                let (sign, s) = match change.tag() {
                    ChangeTag::Delete => ("-", Style::new().red()),
                    ChangeTag::Insert => ("+", Style::new().green()),
                    ChangeTag::Equal => (" ", Style::new().dimmed()),
                };

                write!(
                    &mut f,
                    "{}{} |{}",
                    Line(change.old_index()).dimmed(),
                    Line(change.new_index()).dimmed(),
                    sign.style(s).bold(),
                )?;

                for (emphasized, value) in change.iter_strings_lossy() {
                    if emphasized {
                        write!(&mut f, "{}", value.style(s).underline())?;
                    } else {
                        write!(&mut f, "{}", value.style(s))?;
                    }
                }

                if change.missing_newline() {
                    writeln!(&mut f)?;
                }
            }
        }
    }

    Ok(String::from_utf8_lossy(&f).to_string())
}
