use std::io::Write;
use std::path::PathBuf;
use std::{fmt, path::Path};

use error_snippet::IntoDiagnostic;
use lume_errors::Result;
use owo_colors::{OwoColorize, Style};
use similar::{ChangeTag, TextDiff};

use crate::{TestFailureCallback, TestResult};

pub(crate) fn diff_output_of(output: String, path: PathBuf, output_path: PathBuf) -> Result<TestResult> {
    let new_extension = if let Some(existing_ext) = output_path.extension().and_then(|ext| ext.to_str()) {
        let mut ext = existing_ext.to_string();
        ext.push_str(".new");

        ext
    } else {
        String::from("new")
    };

    let mut output_path_new = output_path.clone();
    output_path_new.set_extension(new_extension);

    if output_path.is_file() {
        let expected_output = std::fs::read_to_string(&output_path).map_err(IntoDiagnostic::into_diagnostic)?;
        let expected_output = normalize_output(&expected_output);

        let diff = TextDiff::from_lines(&expected_output, &output);

        #[allow(clippy::float_cmp)]
        if diff.ratio() == 1.0 {
            return Ok(TestResult::Success);
        }

        std::fs::write(&output_path_new, output.clone()).map_err(IntoDiagnostic::into_diagnostic)?;

        let write_failure_report: TestFailureCallback =
            Box::new(move || print_diff_output(&path, &output_path, &output, &expected_output).unwrap());

        Ok(TestResult::Failure { write_failure_report })
    } else {
        std::fs::write(&output_path_new, output).map_err(IntoDiagnostic::into_diagnostic)?;

        let write_failure_report: TestFailureCallback =
            Box::new(move || format!("Review needed:  {}", output_path_new.display().cyan().underline()));

        Ok(TestResult::Failure { write_failure_report })
    }
}

pub(crate) fn normalize_output(value: &str) -> String {
    value
        .lines()
        .map(str::trim_end)
        .collect::<Vec<_>>()
        .join("\n")
        .trim()
        .to_string()
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

fn print_diff_output(input_path: &Path, expected_path: &Path, actual: &str, expected: &str) -> std::io::Result<String> {
    let mut f = Vec::new();

    writeln!(&mut f, "Source file:    {}", input_path.display().cyan().underline())?;
    writeln!(&mut f, "Snapshot file:  {}", expected_path.display().cyan().underline())?;
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
