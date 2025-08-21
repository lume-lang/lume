#![feature(formatting_options)]
#![feature(path_add_extension)]

mod diff;
mod hir;
mod ui;

use std::path::{MAIN_SEPARATOR_STR, Path, PathBuf};

use error_snippet::{IntoDiagnostic, Result, SimpleDiagnostic};
use glob::glob;
use owo_colors::OwoColorize;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

pub(crate) enum TestResult {
    Success,
    Failure { write_failure_report: TestFailureCallback },
}

impl PartialEq for TestResult {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TestResult::Success, TestResult::Success) | (TestResult::Failure { .. }, TestResult::Failure { .. }) => {
                true
            }
            (_, _) => false,
        }
    }
}

impl Eq for TestResult {}

pub(crate) type TestFailureCallback = Box<dyn FnOnce() -> String + Send + Sync>;

/// Main entrypoint for the Manifold CLI.
pub fn manifold_entry() -> Result<()> {
    let test_root = find_test_root()?;

    run_test_suite(&test_root)
}

/// Attempts to find the root of the compiler project.
fn find_compiler_root() -> PathBuf {
    let manifest_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let compiler_root = manifest_path.parent().unwrap().parent().unwrap();

    compiler_root.to_path_buf()
}

/// Attempts to find the root of the `tests` folder in the compiler project.
fn find_test_root() -> Result<PathBuf> {
    let compiler_root = find_compiler_root();
    let test_root = compiler_root.join("tests");

    if !test_root.is_dir() {
        return Err(SimpleDiagnostic::new("could not find test root")
            .add_cause(SimpleDiagnostic::new(format!(
                "could not find directory {}",
                test_root.display()
            )))
            .into());
    }

    Ok(test_root)
}

/// Represents the type of a given Manifold test.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum ManifoldTestType {
    /// # UI Tests
    ///
    /// UI tests are stored in the `ui/` subdirectory and verify the console
    /// output of different Lume programs and packages. UI tests are mostly
    /// used to verify that diagnostic messages don't change when implementing
    /// a change in the compiler.
    Ui,

    /// # HIR Tests
    ///
    /// HIR tests are stored in the `hir/` subdirectory and verify the lowered HIR
    /// maps of different Lume programs and packages.
    Hir,
}

fn run_test_suite(root: &PathBuf) -> Result<()> {
    let glob_pattern_str = format!("{}/**/*.lm", root.display());
    let glob_pattern = glob(&glob_pattern_str).expect("should have valid glob pattern");

    let files: Vec<PathBuf> = glob_pattern
        .collect::<std::result::Result<Vec<_>, _>>()
        .map_err(IntoDiagnostic::into_diagnostic)?;

    let results = files
        .into_par_iter()
        .filter(|test_file_path| test_file_path.is_file())
        .map(|test_file_path| {
            let test_type = determine_test_type(root, &test_file_path)?;

            let status = match test_type {
                ManifoldTestType::Ui => ui::run_test(test_file_path)?,
                ManifoldTestType::Hir => hir::run_test(test_file_path)?,
            };

            Ok(status)
        })
        .collect::<Result<Vec<_>>>()?;

    let success_count = results.iter().fold(0_usize, |cnt, item| {
        if matches!(item, TestResult::Success) {
            cnt + 1
        } else {
            cnt
        }
    });

    let failure_count = results.len() - success_count;

    if failure_count > 0 {
        for result in results {
            let TestResult::Failure { write_failure_report } = result else {
                continue;
            };

            let report = write_failure_report();

            eprintln!();
            eprintln!("{}", "=== Test failed ===".red());
            eprintln!("{report}");
        }

        eprintln!("test result: {}", "FAILURE".red());
    } else {
        println!("test result: {}", "SUCCESS".green());
    }

    eprintln!("tests passed: {success_count}, tests failed: {failure_count}");

    Ok(())
}

/// Attempts to determine the test type from the path the file is declared within.
///
/// # Test subdirectories
/// - **UI tests** are stored in the `ui/` subdirectory and are used to verify
///   that the reporting facilities and diagnostics are identical between changes
///   in the Lume compiler.
fn determine_test_type(root: &PathBuf, path: &Path) -> Result<ManifoldTestType> {
    let relative_path = path.strip_prefix(root).unwrap();
    let relative_path_str = relative_path.display().to_string();

    let subfolder = relative_path_str.split(MAIN_SEPARATOR_STR).next();

    match subfolder {
        Some("ui") => Ok(ManifoldTestType::Ui),
        Some("hir") => Ok(ManifoldTestType::Hir),
        _ => Err(SimpleDiagnostic::new(format!("could not determine type of test: {relative_path_str}")).into()),
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn manifold_tests() {
        super::manifold_entry().unwrap();
    }
}
