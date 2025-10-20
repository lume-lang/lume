mod binary;
mod diff;
mod hir;
mod panic;
mod ui;

use std::io::Write;
use std::path::{MAIN_SEPARATOR_STR, Path, PathBuf};
use std::sync::Arc;

use error_snippet::{Result, SimpleDiagnostic};
use glob::glob;
use lume_errors::MapDiagnostic;
use owo_colors::OwoColorize;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

#[derive(Default, Debug, Clone, clap::Parser)]
#[clap(
    name = "manifold",
    version = env!("CARGO_PKG_VERSION"),
    about = "Lume's testing framework and regression checker",
    long_about = None
)]
pub struct Config {
    #[arg(help = "If specified, only run tests containing this string in their names")]
    pub test_names: Vec<String>,

    #[arg(long = "root", help = "Directory containing the test suite")]
    pub test_root: Option<PathBuf>,

    #[arg(long, help = "Run all tests sequentially instead of in parallel")]
    pub sequential: bool,
}

impl Config {
    /// Determines whether the test with the given path should be run.
    pub fn should_run_test(&self, path: &Path) -> bool {
        // If no filters were defined, all tests should be run.
        if self.test_names.is_empty() {
            return true;
        }

        self.test_names.iter().any(|name| path.to_string_lossy().contains(name))
    }
}

pub(crate) enum TestResult {
    /// The test was skipped, since it was no included in the test name filter.
    Skipped,

    /// The test succeeded.
    Success,

    /// The test failed - failure reason can be rendered using
    /// `write_failure_report`.
    Failure { write_failure_report: TestFailureCallback },
}

impl PartialEq for TestResult {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TestResult::Skipped, TestResult::Skipped)
            | (TestResult::Success, TestResult::Success)
            | (TestResult::Failure { .. }, TestResult::Failure { .. }) => true,
            (_, _) => false,
        }
    }
}

impl Eq for TestResult {}

pub(crate) type TestFailureCallback = Box<dyn FnOnce() -> String + Send + Sync>;

/// Main entrypoint for the Manifold CLI.
pub fn manifold_entry(config: Config) -> Result<()> {
    let test_root = if let Some(root) = config.test_root.clone() {
        root
    } else {
        find_test_root()?
    };

    run_test_suite(config, &test_root)
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
    /// HIR tests are stored in the `hir/` subdirectory and verify the lowered
    /// HIR maps of different Lume programs and packages.
    Hir,

    /// # Binary Tests
    ///
    /// Binary tests are stored in the `bin/` subdirectory and verify that an
    /// entire executable can be created from a given source file.
    /// Optionally, the output of the executed code will also be verified.
    Binary,
}

fn run_test_suite(config: Config, root: &PathBuf) -> Result<()> {
    let glob_pattern_str = format!("{}/**/*.lm", root.display());
    let glob_pattern = glob(&glob_pattern_str).expect("should have valid glob pattern");

    let files: Vec<PathBuf> = glob_pattern
        .collect::<std::result::Result<Vec<_>, _>>()
        .map_cause("could not collect test files")?;

    panic::install_panic_hook();

    let run_test_file = |test_file_path: PathBuf| {
        let relative_path = test_file_path
            .strip_prefix(root)
            .expect("expected test path to contain root folder");

        if !config.should_run_test(relative_path) {
            return Ok(TestResult::Skipped);
        }

        let test_type = determine_test_type(root, &test_file_path)?;

        panic::set_capture_buf(Arc::default());

        if let Ok(result) = std::panic::catch_unwind(|| run_single_test(test_type, test_file_path.clone())) {
            return result;
        }

        let panic_buf = if let Some(buffer) = panic::take_capture_buf() {
            let buffer = buffer.lock().unwrap_or_else(|e| e.into_inner());

            Some(buffer.clone())
        } else {
            None
        };

        let mut f = Vec::new();

        writeln!(&mut f, "Panic occured during test")?;
        writeln!(
            &mut f,
            "Source file:    {}",
            test_file_path.display().cyan().underline()
        )?;

        if let Some(panic_msg) = panic_buf {
            writeln!(&mut f, "\n{panic_msg}")?;
        }

        let report = String::from_utf8_lossy(&f).to_string();

        Ok(TestResult::Failure {
            write_failure_report: Box::new(|| report),
        })
    };

    let results: Vec<TestResult> = if config.sequential {
        files.into_iter().map(run_test_file).collect::<Result<Vec<_>>>()?
    } else {
        files.into_par_iter().map(run_test_file).collect::<Result<Vec<_>>>()?
    };

    let success_count = results.iter().fold(0_usize, |cnt, item| {
        if matches!(item, TestResult::Success) {
            cnt + 1
        } else {
            cnt
        }
    });

    let failure_count = results.iter().fold(0_usize, |cnt, item| {
        if matches!(item, TestResult::Failure { .. }) {
            cnt + 1
        } else {
            cnt
        }
    });

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

fn run_single_test(test_type: ManifoldTestType, test_file_path: PathBuf) -> Result<TestResult> {
    Ok(match test_type {
        ManifoldTestType::Ui => ui::run_test(test_file_path)?,
        ManifoldTestType::Hir => hir::run_test(test_file_path)?,
        ManifoldTestType::Binary => binary::run_test(test_file_path)?,
    })
}

/// Attempts to determine the test type from the path the file is declared
/// within.
///
/// # Test subdirectories
/// - **UI tests** are stored in the `ui/` subdirectory and are used to verify
///   that the reporting facilities and diagnostics are identical between
///   changes in the Lume compiler.
fn determine_test_type(root: &PathBuf, path: &Path) -> Result<ManifoldTestType> {
    let relative_path = path.strip_prefix(root).unwrap();
    let relative_path_str = relative_path.display().to_string();

    let subfolder = relative_path_str.split(MAIN_SEPARATOR_STR).next();

    match subfolder {
        Some("ui") => Ok(ManifoldTestType::Ui),
        Some("hir") => Ok(ManifoldTestType::Hir),
        Some("bin") => Ok(ManifoldTestType::Binary),
        _ => Err(SimpleDiagnostic::new(format!("could not determine type of test: {relative_path_str}")).into()),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn manifold_tests() {
        manifold_entry(Config::default()).unwrap();
    }
}
