use crate::commands::project_or_cwd;
use crate::error::InvalidCliError;

use compiler::Driver;
use diag::Result;
use getopts::Options;

pub(crate) fn run(args: &[String]) -> Result<i32> {
    let options = Options::new();

    let matches = match options.parse(args) {
        Ok(matches) => matches,
        Err(err) => return Err(InvalidCliError { inner: err }.into()),
    };

    let input: String = if let Some(v) = matches.free.first() {
        project_or_cwd(Some(v))?
    } else {
        project_or_cwd(None)?
    };

    let mut driver = Driver::new();
    let state = driver.build_project(std::path::Path::new(&input))?;

    state.inspect();

    Ok(0)
}
