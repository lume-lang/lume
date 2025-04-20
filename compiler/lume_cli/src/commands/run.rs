use crate::commands::project_or_cwd;
use crate::error::InvalidCliError;

use getopts::Options;
use lume_diag::Result;
use lume_driver::Driver;

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

    Driver::build_project(&std::path::PathBuf::from(input))?;

    Ok(0)
}
