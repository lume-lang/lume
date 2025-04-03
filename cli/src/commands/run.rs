use crate::Error;
use crate::commands::project_or_cwd;
use crate::error::CliError;

use compiler::Driver;
use getopts::Options;

pub(crate) fn run(args: &[String]) -> Result<i32, Error> {
    let options = Options::new();

    let matches = match options.parse(args) {
        Ok(matches) => matches,
        Err(err) => return Err(Error::CliError(CliError::ParsingError(err))),
    };

    let input: String = if let Some(v) = matches.free.first() {
        project_or_cwd(Some(v))?
    } else {
        project_or_cwd(None)?
    };

    let mut driver = Driver::new();

    let state = match driver.build_project(std::path::Path::new(&input)) {
        Ok(state) => state,
        Err(err) => return Err(Error::CompilerError(err)),
    };

    state.inspect();

    Ok(0)
}
