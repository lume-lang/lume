use crate::commands::project_or_cwd;
use crate::error::InvalidCliError;

use error_snippet::{IntoDiagnostic, Result};
use getopts::Options;
use lume_driver::Driver;

pub(crate) fn run(args: &[String]) -> Result<()> {
    let mut options = Options::new();

    options.optflag("", "print-type-ctx", "print the type context before analyzing");

    let matches = match options.parse(args) {
        Ok(matches) => matches,
        Err(err) => {
            return Err(InvalidCliError {
                inner: vec![err.into_diagnostic()],
            }
            .into());
        }
    };

    let input: String = if let Some(v) = matches.free.first() {
        project_or_cwd(Some(v))?
    } else {
        project_or_cwd(None)?
    };

    let mut driver = Driver::from_root(&std::path::PathBuf::from(input))?;
    driver.options.print_type_context = matches.opt_present("print-type-ctx");

    driver.build()?;

    Ok(())
}
