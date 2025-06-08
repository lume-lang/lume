use crate::commands::project_or_cwd;

use clap::{Arg, ArgAction, ArgMatches, Command};
use error_snippet::Result;
use lume_driver::Driver;

pub(crate) fn command() -> Command {
    Command::new("run")
        .about("Build and run a Lume project")
        .arg(Arg::new("path").help("Path to the project").action(ArgAction::Set))
        .arg(
            Arg::new("print-type-ctx")
                .long("print-type-ctx")
                .help("Print the type context before analyzing")
                .action(ArgAction::SetTrue),
        )
}

pub(crate) fn run(args: &ArgMatches) -> Result<()> {
    let input: String = if let Some(v) = args.get_one::<String>("path") {
        project_or_cwd(Some(v))?
    } else {
        project_or_cwd(None)?
    };

    let mut driver = Driver::from_root(&std::path::PathBuf::from(input))?;
    driver.options.print_type_context = args.get_flag("print-type-ctx");

    driver.build()?;

    Ok(())
}
