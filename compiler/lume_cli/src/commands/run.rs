use crate::commands::project_or_cwd;

use clap::{Arg, ArgAction, ArgMatches, Command};
use lume_driver::Driver;
use lume_errors::DiagCtxHandle;
use lume_session::Options;

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
        .arg(
            Arg::new("print-mir")
                .long("print-mir")
                .help("Print the generated MIR")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("print-llvm-ir")
                .long("print-llvm-ir")
                .help("Print the generated LLVM IR")
                .action(ArgAction::SetTrue),
        )
}

#[allow(clippy::needless_pass_by_value)]
pub(crate) fn run(args: &ArgMatches, dcx: DiagCtxHandle) {
    let input = if let Some(v) = args.get_one::<String>("path") {
        project_or_cwd(Some(v))
    } else {
        project_or_cwd(None)
    };

    let project_path = match input {
        Ok(path) => path,
        Err(err) => {
            dcx.emit(err);
            return;
        }
    };

    let options = Options {
        print_type_context: args.get_flag("print-type-ctx"),
        print_mir: args.get_flag("print-mir"),
        print_llvm_ir: args.get_flag("print-llvm-ir"),
    };

    let driver = match Driver::from_root(&std::path::PathBuf::from(project_path), dcx.clone()) {
        Ok(driver) => driver,
        Err(err) => {
            dcx.emit(err);
            return;
        }
    };

    if let Err(err) = driver.build(options) {
        dcx.emit(err);
    }
}
