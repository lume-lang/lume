use clap::{Arg, ArgAction, ArgMatches, Command};
use lume_errors::DiagCtxHandle;

pub(crate) fn command() -> Command {
    Command::new("clean")
        .about("Purges the local cache for Lume packages")
        .arg(
            Arg::new("dry-run")
                .short('n')
                .long("dry-run")
                .help("Execute the command without deleting anything")
                .action(ArgAction::SetTrue),
        )
}

pub(crate) fn run(args: &ArgMatches, mut dcx: DiagCtxHandle) {
    let dry_run = args.get_flag("dry-run");

    if let Err(err) = arc::clean_local_cache_dir(dry_run) {
        dcx.emit(err);
    }
}
