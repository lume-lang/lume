use lume_errors::DiagCtxHandle;

#[derive(Debug, clap::Parser)]
#[command(name = "clean", about = "Purges the local cache for Lume packages", long_about = None)]
pub struct ArcCleanCommand {
    #[arg(short = 'n', long, help = "Execute the command without deleting anything")]
    pub dry_run: bool,
}

impl ArcCleanCommand {
    #[allow(clippy::needless_pass_by_value)]
    pub(crate) fn run(&self, dcx: DiagCtxHandle) {
        if let Err(err) = arc::clean_local_cache_dir(self.dry_run) {
            dcx.emit(err);
        }
    }
}
