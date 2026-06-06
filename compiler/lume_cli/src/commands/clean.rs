use std::path::PathBuf;

use clap::ValueHint;
use lume_driver::{Config, Driver};
use lume_errors::{DiagCtxHandle, IntoDiagnostic};
use lume_session::FileSystemLoader;

use crate::commands::project_or_cwd;

#[derive(Debug, clap::Parser)]
#[command(name = "clean", about = "Cleans up build artifacts from Lume package", long_about = None)]
pub struct CleanCommand {
    /// Path to the project
    #[arg(value_name = "DIR", value_hint = ValueHint::DirPath)]
    pub path: Option<PathBuf>,

    #[arg(short = 'n', long, help = "Execute the command without deleting anything")]
    pub dry_run: bool,
}

impl CleanCommand {
    #[allow(clippy::needless_pass_by_value)]
    pub(crate) fn run(&self, dcx: DiagCtxHandle) {
        let project_path = match project_or_cwd(self.path.as_ref()) {
            Ok(path) => path,
            Err(err) => {
                dcx.emit(err);
                return;
            }
        };

        let config = Config::<FileSystemLoader>::default();
        let progress = lume_cli_tools::progress_bar().with_prefix("Cleaning");

        match Driver::from_root(
            &PathBuf::from(project_path),
            config,
            lume_driver::Callbacks::default(),
            dcx.clone(),
        ) {
            Ok(driver) => {
                let pipeline = driver.to_pipeline();
                let obj_path = pipeline.obj_path();

                tracing::trace!("attempting to remove `{}`...", obj_path.display());
                progress.println("Clean", &pipeline.session.dep_graph.root_package().name);

                if self.dry_run || std::fs::exists(&obj_path).ok() == Some(false) {
                    return;
                }

                if let Err(err) = std::fs::remove_dir_all(obj_path) {
                    dcx.emit_and_push(err.into_diagnostic());
                }
            }
            Err(err) => {
                dcx.emit_and_push(err);
            }
        }
    }
}
