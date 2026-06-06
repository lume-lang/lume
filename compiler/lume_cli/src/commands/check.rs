use std::path::PathBuf;

use lume_driver::{Config, Driver};
use lume_errors::DiagCtxHandle;
use lume_session::FileSystemLoader;

use crate::commands::project_or_cwd;

#[derive(Debug, clap::Parser)]
#[command(name = "check", about = "Check a Lume package", long_about = None)]
pub struct CheckCommand {
    #[command(flatten)]
    pub build: crate::opts::BuildOptions,
}

impl CheckCommand {
    #[allow(clippy::needless_pass_by_value)]
    pub(crate) fn run(&self, dcx: DiagCtxHandle) {
        let project_path = match project_or_cwd(self.build.path.as_ref()) {
            Ok(path) => path,
            Err(err) => {
                dcx.emit(err);
                return;
            }
        };

        let progress_bar = lume_cli_tools::progress_bar().hidden().with_prefix("Building");

        let config = Config::<FileSystemLoader> {
            options: self.build.options(),
            ..Default::default()
        };

        let callbacks = lume_driver::Callbacks {
            arc_event: &|event| {
                if let lume_driver::ArcEvent::PackagesLoaded { graph } = event {
                    progress_bar.set_length(graph.packages.len() as u64);
                    progress_bar.show();
                }
            },
            on_package_state_change: &|_, state| match state {
                lume_driver::PackageState::CompilationCached { .. } => {
                    progress_bar.inc(1);
                }
                lume_driver::PackageState::CompilationStarted { package } => {
                    progress_bar.inc(1);
                    progress_bar.println("Checking", format!("{} v{}", package.name, package.version));
                }
            },
        };

        let driver = match Driver::from_root(&PathBuf::from(project_path), config, callbacks, dcx.clone()) {
            Ok(driver) => driver,
            Err(err) => {
                dcx.emit_and_push(err);
                return;
            }
        };

        if let Err(err) = driver.check() {
            dcx.emit_and_push(err);
        }
    }
}
