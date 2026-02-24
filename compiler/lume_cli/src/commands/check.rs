use std::path::PathBuf;

use lume_driver::{Config, Driver};
use lume_errors::DiagCtxHandle;

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
        let input = if let Some(v) = self.build.path.as_ref() {
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

        let config = Config {
            options: self.build.options(),
            ..Default::default()
        };

        let driver = match Driver::from_root(&PathBuf::from(project_path), config, dcx.clone()) {
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
