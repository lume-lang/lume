use std::path::PathBuf;
use std::process::Command;

use lume_driver::{Config, Driver};
use lume_errors::DiagCtxHandle;

use crate::commands::project_or_cwd;

#[derive(Debug, clap::Parser)]
#[command(name = "run", about = "Build and run a Lume package", long_about = None)]
pub struct RunCommand {
    /// Arguments for the binary to run
    #[arg(trailing_var_arg = true, index = 2)]
    pub args: Vec<String>,

    #[command(flatten)]
    pub build: crate::opts::BuildOptions,
}

impl RunCommand {
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
                dcx.emit(err);
                return;
            }
        };

        match driver.build() {
            Ok(exec) => {
                let exit_code = Command::new(exec.binary)
                    .args(&self.args)
                    .spawn()
                    .and_then(|mut proc| proc.wait())
                    .map_or(-1, |exit| exit.code().unwrap_or_default());

                std::process::exit(exit_code)
            }
            Err(err) => dcx.emit_and_push(err),
        }
    }
}
