use std::process::Command;

use lume_driver::Driver;
use lume_errors::DiagCtxHandle;

use crate::commands::project_or_cwd;

#[derive(Debug, clap::Parser)]
#[command(name = "run", about = "Build and run a Lume package", long_about = None)]
pub struct RunCommand {
    #[command(flatten)]
    pub build: super::BuildOptions,

    #[arg(trailing_var_arg = true)]
    pub args: Vec<String>,
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

        let driver = match Driver::from_root(&std::path::PathBuf::from(project_path), dcx.clone()) {
            Ok(driver) => driver,
            Err(err) => {
                dcx.emit(err);
                return;
            }
        };

        match driver.build(self.build.options()) {
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
