use crate::commands::project_or_cwd;

use lume_driver::Driver;
use lume_errors::DiagCtxHandle;

#[derive(Debug, clap::Parser)]
#[command(name = "build", about = "Build a Lume package", long_about = None)]
pub struct BuildCommand {
    #[command(flatten)]
    pub build: super::BuildOptions,
}

impl BuildCommand {
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
                dcx.emit_and_push(err);
                return;
            }
        };

        if let Err(err) = driver.build(self.build.options()) {
            dcx.emit_and_push(err);
        }
    }
}
