use std::path::PathBuf;
use std::process::Command;

use lume_driver::{Config, Driver};
use lume_errors::DiagCtxHandle;
use lume_session::FileSystemLoader;

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
        let project_path = match project_or_cwd(self.build.path.as_ref()) {
            Ok(path) => PathBuf::from(path),
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
                    progress_bar.println("Compiling", format!("{} v{}", package.name, package.version));
                }
            },
        };

        let driver = match Driver::from_root(&project_path, config, callbacks, dcx.clone()) {
            Ok(driver) => driver,
            Err(err) => {
                dcx.emit(err);
                return;
            }
        };

        match driver.build() {
            Ok(exec) => {
                let relative_path = exec.binary.strip_prefix(&project_path).unwrap_or(&exec.binary);

                progress_bar.finish_with(
                    "Running",
                    format!(
                        "{} {}",
                        relative_path.display(),
                        self.args.iter().map(ToString::to_string).collect::<Vec<_>>().join(" ")
                    ),
                );

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
