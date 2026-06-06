use std::path::PathBuf;

use lume_cli_tools::Stylable;
use lume_driver::{Config, Driver};
use lume_errors::DiagCtxHandle;
use lume_session::FileSystemLoader;

use crate::commands::project_or_cwd;

#[derive(Debug, clap::Parser)]
#[command(name = "build", about = "Build a Lume package", long_about = None)]
pub struct BuildCommand {
    #[command(flatten)]
    pub build: crate::opts::BuildOptions,
}

impl BuildCommand {
    #[allow(clippy::needless_pass_by_value)]
    pub(crate) fn run(&self, dcx: DiagCtxHandle) {
        let project_path = match project_or_cwd(self.build.path.as_ref()) {
            Ok(path) => PathBuf::from(path),
            Err(err) => {
                dcx.emit(err);
                return;
            }
        };

        let compile_progress = lume_cli_tools::progress_bar().hidden().with_prefix("Building");

        let config = Config::<FileSystemLoader> {
            options: self.build.options(),
            ..Default::default()
        };

        let callbacks = lume_driver::Callbacks {
            arc_event: &|event| {
                if let lume_driver::ArcEvent::PackagesLoaded { graph } = event {
                    compile_progress.set_length(graph.packages.len() as u64);
                    compile_progress.show();
                }
            },
            on_package_state_change: &|_, state| match state {
                lume_driver::PackageState::CompilationCached { .. } => {
                    compile_progress.inc(1);
                }
                lume_driver::PackageState::CompilationStarted { package } => {
                    compile_progress.inc(1);
                    compile_progress.println("Compiling", format!("{} v{}", package.name, package.version));
                }
            },
        };

        let driver = match Driver::from_root(&project_path, config, callbacks, dcx.clone()) {
            Ok(driver) => driver,
            Err(err) => {
                dcx.emit_and_push(err);
                return;
            }
        };

        if let Err(err) = driver.build() {
            dcx.emit_and_push(err);
            return;
        }

        let relative_path = if let Ok(cwd) = std::env::current_dir() {
            project_path.strip_prefix(cwd).unwrap_or(&project_path)
        } else {
            &project_path
        };

        compile_progress.println(
            "",
            format!(
                "\n{:>13} You can now run the program using {}",
                "Success!".stylize("accent"),
                format!(
                    "lume run {}",
                    if relative_path.as_os_str() == "." {
                        String::new()
                    } else {
                        relative_path.display().to_string()
                    }
                )
                .stylize("secondary.bold"),
            ),
        );
    }
}
