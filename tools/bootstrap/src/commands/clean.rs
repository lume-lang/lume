use lume_cli_tools::task;
use lume_errors::{Result, SimpleDiagnostic};

use crate::{fs, toolchain};

#[derive(clap::Parser)]
#[command(
    name = "clean",
    override_usage = "lbs clean [OPTIONS] [TOOLCHAIN...]",
    about = "Removes temporary data from toolchain(s)"
)]
pub struct CleanCommand {
    /// Toolchains to clean up. Defaults to all toolchains
    #[arg(value_name = "TOOLCHAIN")]
    pub toolchains: Option<Vec<String>>,

    /// Ignore missing toolchains
    #[arg(long, default_value_t)]
    pub ignore_missing: bool,
}

impl CleanCommand {
    pub(crate) fn run(&self) -> Result<()> {
        let toolchains = if let Some(toolchains) = &self.toolchains {
            toolchains.clone()
        } else {
            toolchain::downloaded_toolchains()?
        };

        for toolchain in &toolchains {
            let toolchain_root = crate::toolchain::download_directory_for(toolchain)?;

            if !toolchain_root.exists() {
                if self.ignore_missing {
                    continue;
                }

                return Err(SimpleDiagnostic::new(format!("toolchain could not be found: {toolchain}")).into());
            }

            task! {
                format!("deleting toolchain {}", toolchain) => {
                    crate::run_dry(|| fs::remove_dir(&toolchain_root))
                },
                Ok(()) => format!("deleted toolchain {toolchain}"),
                Err(err) => {
                    format!("failed to delete toolchain {toolchain} ({})", err.message())
                }
            }?;
        }

        Ok(())
    }
}
