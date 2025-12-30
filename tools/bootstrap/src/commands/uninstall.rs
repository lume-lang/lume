use lume_cli_tools::*;
use lume_errors::{Result, SimpleDiagnostic};
use owo_colors::Style;

use crate::fs;
use crate::toolchain::{self, active_toolchain};

#[derive(clap::Parser)]
#[command(
    name = "uninstall",
    override_usage = "lbs uninstall [OPTIONS] [TOOLCHAIN]",
    about = "Uninstall the given toolchain"
)]
pub struct UninstallCommand {
    /// Toolchain to uninstall
    #[arg(value_name = "TOOLCHAIN")]
    pub version: String,
}

impl UninstallCommand {
    pub(crate) fn run(&self) -> Result<()> {
        let toolchain_directory = toolchain::artifact_directory_for(&self.version)?;
        if !toolchain_directory.exists() {
            return Err(
                SimpleDiagnostic::new("toolchain is not installed; nothing to uninstall")
                    .with_severity(error_snippet::Severity::Warning)
                    .into(),
            );
        }

        // If we're uninstalling the active toolchain, remove the link first.
        if active_toolchain().is_some_and(|active| active.ends_with(&toolchain_directory)) {
            toolchain::unlink_toolchain()?;
        }

        let toolchain_source_dir = toolchain::download_directory_for(&self.version)?;

        task! {
            format!("removing toolchain {}", self.version) => {
                crate::run_dry(|| {
                    fs::remove_dir(&toolchain_directory)?;

                    if toolchain_source_dir.exists() {
                        fs::remove_dir(&toolchain_source_dir)?;
                    }

                    Ok(())
                })
            },
            Ok(()) => {
                if crate::verbose() > 0 {
                    format!(
                        "removed toolchain {} ({})",
                        self.version,
                        colorized!(toolchain_directory.display(), Style::new().dimmed())
                    )
                } else {
                    format!("removed toolchain {}", self.version)
                }
            },
            Err(err) => {
                format!("failed to remove toolchain ({})", err.message())
            }
        }?;

        if active_toolchain().is_none() {
            warn!("currently no active toolchain, since it was just uninstalled");
        }

        Ok(())
    }
}
