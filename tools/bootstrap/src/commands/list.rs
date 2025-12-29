use lume_errors::Result;

use crate::toolchain;

#[derive(clap::Parser)]
#[command(
    name = "list",
    override_usage = "lbs list [OPTIONS]",
    about = "Lists installed toolchains"
)]
pub struct ListCommand {
    /// List downloaded toolchains instead of installed
    #[arg(long, default_value_t)]
    pub downloaded: bool,
}

impl ListCommand {
    pub(crate) fn run(&self) -> Result<()> {
        let toolchains = if self.downloaded {
            toolchain::downloaded_toolchains()?
        } else {
            toolchain::installed_toolchains()?
        };

        #[allow(clippy::disallowed_macros, reason = "used in CLI")]
        for toolchain in toolchains {
            println!("{toolchain}");
        }

        Ok(())
    }
}
