use lume_errors::{Result, SimpleDiagnostic};

#[derive(clap::Parser)]
#[command(
    name = "use",
    override_usage = "lbs use [OPTIONS] [TOOLCHAIN]",
    about = "Sets the active toolchain"
)]
pub struct UseCommand {
    /// Toolchain to set as active
    #[arg(value_name = "TOOLCHAIN")]
    pub version: String,
}

impl UseCommand {
    pub(crate) fn run(&self) -> Result<()> {
        let toolchain_base = crate::toolchain::artifact_directory_for(&self.version)?;
        if !toolchain_base.exists() {
            return Err(
                SimpleDiagnostic::new(format!("toolchain '{}' is not installed", self.version))
                    .with_help("use `lbs install [TOOLCHAIN]` to install it")
                    .into(),
            );
        }

        match crate::run_dry(|| crate::toolchain::link_toolchain(&toolchain_base)) {
            Ok(()) => {
                success!("set toolchain '{}' as active", self.version);
            }
            Err(err) => {
                error!("failed to link toolchain: {}", err.message());
            }
        }

        Ok(())
    }
}
