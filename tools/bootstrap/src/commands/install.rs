use clap::CommandFactory;
use lume_cli_tools::*;
use lume_errors::{Result, SimpleDiagnostic};

use crate::toolchain::{Profile, TargetVersion};
use crate::{Version, fs};

#[derive(clap::Parser)]
#[command(
    name = "install",
    override_usage = "lume-bootstrap install [OPTIONS] [TOOLCHAIN]",
    about = "Install or update the given toolchain"
)]
pub struct InstallCommand {
    #[arg(long, default_value_t)]
    pub profile: Profile,

    #[command(flatten)]
    pub version: Version,

    /// Use the toolchain as the default
    #[arg(long, default_value_t)]
    pub use_as_default: bool,

    /// Don't update shell configuration
    #[arg(long)]
    pub skip_shellrc: bool,
}

impl InstallCommand {
    pub(crate) fn run(&self) -> Result<()> {
        if self.skip_shellrc && !self.use_as_default {
            warn!(
                "{} has no effect without {}\n",
                "--skip-shellrc".stylize("dim"),
                "--use-as-default".stylize("dim"),
            );
        }

        let version = match TargetVersion::try_from(&self.version) {
            Ok(version) => version,
            Err(err) => {
                let mut cmd = InstallCommand::command();
                cmd.error(clap::error::ErrorKind::InvalidValue, err).exit();
            }
        };

        let toolchain_base = crate::toolchain::artifact_directory_for(&version)?;
        fs::create_dir(&toolchain_base)?;

        if let Some(binbuild_tag) = crate::fetch::binbuild_version_of(&version)? {
            let task_download_binaries = new_task("downloading binaries...");
            let download_root = match crate::run_dry(|| crate::fetch::fetch(&binbuild_tag)) {
                Ok(path) => {
                    let message = if crate::verbose() > 0 {
                        format!("fetched lume binaries ({})", path.display().stylize("dim"))
                    } else {
                        String::from("fetched lume binaries")
                    };

                    task_download_binaries.success(message);
                    path
                }
                Err(err) => {
                    task_download_binaries.fail(format!("failed to download binaries: {}", err.message()));
                    return Err(err);
                }
            };

            let task_copy_binaries = new_task("copying compiler artifacts...");
            match crate::run_dry(|| crate::fetch::copy_artifacts(&download_root, &toolchain_base)) {
                Ok(()) => {
                    task_copy_binaries.success("copied all compiler artifacts");
                }
                Err(err) => {
                    task_copy_binaries.fail(format!("failed to copy artifact: {}", err.message()));
                    return Err(err);
                }
            }
        } else {
            return Err(SimpleDiagnostic::new(format!(
                "no binary version available for {version}; source-build disabled"
            ))
            .into());
        }

        if self.use_as_default {
            match crate::run_dry(|| crate::toolchain::link_toolchain(&toolchain_base, self.skip_shellrc)) {
                Ok(()) => {
                    success!("set toolchain '{version}' as active");
                }
                Err(err) => {
                    error!("failed to link toolchain: {}", err.message());
                }
            }
        }

        Ok(())
    }
}
