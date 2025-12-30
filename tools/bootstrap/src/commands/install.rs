use clap::CommandFactory;
use lume_cli_tools::*;
use lume_errors::Result;
use owo_colors::Style;

use crate::toolchain::{Profile, TargetVersion};
use crate::{Version, fs};

#[derive(clap::Parser)]
#[command(
    name = "install",
    override_usage = "lbs install [OPTIONS] [TOOLCHAIN]",
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
                colorized!("--skip-shellrc", Style::new().dimmed()),
                colorized!("--use-as-default", Style::new().dimmed()),
            );
        }

        let version = match TargetVersion::try_from(&self.version) {
            Ok(version) => version,
            Err(err) => {
                let mut cmd = InstallCommand::command();
                cmd.error(clap::error::ErrorKind::InvalidValue, err).exit();
            }
        };

        let compiler_root = task! {
            "cloning 'lume-lang/lume' compiler..." => {
                crate::run_dry(|| crate::git::clone(&version))
            },
            Ok(path) => {
                if crate::verbose() > 0 {
                    format!(
                        "cloned 'lume-lang/lume' ({})",
                        colorized!(path.display(), Style::new().dimmed())
                    )
                } else {
                    String::from("cloned 'lume-lang/lume'")
                }
            },
            Err(err) => {
                format!("failed to clone 'lume-lang/lume' ({})", err.message())
            }
        }?;

        let opts = crate::toolchain::BuildOptions { profile: self.profile };
        let toolchain_base = crate::toolchain::artifact_directory_for(&version)?;

        fs::create_dir(&toolchain_base)?;

        task! {
            "building Lume compiler..." => {
                crate::run_dry(|| crate::toolchain::compile_compiler(&compiler_root, &opts))
            },
            Ok(()) => "finished building Lume compiler",
            Err(err) => {
                format!("failed to build Lume compiler ({})", err.message())
            }
        }?;

        task! {
            "copying compiler artifacts..." => {
                crate::run_dry(|| crate::toolchain::copy_artifacts(&compiler_root, &toolchain_base, &opts))
            },
            Ok(()) => "copied all compiler artifacts",
            Err(err) => {
                format!("failed to copy artifact: {}", err.message())
            }
        }?;

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
