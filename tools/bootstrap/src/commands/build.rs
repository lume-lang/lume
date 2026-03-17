use clap::CommandFactory;
use lume_cli_tools::*;
use lume_errors::Result;

use crate::toolchain::{Profile, TargetVersion};
use crate::{Version, fs};

#[derive(clap::Parser)]
#[command(
    name = "build",
    override_usage = "lume-bootstrap build [OPTIONS] [TOOLCHAIN]",
    about = "Builds the given toolchain from source"
)]
pub struct BuildCommand {
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

impl BuildCommand {
    pub(crate) fn run(&self) -> Result<()> {
        if self.skip_shellrc && !self.use_as_default {
            warn!(
                "{} has no effect without {}\n",
                "--skip-shellrc".stylize("dim"),
                "--use-as-default".stylize("dim")
            );
        }

        let version = match TargetVersion::try_from(&self.version) {
            Ok(version) => version,
            Err(err) => {
                let mut cmd = Self::command();
                cmd.error(clap::error::ErrorKind::InvalidValue, err).exit();
            }
        };

        let toolchain_base = crate::toolchain::artifact_directory_for(&version)?;
        fs::create_dir(&toolchain_base)?;

        let clone_compiler = new_task("cloning 'lume-lang/lume' compiler...");
        let compiler_root = match crate::run_dry(|| crate::git::clone(&version)) {
            Ok(path) => {
                let message = if crate::verbose() > 0 {
                    format!("cloned 'lume-lang/lume' ({})", path.display().stylize("dim"))
                } else {
                    String::from("cloned 'lume-lang/lume'")
                };

                clone_compiler.success(message);

                path
            }
            Err(err) => {
                clone_compiler.fail(format!("failed to clone 'lume-lang/lume' ({})", err.message()));
                return Err(err);
            }
        };

        let opts = crate::toolchain::BuildOptions { profile: self.profile };

        let build_compiler = new_task("building Lume compiler...");
        match crate::run_dry(|| crate::toolchain::compile_compiler(&compiler_root, &opts)) {
            Ok(()) => {
                build_compiler.success("finished building Lume compiler");
            }
            Err(err) => {
                build_compiler.fail(format!("failed to build Lume compiler ({})", err.message()));
                return Err(err);
            }
        }

        let copy_artifacts = new_task("copying compiler artifacts...");
        match crate::run_dry(|| crate::toolchain::copy_artifacts(&compiler_root, &toolchain_base, &opts)) {
            Ok(()) => {
                copy_artifacts.success("copied all compiler artifacts");
            }
            Err(err) => {
                copy_artifacts.fail(format!("failed to copy artifact: {}", err.message()));
                return Err(err);
            }
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
