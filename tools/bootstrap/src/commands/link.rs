use std::path::{Path, PathBuf};

use lume_cli_tools::*;
use lume_errors::{Result, SimpleDiagnostic};

use crate::fs;
use crate::toolchain::Profile;

#[derive(clap::Parser)]
#[command(
    name = "link",
    override_usage = "lume-bootstrap link [OPTIONS] [NAME] [PATH]",
    about = "Installs a toolchain from an existing source tree"
)]
pub struct LinkCommand {
    /// Name of the toolchain
    #[arg()]
    pub name: String,

    /// Path of the Lume source tree
    #[arg()]
    pub path: PathBuf,

    #[arg(long, default_value_t)]
    pub profile: Profile,

    /// Use the toolchain as the default
    #[arg(long, default_value_t)]
    pub use_as_default: bool,

    /// Don't update shell configuration
    #[arg(long)]
    pub skip_shellrc: bool,
}

impl LinkCommand {
    pub(crate) fn run(&self) -> Result<()> {
        if self.skip_shellrc && !self.use_as_default {
            warn!(
                "{} has no effect without {}\n",
                "--skip-shellrc".stylize("dim"),
                "--use-as-default".stylize("dim")
            );
        }

        if !is_lume_source_tree(&self.path) {
            return Err(SimpleDiagnostic::new(format!(
                "directory '{}' is not a Lume source tree",
                self.path.display()
            ))
            .into());
        }

        let toolchain_base = crate::toolchain::artifact_directory_for(&self.name)?;
        fs::create_dir(&toolchain_base)?;

        let opts = crate::toolchain::BuildOptions { profile: self.profile };

        let build_compiler = new_task("building Lume compiler...");
        match crate::run_dry(|| crate::toolchain::compile_compiler(&self.path, &opts)) {
            Ok(()) => {
                build_compiler.success("finished building Lume compiler");
            }
            Err(err) => {
                build_compiler.fail(format!("failed to build Lume compiler ({})", err.message()));
                return Err(err);
            }
        }

        let copy_artifacts = new_task("copying compiler artifacts...");
        match crate::run_dry(|| crate::toolchain::copy_artifacts(&self.path, &toolchain_base, &opts)) {
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
                    success!("set toolchain '{}' as active", self.name);
                }
                Err(err) => {
                    error!("failed to link toolchain: {}", err.message());
                }
            }
        }

        Ok(())
    }
}

/// Attempts to determine whether the given directory contains a Lume source
/// tree.
fn is_lume_source_tree(path: &Path) -> bool {
    let Ok(manifest) = std::fs::read_to_string(path.join("Cargo.toml")) else {
        return false;
    };

    manifest.contains("[package]\nname = \"lume\"")
}
