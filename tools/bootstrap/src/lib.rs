use std::sync::LazyLock;
use std::sync::atomic::AtomicBool;

use clap::Parser;
use lume_errors::{DiagCtx, Result};
use regex::Regex;

use crate::toolchain::TargetVersion;

pub(crate) mod cmd;
pub(crate) mod fs;
pub(crate) mod home;
pub(crate) mod toolchain;

#[macro_use]
pub(crate) mod spinner;

mod commands {
    pub mod clean;
    pub mod install;
    pub mod list;
    pub mod uninstall;
    pub mod use_;
}

pub(crate) mod env {
    cfg_if::cfg_if! {
        if #[cfg(target_os = "macos")] {
            pub mod macos;
            pub use macos::LUME_ENV;
        } else if #[cfg(target_os = "linux")] {
            pub mod linux;
            pub use linux::LUME_ENV;
        }
    }
}

#[derive(Parser)]
#[clap(
    name = "lbs",
    version = env!("CARGO_PKG_VERSION"),
    about = "Lume's toolchain manager"
)]
#[command(subcommand_required(true), arg_required_else_help(true))]
struct Arguments {
    /// Prevent modifying the filesystem
    #[clap(long, global = true, default_value_t)]
    pub dry_run: bool,

    #[clap(subcommand)]
    pub subcommand: Subcommands,
}

#[derive(clap::Args)]
#[group(required = true, multiple = false)]
pub struct Version {
    /// Install the newest, possibly unstable, toolchain
    #[arg(long, help_heading = "Toolchain selection")]
    pub latest: bool,

    /// Install toolchain from the given Git branch
    #[arg(long, value_name = "BRANCH", help_heading = "Toolchain selection")]
    pub branch: Option<String>,

    /// Install toolchain from the given tag version
    #[arg(long, value_name = "VERSION", help_heading = "Toolchain selection")]
    pub tag: Option<String>,

    /// Install toolchain from the given Git commit hash
    #[arg(long, value_name = "COMMIT", help_heading = "Toolchain selection")]
    pub commit: Option<String>,
}

impl TryFrom<&Version> for TargetVersion {
    type Error = &'static str;

    fn try_from(version: &Version) -> std::result::Result<Self, Self::Error> {
        static GIT_SHORT_SHA: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^[0-9a-f]{7}$").unwrap());
        static GIT_LONG_SHA: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^[0-9a-f]{40}$").unwrap());

        if version.latest {
            return Ok(TargetVersion::Branch(String::from("main")));
        }

        if let Some(branch) = &version.branch {
            return Ok(TargetVersion::Branch(branch.to_owned()));
        }

        if let Some(tag) = &version.tag {
            match semver::Version::parse(tag) {
                Ok(version) => return Ok(TargetVersion::Tag(version)),
                Err(_) => return Err("invalid version format"),
            };
        }

        if let Some(commit) = &version.commit {
            if GIT_LONG_SHA.is_match(commit) {
                return Ok(TargetVersion::Commit(commit.to_owned()));
            }

            if GIT_SHORT_SHA.is_match(commit) {
                return Err("short Git commit hashes are not supported");
            }

            return Err("invalid Git commit hash");
        }

        Err("failed to parse toolchain version")
    }
}

#[derive(Parser)]
enum Subcommands {
    Install(commands::install::InstallCommand),
    Uninstall(commands::uninstall::UninstallCommand),
    Use(commands::use_::UseCommand),
    List(commands::list::ListCommand),
    Clean(commands::clean::CleanCommand),
}

pub fn lbs_cli_entry() {
    let matches = Arguments::parse();
    let dcx = DiagCtx::new();

    set_dry_run(matches.dry_run);

    let _ = dcx.with_opt(|_handle| match matches.subcommand {
        Subcommands::Install(cmd) => cmd.run(),
        Subcommands::Uninstall(cmd) => cmd.run(),
        Subcommands::Use(cmd) => cmd.run(),
        Subcommands::List(cmd) => cmd.run(),
        Subcommands::Clean(cmd) => cmd.run(),
    });

    let tainted = dcx.is_tainted();

    let mut renderer = error_snippet::GraphicalRenderer::new();
    renderer.use_colors = true;
    renderer.highlight_source = true;

    dcx.render_stderr(&mut renderer);
    dcx.clear();

    if tainted {
        std::process::exit(133);
    }
}

/// Determines if the given binary is installed on the system.
pub(crate) fn is_binary_installed(binary: &'static str) -> bool {
    let Ok(output) = std::process::Command::new(binary).arg("--version").output() else {
        return false;
    };

    output.status.success()
}

static DRY_RUN: AtomicBool = AtomicBool::new(false);

pub(crate) fn set_dry_run(value: bool) {
    DRY_RUN.store(value, std::sync::atomic::Ordering::Relaxed);
}

pub(crate) fn is_dry_run() -> bool {
    DRY_RUN.load(std::sync::atomic::Ordering::Relaxed)
}

pub(crate) fn run_dry<T: Default>(f: impl FnOnce() -> Result<T>) -> Result<T> {
    if is_dry_run() { Ok(Default::default()) } else { f() }
}

#[cfg(test)]
mod tests {
    use clap::CommandFactory;

    use super::*;

    #[test]
    fn verify_app() {
        Arguments::command().debug_assert();
    }

    #[test]
    fn cli_tests() {
        trycmd::TestCases::new().case("README.md").case("tests/cmd/*.toml");
    }
}
