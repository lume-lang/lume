use std::io::{Read, Write as _};
use std::path::PathBuf;

use lume_errors::{MapDiagnostic, Result, SimpleDiagnostic};

use crate::{colorized, fs};

pub const LUME_ENV: &str = r#"#!/bin/sh
## Shell environment setup for Lume

# Prevent adding duplicate entries to PATH, LIBRARY_PATH

case ":${PATH}:" in
    *:"$HOME/.lume/bin":*)
        ;;
    *)
        export PATH="$HOME/.lume/bin:$PATH"
        ;;
esac

case ":${LIBRARY_PATH}:" in
    *:"$HOME/.lume/lib":*)
        ;;
    *)
        export LIBRARY_PATH="$HOME/.lume/lib:$LIBRARY_PATH"
        ;;
esac
"#;

#[cfg(target_env = "msvc")]
static LINKS: &[&str] = &["bin/lume.exe", "lib/liblume_runtime.lib"];

#[cfg(not(target_env = "msvc"))]
static LINKS: &[&str] = &["bin/lume", "lib/liblume_runtime.a"];

macro_rules! print_post_install_env {
    () => {
        #[allow(clippy::disallowed_macros)]
        if !$crate::is_quiet() {
            println!(
                "
             🎉 {header}

Before you can use it, you may need to restart your shell or source your
shell configuration file, so your `PATH` environment variable is updated.

Your shell configuration file has been updated, so it can locate the new
Lume toolchain. The shell configuration file is located at: {shellrc}

Afterwards, you can verify that Lume is installed correctly by running:

    {version_cmd}

",
                header = colorized!(
                    "Lume has been installed successfully!",
                    owo_colors::Style::new().bright_yellow().bold()
                ),
                version_cmd = colorized!("lume --version", owo_colors::Style::new().dimmed()),
                shellrc = colorized!(shellrc(), owo_colors::Style::new().dimmed())
            )
        }
    };
}

macro_rules! print_post_install_noenv {
    (path => $path:expr) => {
        #[allow(clippy::disallowed_macros)]
        if !$crate::is_quiet() {
            println!(
                "
             🎉 {header}

Before you can use it, you need to add the Lume bin directory to your
`PATH` environment variable. You can do it for the current session using:

    {temporary_cmd}

To do this permanently, you can add the following line to your shell
configuration file (e.g., ~/.bashrc, ~/.zshrc):

    {persistent_cmd}

",
                header = colorized!(
                    "Lume has been installed successfully!",
                    owo_colors::Style::new().bright_yellow().bold()
                ),
                temporary_cmd = colorized!(
                    format!(r#"export PATH="{path}:$PATH""#, path = $path),
                    owo_colors::Style::new().dimmed()
                ),
                persistent_cmd = colorized!(
                    format!(
                        r#"echo 'export PATH="{path}:$PATH"' >> {shellrc}"#,
                        path = $path,
                        shellrc = shellrc()
                    ),
                    owo_colors::Style::new().dimmed()
                )
            )
        }
    };
}

/// Determines whether the home directory contains the links added by
/// [`add_links`].
pub fn has_links() -> Result<bool> {
    let Some(lume_home) = lume_assets::determine_lume_home() else {
        return Err(SimpleDiagnostic::new("could not determine Lume home directory").into());
    };

    if !lume_home.exists() {
        return Ok(false);
    }

    Ok(LINKS.iter().all(|name| lume_home.join(name).exists()))
}

/// Links the binaries inside the `.lume` directory, which exists inside the
/// home directory.
pub fn add_links(skip_shellrc: bool) -> Result<()> {
    let toolchain_link_path = lume_assets::current_toolchain_linkpath()?;

    let Some(lume_home) = lume_assets::determine_lume_home() else {
        return Err(SimpleDiagnostic::new("could not determine Lume home directory").into());
    };

    fs::create_dir(&lume_home)?;

    for relative_link_path in LINKS {
        let link_path = lume_home.join(relative_link_path);
        let target_path = toolchain_link_path.join(relative_link_path);

        if let Some(parent_dir) = link_path.parent() {
            fs::create_dir(parent_dir)?;
        }

        if link_path.exists() {
            std::fs::remove_file(&link_path)?;
        }

        fs::link(target_path, link_path)?;
    }

    // Write the `.lume/env` file in the home directory.
    write_lume_env()?;

    // Update the shell environment variables, if needed.
    if !skip_shellrc && should_update_shellrc() {
        update_shellrc()?;

        print_post_install_env!();
    } else {
        print_post_install_noenv!(path => lume_home.join("bin").display());
    }

    Ok(())
}

/// Writes the Lume shell setup script to the `.lume/env` file.
fn write_lume_env() -> Result<()> {
    let Some(lume_home) = lume_assets::determine_lume_home() else {
        return Err(SimpleDiagnostic::new("could not determine Lume home directory").into());
    };

    let lume_env_path = lume_home.join("env");
    std::fs::write(lume_env_path, LUME_ENV).map_diagnostic()?;

    Ok(())
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
enum Shell {
    #[default]
    Bash,
    Zsh,
    Fish,
}

/// Attempts to get the currently running shell.
fn current_shell() -> Option<Shell> {
    let shell = std::env::var("SHELL").ok();

    if shell.as_ref().is_some_and(|s| s.ends_with("bash")) {
        return Some(Shell::Bash);
    }

    if shell.as_ref().is_some_and(|s| s.ends_with("zsh")) {
        return Some(Shell::Zsh);
    }

    if shell.as_ref().is_some_and(|s| s.ends_with("fish")) {
        return Some(Shell::Fish);
    }

    None
}

/// Gets the path to the shell configuration file.
fn shellrc() -> &'static str {
    match current_shell().unwrap_or_default() {
        Shell::Bash => "~/.bashrc",
        Shell::Zsh => "~/.zshrc",
        Shell::Fish => "~/.config/fish/config.fish",
    }
}

/// Determines whether the `PATH` environment variable should be updated in the
/// current shell configuration.
fn should_update_shellrc() -> bool {
    std::env::var("LBS_SKIP_PATH").map_or(true, |value| {
        matches!(value.to_lowercase().as_str(), "false" | "0" | "no")
    })
}

/// Updates the shell configuration file to include the Lume binaries.
fn update_shellrc() -> Result<()> {
    let lume_home = lume_assets::determine_lume_home().expect("determine Lume home");
    let lume_env_path = lume_home.join("env");

    let content = format!(
        r#"
# Shell setup for Lume
source "{}"
"#,
        lume_env_path.display()
    );

    let shellrc_relative_path = shellrc();

    let shell_config = if shellrc_relative_path.starts_with("~/") {
        let home_dir = std::env::home_dir().expect("get home directory");

        home_dir.join(shellrc_relative_path.trim_start_matches("~/"))
    } else {
        PathBuf::from(shellrc_relative_path)
    };

    let mut shell_file = std::fs::OpenOptions::new()
        .read(true) // to read whether it's already added
        .append(true)
        .open(&shell_config)
        .map_cause(format!(
            "failed to open shell configuration file: {}",
            shell_config.display()
        ))?;

    // Check if the content is already added
    let mut buffer = String::new();
    shell_file
        .read_to_string(&mut buffer)
        .map_cause("failed to read shell configuration file")?;

    if !buffer.contains("# Shell setup for Lume") {
        shell_file.write_all(content.as_bytes()).map_diagnostic()?;
    }

    Ok(())
}

/// Links the binaries inside the `.lume` directory, if they don't already
/// exist.
pub fn add_links_if_needed(skip_shellrc: bool) -> Result<()> {
    if has_links().is_ok_and(|r| r) {
        return Ok(());
    }

    add_links(skip_shellrc)
}
