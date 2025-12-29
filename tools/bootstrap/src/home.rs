use lume_errors::{MapDiagnostic, Result, SimpleDiagnostic};

use crate::fs;

pub const LUME_ENV: &str = r#"#!/bin/sh
## Shell setup for Lume

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
pub fn add_links() -> Result<()> {
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

/// Links the binaries inside the `.lume` directory, if they don't already
/// exist.
pub fn add_links_if_needed() -> Result<()> {
    if has_links().is_ok_and(|r| r) {
        return Ok(());
    }

    add_links()
}
