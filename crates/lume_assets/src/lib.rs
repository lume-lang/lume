use std::path::PathBuf;

use lume_errors::{MapDiagnostic, Result, SimpleDiagnostic};

/// Attempts to determine the absolute path to the compiler asset with the
/// given file name, depending on the current execution environment.
///
/// If executing within the source tree using the `cargo` command, returns
/// the path to the local asset within the source tree. Otherwise, attempts
/// to find the asset in the system cache.
///
/// # Errors
///
/// Returns [`Err`] if the asset could not be found within the current
/// environment.
pub fn asset_file_path(asset_name: &str) -> Result<PathBuf> {
    let asset_path = target_build_dir()?.join(asset_name);

    if !asset_path.exists() {
        return if is_dev() {
            Err(
                SimpleDiagnostic::new(format!("could not find asset in source tree: {asset_name}"))
                    .with_help("was the compiler built correctly?")
                    .with_help("use `cargo build --workspace --all-targets` to build all assets")
                    .into(),
            )
        } else {
            Err(
                SimpleDiagnostic::new(format!("could not find asset in system: {asset_name}"))
                    .with_help("was the compiler installed correctly?")
                    .into(),
            )
        };
    }

    Ok(asset_path)
}

/// Attempts to determine whether the current process has been invoked
/// during development, such as being invoked using `cargo`.
pub fn is_dev() -> bool {
    // If `CARGO` is set, we know we are being run as part of a `cargo run` command
    // which only happens inside of the source tree. Otherwise, we're likely
    // outside the tree and we need to look for the runner in the system
    // directories.
    std::env::var_os("CARGO").is_some()
}

/// Attempts to determine the absolute path to the root of the source tree,
/// where the Lume compiler resides.
///
/// # Errors
///
/// Returns [`Err`] if current process is invoked outside of the source tree.
pub fn compiler_root_dir() -> Result<PathBuf> {
    let relative_dir = if std::env!("CARGO_PKG_NAME") == "lume" {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    } else {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../")
    };

    relative_dir
        .canonicalize()
        .map_cause("could not determine root project directory")
}

/// Attempts to determine the absolute path to the build directory,
/// corresponding to the current profile being compiled.
///
/// # Errors
///
/// Returns [`Err`] if current process is invoked outside of the source tree or
/// of the profile directory does not exist.
pub fn target_build_dir() -> Result<PathBuf> {
    #[cfg(not(debug_assertions))]
    let profile_name = "release";

    #[cfg(debug_assertions)]
    let profile_name = "debug";

    let build_dir = compiler_root_dir()?.join("target").join(profile_name);
    if !build_dir.exists() {
        return Err(
            SimpleDiagnostic::new("could not find profile build directory in project")
                .with_help(format!("attempted to look for `target/{profile_name}`"))
                .into(),
        );
    }

    Ok(build_dir)
}

/// Attempts to determine the absolute path to the toolchain path, depending on
/// the current execution environment.
///
/// When a directory is returned from this function, it is guaranteed to exist.
///
/// # Errors
///
/// Returns [`Err`] if the path could not be found within the current
/// environment.
pub fn toolchain_base_path() -> Result<PathBuf> {
    let Some(lume_dir) = determine_data_dir() else {
        return Err(SimpleDiagnostic::new("could not determine toolchain directory").into());
    };

    let toolchains_path = lume_dir.join("toolchains");

    std::fs::create_dir_all(&toolchains_path).map_diagnostic()?;

    Ok(toolchains_path)
}

/// Attempts to determine the absolute path to the link which points to the
/// currently selected toolchain.
///
/// If no toolchain is selected, returns [`None`].
///
/// # Errors
///
/// Returns [`Err`] if the path could not be determined within the current
/// environment.
pub fn current_toolchain_linkpath() -> Result<PathBuf> {
    let toolchain_path = toolchain_base_path()?;

    Ok(toolchain_path.join("current"))
}

/// Attempts to determine the absolute path to the currently selected toolchain.
///
/// If no toolchain is selected, returns [`None`].
///
/// # Errors
///
/// Returns [`Err`] if the path could not be determined within the current
/// environment.
pub fn toolchain_current_path() -> Result<Option<PathBuf>> {
    let link_path = current_toolchain_linkpath()?;
    if !link_path.exists() {
        return Ok(None);
    }

    std::fs::read_link(link_path).map_diagnostic().map(Some)
}

/// Determines the directory where the system runtime library would be stored.
///
/// Depending on the directory returned, the directory might not exist.
pub fn determine_data_dir() -> Option<PathBuf> {
    // |Platform | Value                                    | Example                                  |
    // | ------- | ---------------------------------------- | ---------------------------------------- |
    // | Linux   | `$XDG_DATA_HOME` or `$HOME`/.local/share | /home/alice/.local/share                 |
    // | macOS   | `$HOME`/Library/Application Support      | /Users/Alice/Library/Application Support |
    // | Windows | `{FOLDERID_RoamingAppData}`              | C:\Users\Alice\AppData\Roaming           |
    if let Some(dir) = dirs::data_dir()
        && dir.exists()
    {
        return Some(dir.join("lume"));
    }

    // |Platform | Value                                    | Example                                  |
    // | ------- | ---------------------------------------- | ---------------------------------------- |
    // | Linux   | `$XDG_DATA_HOME` or `$HOME`/.local/share | /home/alice/.local/share                 |
    // | macOS   | `$HOME`/Library/Application Support      | /Users/Alice/Library/Application Support |
    // | Windows | `{FOLDERID_LocalAppData}`                | C:\Users\Alice\AppData\Local             |
    if let Some(dir) = dirs::data_local_dir()
        && dir.exists()
    {
        return Some(dir.join("lume"));
    }

    // |Platform | Value                               | Example                      |
    // | ------- | ----------------------------------- | ---------------------------- |
    // | Linux   | `$XDG_CACHE_HOME` or `$HOME`/.cache | /home/alice/.cache           |
    // | macOS   | `$HOME`/Library/Caches              | /Users/Alice/Library/Caches  |
    // | Windows | `{FOLDERID_LocalAppData}`           | C:\Users\Alice\AppData\Local |
    if let Some(dir) = dirs::cache_dir()
        && dir.exists()
    {
        return Some(dir.join("lume"));
    }

    // |Platform | Value                | Example        |
    // | ------- | -------------------- | -------------- |
    // | Linux   | `$HOME`              | /home/alice    |
    // | macOS   | `$HOME`              | /Users/Alice   |
    // | Windows | `{FOLDERID_Profile}` | C:\Users\Alice |
    if let Some(home_dir) = dirs::home_dir() {
        return Some(home_dir.join(".lume"));
    }

    None
}

/// Attempts to determine the absolute path to the local Lume directory within
/// the home directory.
///
/// Depending on the directory returned, the directory might not exist.
pub fn determine_lume_home() -> Option<PathBuf> {
    // |Platform | Value                | Example        |
    // | ------- | -------------------- | -------------- |
    // | Linux   | `$HOME`              | /home/alice    |
    // | macOS   | `$HOME`              | /Users/Alice   |
    // | Windows | `{FOLDERID_Profile}` | C:\Users\Alice |
    if let Some(home_dir) = dirs::home_dir() {
        return Some(home_dir.join(".lume"));
    }

    // |Platform | Value                                    | Example                                  |
    // | ------- | ---------------------------------------- | ---------------------------------------- |
    // | Linux   | `$XDG_DATA_HOME` or `$HOME`/.local/share | /home/alice/.local/share                 |
    // | macOS   | `$HOME`/Library/Application Support      | /Users/Alice/Library/Application Support |
    // | Windows | `{FOLDERID_LocalAppData}`                | C:\Users\Alice\AppData\Local             |
    if let Some(dir) = dirs::data_local_dir()
        && dir.exists()
    {
        return Some(dir.join("lume"));
    }

    None
}
