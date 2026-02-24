pub use imp::{install_handlers, uninstall_handlers};

#[cfg(target_os = "linux")]
mod linux;

#[cfg(target_os = "linux")]
use linux as imp;

#[cfg(target_os = "macos")]
mod macos;

#[cfg(target_os = "macos")]
use macos as imp;
