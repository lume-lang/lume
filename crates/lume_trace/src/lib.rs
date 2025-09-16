#![doc = include_str!("../README.md")]

pub extern crate tracing;

pub use tracing::Level;

/// Even when trace logging is disabled, the tracing macros has a significant performance
/// cost so we disable it by default.
#[macro_export]
macro_rules! event {
    ($($tt:tt)*) => {
        if cfg!(feature = "tracing") {
            $crate::tracing::event!($($tt)*);
        }
    };
}

/// Even when trace logging is disabled, the tracing macros has a significant performance
/// cost so we disable it by default.
#[macro_export]
macro_rules! trace {
    ($($tt:tt)*) => {
        if cfg!(feature = "tracing") {
            $crate::tracing::trace!($($tt)*);
        }
    };
}

/// Even when trace logging is disabled, the tracing macros has a significant performance
/// cost so we disable it by default.
#[macro_export]
macro_rules! debug {
    ($($tt:tt)*) => {
        if cfg!(feature = "tracing") {
            $crate::tracing::debug!($($tt)*);
        }
    };
}

/// Even when trace logging is disabled, the tracing macros has a significant performance
/// cost so we disable it by default.
#[macro_export]
macro_rules! info {
    ($($tt:tt)*) => {
        if cfg!(feature = "tracing") {
            $crate::tracing::info!($($tt)*);
        }
    };
}

/// Even when trace logging is disabled, the tracing macros has a significant performance
/// cost so we disable it by default.
#[macro_export]
macro_rules! warn {
    ($($tt:tt)*) => {
        if cfg!(feature = "tracing") {
            $crate::tracing::warn!($($tt)*);
        }
    };
}

/// Even when trace logging is disabled, the tracing macros has a significant performance
/// cost so we disable it by default.
#[macro_export]
macro_rules! error {
    ($($tt:tt)*) => {
        if cfg!(feature = "tracing") {
            $crate::tracing::error!($($tt)*);
        }
    };
}

/// Determines whether the given tracing level is enabled.
#[macro_export]
macro_rules! enabled {
    ($level:path) => {
        cfg!(feature = "tracing") && $crate::tracing::enabled!($level)
    };
}
