pub use owo_colors::{self};

pub mod spinner;
pub mod task;

pub use spinner::Spinner;

/// Colorizes the given text with the given style.
///
/// If colors aren't supported, the text is returned as-is.
#[macro_export]
macro_rules! colorized {
    ($this:expr, $style:expr) => {
        $crate::owo_colors::OwoColorize::if_supports_color(&$this, $crate::owo_colors::Stream::Stdout, |text| {
            $style.style(text)
        })
    };
}

/// Prints an error message to the standard output.
#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => {
        #[allow(clippy::disallowed_macros, reason = "used for CLI logging")]
        {
            print!("{} ", colorized!("×", $crate::owo_colors::Style::new().red().bold()));
            println!($($arg)*);
        }
    };
}

/// Prints a warning message to the standard output.
#[macro_export]
macro_rules! warn {
    ($($arg:tt)*) => {
        #[allow(clippy::disallowed_macros, reason = "used for CLI logging")]
        {
            print!("{} ", colorized!("⚠", $crate::owo_colors::Style::new().yellow().bold()));
            println!($($arg)*);
        }
    };
}

/// Prints an information message to the standard output.
#[macro_export]
macro_rules! info {
    ($($arg:tt)*) => {
        #[allow(clippy::disallowed_macros, reason = "used for CLI logging")]
        {
            print!("{} ", colorized!("ℹ", $crate::owo_colors::Style::new().bright_blue().bold()));
            println!($($arg)*);
        }
    };
}

/// Prints a success message to the standard output.
#[macro_export]
macro_rules! success {
    ($($arg:tt)*) => {
        #[allow(clippy::disallowed_macros, reason = "used for CLI logging")]
        {
            print!("{} ", colorized!("✓", $crate::owo_colors::Style::new().green().bold()));
            println!($($arg)*);
        }
    };
}
