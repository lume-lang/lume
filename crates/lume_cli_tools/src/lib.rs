pub use owo_colors::{self};

pub mod spinner;
pub mod task;

pub use spinner::Spinner;
pub use task::*;

pub trait Stylable: Sized {
    /// Styles the given value from a dotted style string.
    ///
    /// Effectively the string is split at each dot and then the
    /// terms in between are applied.  For instance `red.on_blue` will
    /// create a string that is red on blue background. `9.on_12` is
    /// the same, but using 256 color numbers. Unknown terms are
    /// ignored.
    ///
    /// # Examples
    ///
    /// ```
    /// use lume_cli_tools::Stylable;
    ///
    /// println!("Hello {}!", "world".stylize("bright.yellow.bold"));
    /// ```
    ///
    /// For more information, see [`console::Style::from_dotted_str`].
    fn stylize(self, fmt: &str) -> console::StyledObject<Self> {
        console::Style::from_dotted_str(fmt).for_stdout().apply_to(self)
    }
}

impl<D> Stylable for D {}

/// Prints an error message to the standard output.
#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => {
        #[allow(clippy::disallowed_macros, reason = "used for CLI logging")]
        {
            print!("{} ", $crate::Stylable::stylize("×", "red.bold"));
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
            print!("{} ", $crate::Stylable::stylize("⚠", "yellow.bold"));
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
            print!("{} ", $crate::Stylable::stylize("ℹ", "bright.blue.bold"));
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
            print!("{} ", $crate::Stylable::stylize("✓", "green.bold"));
            println!($($arg)*);
        }
    };
}
