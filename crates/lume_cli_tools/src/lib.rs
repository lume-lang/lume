pub use owo_colors::{self};

pub mod progress;
pub mod spinner;
pub mod task;

pub use console::Style;
pub use spinner::Spinner;
pub use task::*;

pub const ACCENT_PRIMARY: &str = "#936FF7";
pub const ACCENT_SECONDARY: &str = "#AF88F5";
pub const ACCENT_ACCENT: &str = "#F074DA";

/// Creates a new [`console::Style`] from a dotted style string.
///
/// Effectively the string is split at each dot and then the
/// terms in between are applied.  For instance `red.on_blue` will
/// create a string that is red on blue background. `9.on_12` is
/// the same, but using 256 color numbers. Unknown terms are
/// ignored.
///
/// For more information, see [`console::Style::from_dotted_str`].
pub fn from_dotted_str(s: &str) -> console::Style {
    fn rgb_from_hex(hex: &str) -> Result<(u8, u8, u8), std::num::ParseIntError> {
        let r = u8::from_str_radix(&hex[1..3], 16)?;
        let g = u8::from_str_radix(&hex[3..5], 16)?;
        let b = u8::from_str_radix(&hex[5..7], 16)?;

        Ok((r, g, b))
    }

    let mut rv = console::Style::new();
    for part in s.split('.') {
        rv = match part {
            // Lume primary colors
            "primary" => {
                if let Ok((r, g, b)) = rgb_from_hex(ACCENT_PRIMARY) {
                    rv.true_color(r, g, b)
                } else {
                    continue;
                }
            }
            "secondary" => {
                if let Ok((r, g, b)) = rgb_from_hex(ACCENT_SECONDARY) {
                    rv.true_color(r, g, b)
                } else {
                    continue;
                }
            }
            "accent" => {
                if let Ok((r, g, b)) = rgb_from_hex(ACCENT_ACCENT) {
                    rv.true_color(r, g, b)
                } else {
                    continue;
                }
            }

            // Primary colors
            "black" => rv.black(),
            "red" => rv.red(),
            "green" => rv.green(),
            "yellow" => rv.yellow(),
            "blue" => rv.blue(),
            "magenta" => rv.magenta(),
            "cyan" => rv.cyan(),
            "white" => rv.white(),

            // Flags
            "bright" => rv.bright(),
            "bold" => rv.bold(),
            "dim" => rv.dim(),
            "underlined" => rv.underlined(),
            "blink" => rv.blink(),
            "blink_fast" => rv.blink_fast(),
            "reverse" => rv.reverse(),
            "hidden" => rv.hidden(),
            "strikethrough" => rv.strikethrough(),

            // Misc.
            true_color if true_color.starts_with('#') && true_color.len() == 7 => {
                if let (Ok(r), Ok(g), Ok(b)) = (
                    u8::from_str_radix(&true_color[1..3], 16),
                    u8::from_str_radix(&true_color[3..5], 16),
                    u8::from_str_radix(&true_color[5..7], 16),
                ) {
                    rv.true_color(r, g, b)
                } else {
                    continue;
                }
            }
            c => {
                if let Ok(n) = c.parse::<u8>() {
                    rv.color256(n)
                } else {
                    continue;
                }
            }
        }
    }

    rv
}

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
        from_dotted_str(fmt).for_stdout().apply_to(self)
    }
}

impl<D> Stylable for D {}

/// Creates a new progress bar with the Lume style.
pub fn progress_bar() -> progress::ProgressBar {
    progress_bar_with_gutter(progress::DEFAULT_GUTTER_SIZE)
}

/// Creates a new progress bar with the Lume style.
pub fn progress_bar_with_gutter(gutter: usize) -> progress::ProgressBar {
    progress::ProgressBar::no_length().with_gutter(gutter)
}

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
