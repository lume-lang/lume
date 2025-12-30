//! This module provides a simple spinner for the command line interface.
//!
//! ### Usage
//!
//! ```
//! use lume_cli_tools::*;
//! use std::thread::sleep;
//! use std::time::Duration;
//!
//! let mut spinner = Spinner::new("Loading...", None);
//! sleep(Duration::from_millis(800));
//! spinner.stop_with_success("Success!");
//! ```

use std::fmt::Write as _;
use std::io::Write as _;
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, LazyLock, RwLock};
use std::thread::JoinHandle;
use std::time::Duration;

use owo_colors::Style;

use crate::colorized;

/// A Struct that contains the data for a spinner.
/// Frames is a Vec of &str, each &str is a frame of the spinner.
/// Interval is the number of milliseconds to wait before moving to the next
/// frame.
#[derive(Debug, Clone)]
struct Frames {
    pub frames: Vec<&'static str>,
    pub interval: u16,
}

static SPINNER_FRAMES: LazyLock<Frames> = LazyLock::new(|| Frames {
    frames: vec!["✶", "✸", "✹", "✺", "✹", "✷"],
    interval: 70,
});

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub enum Stream {
    #[default]
    Stdout,
    Stderr,
}

impl Stream {
    pub fn flush(&self) -> std::io::Result<()> {
        match self {
            Stream::Stdout => std::io::stdout().flush(),
            Stream::Stderr => std::io::stderr().flush(),
        }
    }
}

impl std::fmt::Write for Stream {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        match self {
            Stream::Stdout => std::io::stdout().write_all(s.as_bytes()),
            Stream::Stderr => std::io::stderr().write_all(s.as_bytes()),
        }
        .map_err(|_| std::fmt::Error)
    }
}

pub struct Spinner {
    spinning: Arc<AtomicBool>,
    handle: Option<JoinHandle<()>>,

    stream: Stream,
    style: Option<Style>,
}

impl Spinner {
    /// Creates a new spinner with the given initial message. By default, output
    /// is written to `stdout`.
    ///
    /// Optionally, you can provide a custom spinner style to apply to the
    /// spinner icon itself.
    pub fn new<M: Into<String>>(message: M, style: Option<Style>) -> Self {
        Self::new_with_stream(message.into(), style, Stream::default())
    }

    /// Creates a new spinner with the given initial message and output stream.
    ///
    /// Optionally, you can provide a custom spinner style to apply to the
    /// spinner icon itself.
    pub fn new_with_stream<M: Into<String>>(message: M, style: Option<Style>, stream: Stream) -> Self {
        let spinning = Arc::new(AtomicBool::new(true));
        let message = Arc::new(RwLock::new(message.into()));

        let handle = std::thread::spawn({
            let spinning = spinning.clone();
            let style = style.unwrap_or(Style::new().blue().bold());

            move || spinner_thread(spinning, message, style, stream)
        });

        Self {
            spinning,
            handle: Some(handle),
            stream,
            style,
        }
    }

    /// Updates the spinner message.
    pub fn update_message<M: Into<String>>(&mut self, message: M) {
        self.stop();

        let _replaced = std::mem::replace(self, Self::new_with_stream(message, self.style, self.stream));
    }

    /// Stops the spinner.
    ///
    /// Note: this method also clears the terminal after stopping the
    /// spinner.
    pub fn stop(&mut self) {
        self.spinning.store(false, std::sync::atomic::Ordering::Relaxed);

        if let Some(handle) = self.handle.take() {
            handle.join().expect("error: failed to join spinner thread");
        }
    }

    /// Stops the spinner and prints an error message.
    ///
    /// Note: this method also clears the terminal after stopping the
    /// spinner.
    pub fn stop_with_error<M: Into<String>>(&mut self, message: M) {
        self.stop();

        crate::error!("{}", message.into());
    }

    /// Stops the spinner and prints a warning message.
    ///
    /// Note: this method also clears the terminal after stopping the
    /// spinner.
    pub fn stop_with_warning<M: Into<String>>(&mut self, message: M) {
        self.stop();

        crate::warn!("{}", message.into());
    }

    /// Stops the spinner and prints a success message.
    ///
    /// Note: this method also clears the terminal after stopping the
    /// spinner.
    pub fn stop_with_success<M: Into<String>>(&mut self, message: M) {
        self.stop();

        crate::success!("{}", message.into());
    }
}

fn spinner_thread(spinning: Arc<AtomicBool>, message: Arc<RwLock<String>>, style: Style, mut stream: Stream) {
    let spinner_frames = &*SPINNER_FRAMES;

    let frames = SPINNER_FRAMES
        .frames
        .iter()
        .cycle()
        .take_while(|_| spinning.load(std::sync::atomic::Ordering::Relaxed));

    let mut last_length = 0;

    for frame in frames {
        let message = message.read().unwrap();
        let formatted_str = format!("{} {message}", colorized!(frame, style));

        // Get us back to the start of the line.
        clear_line(last_length, &mut stream);
        last_length = formatted_str.len();

        write!(stream, "{formatted_str}").expect("error: failed to write to stream");

        stream.flush().expect("error: failed to flush stream");
        std::thread::sleep(Duration::from_millis(u64::from(spinner_frames.interval)));
    }

    clear_line(last_length, &mut stream);
}

/// Clears the current line in a terminal using a carriage return.
///
/// After the line is cleared, it moves the cursor back to the start of the
/// line.
fn clear_line(len: usize, stream: &mut Stream) {
    let delete = move |stream: &mut Stream| {
        write!(stream, "\r")?;

        for _ in 0..len {
            write!(stream, " ")?;
        }

        // Move back to the start of the line.
        write!(stream, "\r")
    };

    delete(stream).expect("error: failed to reset line in stream");
}
