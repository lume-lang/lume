use crate::Diagnostic;

pub mod graphical;

pub use graphical::*;

/// Represents a wrapper around a standard formatter.
pub struct Formatter<'a> {
    inner: &'a mut dyn std::fmt::Write,
}

impl std::fmt::Write for Formatter<'_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.inner.write_str(s)
    }
}

/// Defines a trait for rendering diagnostics to a formatter.
pub trait Renderer {
    /// Renders the diagnostic to a string buffer.
    fn render(&mut self, diagnostic: &dyn Diagnostic) -> Result<String, std::fmt::Error> {
        let mut buffer = String::new();
        let mut formatter = Formatter { inner: &mut buffer };

        self.render_fmt(&mut formatter, diagnostic)?;

        Ok(buffer)
    }

    /// Renders the diagnostic to the standard output buffer.
    fn render_stderr(&mut self, diagnostic: &dyn Diagnostic) -> std::fmt::Result {
        eprint!("{}", self.render(diagnostic)?);

        Ok(())
    }

    /// Renders the diagnostic to the given formatter.
    fn render_fmt(&mut self, f: &mut Formatter, diagnostic: &dyn Diagnostic) -> std::fmt::Result;
}
