use crate::{Diagnostic, Renderer, Severity};

/// Represents an error which can occur when draining errors
/// from the [`DiagnosticHandler::drain()`] and
/// [`DiagnosticHandler::report_and_drain`].
pub enum DrainError {
    /// Defines that the error occured when attempting to write
    /// the diagnostic to the output buffer.
    Fmt(std::fmt::Error),

    /// Defines that one-or-more errors were reported during the drain,
    /// which are not propogating upwards to the calling function.
    ///
    /// The variant defines the number of errors which were reported. Note that
    /// this number does *not* include non-errors such as warnings, nor does
    /// it count any sub-diagnostics, such as labels or related errors.
    CompoundError(usize),
}

impl From<std::fmt::Error> for DrainError {
    fn from(err: std::fmt::Error) -> Self {
        Self::Fmt(err)
    }
}

impl std::error::Error for DrainError {}

impl std::fmt::Debug for DrainError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fmt(e) => e.fmt(f),
            Self::CompoundError(cnt) => f.debug_tuple("CompoundError").field(cnt).finish(),
        }
    }
}

impl std::fmt::Display for DrainError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fmt(e) => e.fmt(f),
            Self::CompoundError(cnt) => write!(f, "aborting due to {cnt} previous errors"),
        }
    }
}

/// Abstract handler type for reporting diagnostics.
///
/// Handlers are nothing more than a "store" for diagnostics, which
/// decides when to drain the diagnostics to the user.
pub trait Handler: std::any::Any {
    /// Reports the diagnostic to the handler, without emitting it immediately.
    fn report(&mut self, diagnostic: Box<dyn Diagnostic>);

    /// Drains all the diagnostics to the console and empties the local store.
    fn drain(&mut self) -> Result<(), DrainError>;

    /// Reports the diagnostic to the handler and emits it immediately, along
    /// with all other stored diagnostics within the handler.
    fn report_and_drain(&mut self, diagnostic: Box<dyn Diagnostic>) -> Result<(), DrainError> {
        self.report(diagnostic);

        self.drain()
    }
}

/// The default diagnostic handler.
///
/// The [`DiagnosticHandler`] allows to report to the user immediately or
/// deferred until drained, and aborting upon draining an error (or worse)
/// diagnostic.
///
/// # Examples
///
/// To use deferred reporting:
///
/// ```
/// use lume_errors::{SimpleDiagnostic, GraphicalRenderer, Handler, DiagnosticHandler};
///
/// let diagnostic = SimpleDiagnostic::new("An error occurred");
///
/// let renderer = GraphicalRenderer::new();
/// let mut handler = DiagnosticHandler::with_renderer(Box::new(renderer));
///
/// handler.report(Box::new(diagnostic));
/// ```
///
/// If not, you can drain the diagnostics immediately after reporting it:
///
/// ```
/// use lume_errors::{SimpleDiagnostic, GraphicalRenderer, Handler, DiagnosticHandler};
///
/// let diagnostic = SimpleDiagnostic::new("An error occurred");
///
/// let renderer = GraphicalRenderer::new();
/// let mut handler = DiagnosticHandler::with_renderer(Box::new(renderer));
///
/// handler.report_and_drain(Box::new(diagnostic));
/// ```
///
/// To abort upon draining an error diagnostic, use the
/// [`DiagnosticHandler::exit_on_error()`] method:
///
/// ```
/// use lume_errors::{DiagnosticHandler, GraphicalRenderer};
///
/// let renderer = GraphicalRenderer::new();
/// let mut handler = DiagnosticHandler::with_renderer(Box::new(renderer));
/// handler.exit_on_error();
///
/// // ...
/// ```
pub struct DiagnosticHandler {
    /// Defines whether to exit upon emitting an error.
    exit_on_error: bool,

    /// Stores all the diagnostics which have been reported.
    emitted_diagnostics: Vec<Box<dyn Diagnostic>>,

    /// Defines the renderer to use when rendering the diagnostics.
    renderer: Box<dyn Renderer + Send + Sync>,
}

impl DiagnosticHandler {
    /// Creates a new empty handler.
    pub fn with_renderer(renderer: Box<dyn Renderer + Send + Sync>) -> Self {
        DiagnosticHandler {
            exit_on_error: false,
            emitted_diagnostics: Vec::new(),
            renderer,
        }
    }

    /// Enables the handler to exit upon emitting an error.
    pub fn exit_on_error(&mut self) {
        self.exit_on_error = true;
    }

    /// Gets an [`Iterator`] over all the emitted diagnostics to the handler,
    /// which have yet to be drained.
    pub fn emitted(&self) -> impl Iterator<Item = &Box<dyn Diagnostic>> {
        self.emitted_diagnostics.iter()
    }

    /// Gets the amount of diagnostics within the handler, which have
    /// yet to be drained.
    pub fn count(&self) -> usize {
        self.emitted_diagnostics.len()
    }
}

impl Handler for DiagnosticHandler {
    fn report(&mut self, diagnostic: Box<dyn Diagnostic>) {
        self.emitted_diagnostics.push(diagnostic);
    }

    fn drain(&mut self) -> Result<(), DrainError> {
        let mut encountered_errors = 0usize;

        for diagnostic in self.emitted_diagnostics.drain(..) {
            self.renderer.render_stderr(diagnostic.as_ref())?;

            // If the diagnostic is an error, mark it down.
            if diagnostic.severity() >= Severity::Error {
                encountered_errors += 1;
            }
        }

        // If we've encountered any errors, and we're enabled to propogate errors
        // upwards, return a specific error to compound all encountered errors.
        if encountered_errors > 0 && self.exit_on_error {
            return Err(DrainError::CompoundError(encountered_errors));
        }

        Ok(())
    }
}

/// A buffered version of [`DiagnosticHandler`].
///
/// The [`BufferedDiagnosticHandler`] will save rendered diagnostics to an
/// internal buffer, allowing them to be read back as [`String`]-values. This is
/// mostly used for UI testing.
pub struct BufferedDiagnosticHandler {
    /// Stores all the rendered diagnostics which have been drained.
    buffer: String,

    /// Stores all the diagnostics which have been reported.
    emitted_diagnostics: Vec<Box<dyn Diagnostic>>,

    /// Defines the renderer to use when rendering the diagnostics.
    renderer: Box<dyn Renderer + Send + Sync>,
}

impl BufferedDiagnosticHandler {
    /// Creates a new empty handler.
    pub fn with_renderer(capacity: usize, renderer: Box<dyn Renderer + Send + Sync>) -> Self {
        Self {
            buffer: String::with_capacity(capacity),
            emitted_diagnostics: Vec::new(),
            renderer,
        }
    }

    /// Gets the [`String`] buffer which contains the rendered diagnostics.
    pub fn buffer(&self) -> &str {
        &self.buffer
    }

    /// Gets an [`Iterator`] over all the emitted diagnostics to the handler,
    /// which have yet to be drained.
    pub fn emitted(&self) -> impl Iterator<Item = &Box<dyn Diagnostic>> {
        self.emitted_diagnostics.iter()
    }

    /// Gets the amount of diagnostics within the handler, which have
    /// yet to be drained.
    pub fn count(&self) -> usize {
        self.emitted_diagnostics.len()
    }
}

impl Handler for BufferedDiagnosticHandler {
    fn report(&mut self, diagnostic: Box<dyn Diagnostic>) {
        self.emitted_diagnostics.push(diagnostic);
    }

    fn drain(&mut self) -> Result<(), DrainError> {
        for diagnostic in self.emitted_diagnostics.drain(..) {
            let rendered = self.renderer.render(diagnostic.as_ref())?;

            self.buffer.push_str(&rendered);
        }

        Ok(())
    }
}
