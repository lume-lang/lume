use crate::{LumeDiagnostic, Severity};

/// Abstract handler type for reporting diagnostics.
///
/// Handlers are nothing more than a "store" for diagnostics, which
/// decides when to drain the diagnostics to the user.
pub trait Handler<'a> {
    /// Reports the diagnostic to the handler, without emitting it immediately.
    fn report(&mut self, diagnostic: impl Into<LumeDiagnostic<'a>>);

    /// Drains all the diagnostics to the console and empties the local store.
    fn drain(&mut self);

    /// Reports the diagnostic to the handler and emits it immediately, along
    /// with all other stored diagnostics within the handler.
    fn report_and_drain(&mut self, diagnostic: impl Into<LumeDiagnostic<'a>>) {
        self.report(diagnostic);

        self.drain();
    }
}

/// The default diagnostic handler.
///
/// The [`DiagnosticHandler`] allows to report to the user immediately or deferred until drained,
/// and aborting upon draining an error (or worse) diagnostic.
///
/// # Examples
///
/// To use deferred reporting:
///
/// ```
/// use lume_diag::LumeDiagnostic;
/// use lume_diag::handler::Handler;
/// use lume_diag::handler::DiagnosticHandler;
///
/// let diagnostic = LumeDiagnostic::new("An error occurred");
///
/// let mut handler = DiagnosticHandler::new();
/// handler.report(diagnostic);
/// ```
///
/// If not, you can drain the diagnostics immediately after reporting it:
///
/// ```
/// use lume_diag::LumeDiagnostic;
/// use lume_diag::handler::Handler;
/// use lume_diag::handler::DiagnosticHandler;
///
/// let diagnostic = LumeDiagnostic::new("An error occurred");
///
/// let mut handler = DiagnosticHandler::new();
/// handler.report_and_drain(diagnostic);
/// ```
///
/// To abort upon draining an error diagnostic, use the [`exit_on_error`] method:
///
/// ```
/// use lume_diag::handler::DiagnosticHandler;
///
/// let mut handler = DiagnosticHandler::new();
/// handler.exit_on_error();
///
/// // ...
/// ```
#[derive(Default, Debug)]
pub struct DiagnosticHandler<'a> {
    /// Defines whether to exit upon emitting an error.
    exit_on_error: bool,

    /// Stores all the diagnostics which have been reported.
    emitted_diagnostics: Vec<LumeDiagnostic<'a>>,
}

impl<'a> DiagnosticHandler<'a> {
    /// Creates a new empty handler.
    pub fn new() -> Self {
        DiagnosticHandler::default()
    }

    /// Enables the handler to exit upon emitting an error.
    pub fn exit_on_error(&mut self) {
        self.exit_on_error = true
    }
}

impl<'a> Handler<'a> for DiagnosticHandler<'a> {
    fn report(&mut self, diagnostic: impl Into<LumeDiagnostic<'a>>) {
        self.emitted_diagnostics.push(diagnostic.into());
    }

    fn drain(&mut self) {
        let mut encountered_errors = 0usize;

        while let Some(diagnostic) = self.emitted_diagnostics.pop() {
            // If the diagnostic is an error, mark it down.
            if diagnostic.severity == Severity::Error {
                encountered_errors += 1;
            }

            diagnostic.render();
        }

        // If we've encountered any errors,
        if encountered_errors > 0 && self.exit_on_error {
            let message = format!("aborting due to {} previous errors", encountered_errors);

            LumeDiagnostic::new(message).render();

            std::process::exit(1);
        }
    }
}
