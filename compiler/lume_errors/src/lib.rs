#![allow(clippy::arc_with_non_send_sync)]

use error_snippet::{DiagnosticHandler, Handler, Renderer};
use std::sync::{Arc, Mutex, MutexGuard};

/// Defines the different options for outputting diagnostics,
/// once they've been drained from the the diagnostic context ([`DiagCtx`]).
pub enum DiagOutputFormat {
    /// The output should be graphical, i.e. should be forwarded
    /// to the user, so errors and warnings are easy to understand and read.
    Graphical,

    /// No output will be rendered and all diagnosticsw will be sent to the void.
    Stubbed,
}

/// A context to deal with diagnostics, which is meant to
/// be used throughout the entire lifespan of the compiler / driver
/// process.
///
/// Certain diagnostics may cause a single stage within the compiler
/// to halt or exit early, where-as others might be more benign.
pub struct DiagCtx {
    /// The inner handler for diagnostics, which holds all the
    /// reporting diagnostics.
    handler: Arc<Mutex<DiagnosticHandler>>,
}

impl DiagCtx {
    /// Creates a new [`DiagCtx`] instance using the given output format.
    pub fn new(fmt: DiagOutputFormat) -> Self {
        let renderer: Box<dyn Renderer + Send + Sync> = match fmt {
            DiagOutputFormat::Graphical => Box::new(error_snippet::GraphicalRenderer::new()),
            DiagOutputFormat::Stubbed => Box::new(StubRenderer {}),
        };

        let handler = error_snippet::DiagnosticHandler::with_renderer(renderer);

        DiagCtx {
            handler: Arc::new(Mutex::new(handler)),
        }
    }

    /// Create a handle for the diagnostic context, which can be
    /// used to emit diagnositcs to the inner context.
    pub fn handle(&mut self) -> DiagCtxHandle {
        DiagCtxHandle {
            inner: Arc::clone(&self.handler),
        }
    }

    /// Creates a new handle, which is only valid within the given closure,
    /// which is executed immedietly. Upon finishing the closure, the handle is dropped
    /// and all diagnostics reporting within it are immedietly drained to the inner handler.
    pub fn with<TReturn>(&mut self, mut f: impl FnMut(DiagCtxHandle) -> TReturn) -> TReturn {
        let handle = self.handle();

        f(handle)
    }
}

/// A handle to a parent [`DiagCtx`], which can be used in
/// distinct sequential "stages", where each stage can only progress
/// forward if no halting diagnostics were reporting in any of the previous
/// stages.
///
/// The handle acts as a mutable reference to it's parent [`DiagCtx`] instance,
/// but will drain all errors to the output, once it's been dropped or manually drained.
pub struct DiagCtxHandle {
    /// Contains the parent [`DiagCtx`] handler.
    inner: Arc<Mutex<DiagnosticHandler>>,
}

impl DiagCtxHandle {
    /// Creates a new [`DiagCtxHandle`] with a stubbed renderer, functioning
    /// similar to a shim. Mostly used for testing.
    pub fn shim() -> Self {
        DiagCtx::new(DiagOutputFormat::Stubbed).handle()
    }

    /// Retrives the instance of the parent [`DiagnosticHandler`], which
    /// is contained within the handle.
    fn handler(&mut self) -> MutexGuard<'_, DiagnosticHandler> {
        self.inner.lock().unwrap()
    }

    /// Emits the given error to the parent context.
    ///
    /// Depending on the severity of the error, it might cause the
    /// execution of the program to halt (e.g. fatal errors, bugs, etc.), but
    /// most would simply be reporting until drained from the context.
    pub fn emit(&mut self, diagnostic: error_snippet::Error) {
        self.handler().report(diagnostic);
    }
}

unsafe impl Send for DiagCtxHandle {}
unsafe impl Sync for DiagCtxHandle {}

impl Drop for DiagCtxHandle {
    fn drop(&mut self) {
        self.handler().drain().unwrap()
    }
}

/// Defines a [`Renderer`] for handlers, which don't report any errors.
/// Instead, it sends the reported diagnostics to the void, where they won't disturb anyone.
struct StubRenderer {}

impl Renderer for StubRenderer {
    fn render_fmt(
        &mut self,
        _f: &mut error_snippet::Formatter,
        _diagnostic: &dyn error_snippet::Diagnostic,
    ) -> std::fmt::Result {
        Ok(())
    }
}
