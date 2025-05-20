#![allow(clippy::arc_with_non_send_sync)]
#![feature(negative_impls)]

use error_snippet::{DiagnosticHandler, Handler, Renderer, SimpleDiagnostic};
use std::sync::{
    Arc, Mutex, MutexGuard,
    atomic::{AtomicBool, Ordering},
};

/// Derived from the Rust compiler.
///
/// [Original source](https://github.com/rust-lang/rust/blob/c79bbfab78dcb0a72aa3b2bc35c00334b58bfe2e/compiler/rustc_span/src/fatal_error.rs)
#[derive(Debug, Clone, Copy)]
pub struct FatalError;

pub struct FatalErrorMarker;

// Don't implement Send on FatalError. This makes it impossible to `panic_any!(FatalError)`.
// We don't want to invoke the panic handler and print a backtrace for fatal errors.
impl !Send for FatalError {}

impl FatalError {
    pub fn raise() -> ! {
        std::panic::resume_unwind(Box::new(FatalErrorMarker))
    }
}

impl std::fmt::Display for FatalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fatal error occured, aborting...")
    }
}

impl std::error::Error for FatalError {}

/// Defines the different options for outputting diagnostics,
/// once they've been drained from the the diagnostic context ([`DiagCtx`]).
#[derive(Clone, Copy)]
pub enum DiagOutputFormat {
    /// The output should be graphical, i.e. should be forwarded
    /// to the user, so errors and warnings are easy to understand and read.
    Graphical,

    /// No output will be rendered and all diagnosticsw will be sent to the void.
    Stubbed,
}

/// A struct for handling diagnostics.
///
/// The struct is only defined on [`DiagCtx`]-instances, and is only meant to
/// be shared with handles created by them (via the [`DiagCtx::handle()`] and [`DiagCtx::with()`] methods).
struct DiagCtxInner {
    /// The inner handler for diagnostics, which holds all the
    /// reporting diagnostics.
    handler: DiagnosticHandler,

    /// Defines whether the context has reported any errors upon draining
    /// and should exit at the soonest possible convenience.
    tainted: AtomicBool,

    /// Defines whether to halt the application, if an error is encountered
    /// during a drain.
    exit_on_error: bool,
}

impl DiagCtxInner {
    /// Creates a new [`DiagCtxInner`] instance using the given output format.
    fn new(fmt: DiagOutputFormat) -> Self {
        let renderer: Box<dyn Renderer + Send + Sync> = match fmt {
            DiagOutputFormat::Graphical => Box::new(error_snippet::GraphicalRenderer::new()),
            DiagOutputFormat::Stubbed => Box::new(StubRenderer {}),
        };

        let mut handler = error_snippet::DiagnosticHandler::with_renderer(renderer);

        // If we drain an error to the handler, propagate the error count upwards, so
        // we can handle it at a higher level, instead of exiting the application.
        handler.exit_on_error();

        DiagCtxInner {
            handler,
            tainted: AtomicBool::new(false),
            exit_on_error: false,
        }
    }

    /// Drains the currently reported errors in the context to the output buffer.
    ///
    /// For more information, read the documentation on [`DiagCtxHandle::drain()`].
    fn drain(&mut self) {
        let encountered_errors = match self.handler.drain() {
            Ok(()) => return,
            Err(error_snippet::DrainError::Fmt(e)) => panic!("{e:?}"),
            Err(error_snippet::DrainError::CompoundError(cnt)) => cnt,
        };

        let message = format!("aborting due to {encountered_errors} previous errors");
        let abort_diag = Box::new(SimpleDiagnostic::new(message));

        let _ = self.handler.report_and_drain(abort_diag);

        if self.exit_on_error {
            // Raise a fatal error, without printing a backtrace.
            FatalError::raise();
        } else {
            // Mark the context as tainted.
            self.tainted.store(true, Ordering::Release);
        }
    }
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
    inner: Arc<Mutex<DiagCtxInner>>,
}

impl DiagCtx {
    /// Creates a new [`DiagCtx`] instance using the given output format.
    pub fn new(fmt: DiagOutputFormat) -> Self {
        let inner = DiagCtxInner::new(fmt);

        DiagCtx {
            inner: Arc::new(Mutex::new(inner)),
        }
    }

    /// Retrives the instance of the parent [`DiagCtxInner`], which
    /// is contained within the context.
    fn handler(&self) -> MutexGuard<'_, DiagCtxInner> {
        self.inner.lock().unwrap()
    }

    /// Enables the context to halt the application, if an error is encountered
    /// during a drain.
    pub fn exit_on_error(&mut self) {
        self.handler().exit_on_error = true;
    }

    /// Create a handle for the diagnostic context, which can be
    /// used to emit diagnositcs to the inner context.
    pub fn handle(&mut self) -> DiagCtxHandle {
        DiagCtxHandle {
            inner: Arc::clone(&self.inner),
        }
    }

    /// Creates a new handle, which is only valid within the given closure,
    /// which is executed immedietly. Upon finishing the closure, the handle is dropped
    /// and all diagnostics reporting within it are immedietly drained to the inner handler.
    pub fn with<TReturn>(&mut self, f: impl FnOnce(DiagCtxHandle) -> TReturn) -> TReturn {
        let handle = self.handle();

        f(handle)
    }

    /// Determines whether the diagnostic context is tainted.
    ///
    /// The context can become tainted if one-or-more errors are reporting
    /// during a drain to the parent handler. This does not include warnings or
    /// sub-diagnostics.
    ///
    /// # Examples
    ///
    /// Using [`DiagCtx::with()`]:
    /// ```
    /// use error_snippet::SimpleDiagnostic;
    /// use lume_errors::{DiagCtx, DiagOutputFormat};
    ///
    /// let mut dcx = DiagCtx::new(DiagOutputFormat::Stubbed);
    ///
    /// dcx.with(|mut handle| {
    ///     let error = SimpleDiagnostic::new("An error occurred");
    ///     handle.emit(error.into());
    /// });
    ///
    /// assert_eq!(dcx.tainted(), true);
    /// ```
    ///
    /// Errors emitted to the [`DiagCtxHandle`] handle are only drained when
    /// calling the [`DiagCtxHandle::drain()`] method manually, or when the handle
    /// is dropped. So, the following example will *not* taint the context:
    /// ```
    /// use error_snippet::SimpleDiagnostic;
    /// use lume_errors::{DiagCtx, DiagOutputFormat};
    ///
    /// let mut dcx = DiagCtx::new(DiagOutputFormat::Stubbed);
    /// let mut handle = dcx.handle();
    ///
    /// let error = SimpleDiagnostic::new("An error occurred");
    /// handle.emit(error.into());
    ///
    /// // `handle` is not dropped, so no errors are drained!
    /// assert_eq!(dcx.tainted(), false);
    ///
    /// // we can manually drain using [`DiagCtxHandle::drain()`] or
    /// // simply drop the handle.
    /// drop(handle);
    /// assert_eq!(dcx.tainted(), true);
    /// ```
    pub fn tainted(&self) -> bool {
        self.handler().tainted.load(Ordering::Acquire)
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
    /// Contains the parent [`DiagCtxInner`] handler.
    inner: Arc<Mutex<DiagCtxInner>>,
}

impl DiagCtxHandle {
    /// Creates a new [`DiagCtxHandle`] with a stubbed renderer, functioning
    /// similar to a shim. Mostly used for testing.
    pub fn shim() -> Self {
        DiagCtx::new(DiagOutputFormat::Stubbed).handle()
    }

    /// Retrives the instance of the parent [`DiagCtxInner`], which
    /// is contained within the handle.
    fn handler(&self) -> MutexGuard<'_, DiagCtxInner> {
        self.inner.lock().unwrap()
    }

    /// Emits the given error to the parent context.
    ///
    /// Depending on the severity of the error, it might cause the
    /// execution of the program to halt (e.g. fatal errors, bugs, etc.), but
    /// most would simply be reporting until drained from the context.
    #[track_caller]
    pub fn emit(&mut self, diagnostic: error_snippet::Error) {
        self.handler().handler.report(diagnostic);
    }

    /// Drains the currently reported errors in the context to the output buffer.
    ///
    /// If any reported diagnostics have a severity at or above [`error_snippet::Severity::Error`],
    /// they will be counted towards a [`error_snippet::DrainError::CompoundError`], which will be
    /// raised when draining has finished.
    pub fn drain(&mut self) {
        self.handler().drain();
    }

    /// Determines whether the diagnostic context is tainted.
    ///
    /// For more information, read the documentation on [`DiagCtx::tainted()`].
    pub fn tainted(&self) -> bool {
        self.handler().tainted.load(Ordering::Acquire)
    }

    /// Create a handle for the diagnostic context, which can be
    /// used to emit diagnositcs to the inner context.
    #[must_use]
    pub fn handle(&mut self) -> DiagCtxHandle {
        DiagCtxHandle {
            inner: Arc::clone(&self.inner),
        }
    }

    /// Creates a new handle, which is only valid within the given closure,
    /// which is executed immedietly. Upon finishing the closure, the handle is dropped
    /// and all diagnostics reporting within it are immedietly drained to the inner handler.
    pub fn with<TReturn>(&mut self, f: impl FnOnce(DiagCtxHandle) -> TReturn) -> TReturn {
        let handle = self.handle();

        f(handle)
    }
}

unsafe impl Send for DiagCtxHandle {}
unsafe impl Sync for DiagCtxHandle {}

impl Drop for DiagCtxHandle {
    fn drop(&mut self) {
        self.drain();
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
