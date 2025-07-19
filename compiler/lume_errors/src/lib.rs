#![allow(clippy::arc_with_non_send_sync)]
#![feature(negative_impls)]

pub use error_snippet::Error;
pub use error_snippet::Result;

use error_snippet::{
    BufferedDiagnosticHandler, DiagnosticHandler, GraphicalRenderer, Handler, Renderer, SimpleDiagnostic,
};
use std::sync::{
    Arc, Mutex, MutexGuard,
    atomic::{AtomicBool, Ordering},
};

/// Defines the different options for outputting diagnostics,
/// once they've been drained from the the diagnostic context ([`DiagCtx`]).
#[derive(Clone, Copy, PartialEq, Eq)]
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
pub struct DiagCtxInner {
    /// Defines whether the context should request the renderer to use colors.
    use_colors: bool,

    /// The inner handler for diagnostics, which holds all the
    /// reporting diagnostics.
    handler: Box<dyn Handler>,

    /// Defines whether the context has reported any errors upon draining
    /// and should exit at the soonest possible convenience.
    tainted: AtomicBool,
}

impl DiagCtxInner {
    /// Creates a new [`DiagCtxInner`] instance using the given output format.
    fn new(fmt: DiagOutputFormat) -> Self {
        let use_colors = fmt == DiagOutputFormat::Graphical;

        let renderer: Box<dyn Renderer + Send + Sync> = match fmt {
            DiagOutputFormat::Graphical => {
                let mut renderer = GraphicalRenderer::default();
                renderer.use_colors = use_colors;
                renderer.highlight_source = true;

                Box::new(renderer)
            }
            DiagOutputFormat::Stubbed => Box::new(StubRenderer {}),
        };

        let mut handler = DiagnosticHandler::with_renderer(renderer);

        // If we drain an error to the handler, propagate the error count upwards, so
        // we can handle it at a higher level, instead of exiting the application.
        handler.exit_on_error();

        Self::with_handler(Box::new(handler), use_colors)
    }

    /// Creates a new [`DiagCtxInner`] instance using the given [`Handler`] implementation.
    fn with_handler(handler: Box<dyn Handler>, use_colors: bool) -> Self {
        DiagCtxInner {
            use_colors,
            handler,
            tainted: AtomicBool::new(false),
        }
    }

    /// Creates a new [`DiagCtxInner`] instance with buffered, graphical output.
    fn new_buffered(capacity: usize) -> Self {
        let mut renderer = GraphicalRenderer::new();
        renderer.use_colors = false;

        let handler = BufferedDiagnosticHandler::with_renderer(capacity, Box::new(renderer));

        DiagCtxInner {
            use_colors: false,
            handler: Box::new(handler),
            tainted: AtomicBool::new(false),
        }
    }

    /// Drains the currently reported errors in the context to the output buffer.
    ///
    /// For more information, read the documentation on [`DiagCtxHandle::drain()`].
    fn drain(&mut self) -> Result<()> {
        let encountered_errors = owo_colors::with_override(self.use_colors, || match self.handler.drain() {
            Ok(()) => 0,
            Err(error_snippet::DrainError::Fmt(e)) => panic!("{e:?}"),
            Err(error_snippet::DrainError::CompoundError(cnt)) => cnt,
        });

        if encountered_errors == 0 {
            return Ok(());
        }

        // Mark the context as tainted.
        self.tainted.store(true, Ordering::Release);

        let message = format!(
            "aborting due to {encountered_errors} previous error{}",
            if encountered_errors == 1 { "" } else { "s" }
        );

        Err(SimpleDiagnostic::new(message).into())
    }

    /// Returns the buffered content of the diagnostics context.
    ///
    /// If the context is not buffered, returns [`None`]. Otherwise, returns the buffer
    /// as a [`Some(String)`]. To create a buffered context, see [`DiagCtxInner::new_buffered()`].
    pub fn buffer(&self) -> Option<String> {
        let handler = self.handler.as_ref() as &dyn std::any::Any;

        handler
            .downcast_ref::<error_snippet::BufferedDiagnosticHandler>()
            .map(|buf| buf.buffer().to_owned())
    }

    /// Returns the emitted diagnostics of the diagnostics context.
    ///
    /// This method only returns drained diagnostics. Reported diagnostics which have
    /// not yet been drained are not returned.
    pub fn emitted(&self) -> impl Iterator<Item = &Box<dyn error_snippet::Diagnostic>> {
        let handler = self.handler.as_ref() as &dyn std::any::Any;

        handler
            .downcast_ref::<StubHandler>()
            .map_or_else(|| [].iter(), |handler| handler.drained.iter())
    }
}

/// A context to deal with diagnostics, which is meant to
/// be used throughout the entire lifespan of the compiler / driver
/// process.
///
/// Certain diagnostics may cause a single stage within the compiler
/// to halt or exit early, where-as others might be more benign.
#[derive(Clone)]
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

    /// Creates a new [`DiagCtx`] instance using the given [`Handler`] implementation.
    pub fn with_handler(handler: Box<dyn Handler>, use_colors: bool) -> Self {
        let inner = DiagCtxInner::with_handler(handler, use_colors);

        DiagCtx {
            inner: Arc::new(Mutex::new(inner)),
        }
    }

    /// Creates a new [`DiagCtx`] instance with buffered, graphical output.
    #[allow(dead_code, reason = "will only be used in snapshot tests")]
    pub fn new_buffered(capacity: usize) -> Self {
        let inner = DiagCtxInner::new_buffered(capacity);

        DiagCtx {
            inner: Arc::new(Mutex::new(inner)),
        }
    }

    /// Retrives the instance of the parent [`DiagCtxInner`], which
    /// is contained within the context.
    ///
    /// # Panics
    ///
    /// Panics if the inner diagnostics context has been locked by another thread.
    pub fn inner(&self) -> MutexGuard<'_, DiagCtxInner> {
        self.inner.lock().unwrap()
    }

    /// Create a handle for the diagnostic context, which can be
    /// used to emit diagnositcs to the inner context.
    fn handle(&self) -> DiagCtxHandle {
        DiagCtxHandle {
            inner: Arc::clone(&self.inner),
        }
    }

    /// Drains the currently reported errors in the context to the output buffer.
    ///
    /// # Errors
    ///
    /// If any reported diagnostics have a severity at or above [`error_snippet::Severity::Error`],
    /// they will be counted towards a [`error_snippet::DrainError::CompoundError`], which will be
    /// raised when draining has finished.
    pub fn drain(&mut self) -> Result<()> {
        self.inner().drain()
    }

    /// Emits the given diagnostic to the context directly, without
    /// passing any handles or instances around.
    ///
    /// The diagnostic will not be drained from the handler immediately. To
    /// drain the diagnostic, see [`DiagCtx::drain()`].
    pub fn emit(&self, diag: error_snippet::Error) {
        self.inner().handler.report(diag);
    }

    /// Creates a new handle, which is only valid within the given closure,
    /// which is executed immediately. Upon finishing the closure, the handle is dropped
    /// and all diagnostics reporting within it are immediately drained to the inner handler.
    ///
    /// # Errors
    ///
    /// Returns `Err` if an error occured while executing the closure or if the closure itself
    /// returned `Err`.
    pub fn with_res<TReturn>(&self, f: impl FnOnce(DiagCtxHandle) -> TReturn) -> Result<TReturn> {
        let handle = self.handle();
        let res = f(handle);

        self.inner().drain()?;

        Ok(res)
    }

    /// Creates a new handle, which is only valid within the given closure,
    /// which is executed immediately. Upon finishing the closure, the handle is dropped
    /// and all diagnostics reporting within it are immediately drained to the inner handler.
    ///
    /// # Errors
    ///
    /// Returns `Err` if an error occured while executing the closure or if the closure itself
    /// returned `Err`.
    pub fn with<TReturn>(&self, f: impl FnOnce(DiagCtxHandle) -> Result<TReturn>) -> Result<TReturn> {
        let handle = self.handle();
        let res = f(handle);

        self.inner().drain()?;

        res
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
    /// dcx.with_res(|mut handle| {
    ///     let error = SimpleDiagnostic::new("An error occurred");
    ///     handle.emit(error.into());
    /// });
    ///
    /// assert_eq!(dcx.tainted(), true);
    /// ```
    ///
    /// Errors emitted to the [`DiagCtxHandle`] handle are only drained when
    /// calling the [`DiagCtxHandle::drain()`] method manually, or when the handle
    /// is dropped.
    pub fn tainted(&self) -> bool {
        self.inner().tainted.load(Ordering::Acquire)
    }

    /// Returns the buffered content of the diagnostics context.
    ///
    /// If the context is not buffered, returns [`None`]. Otherwise, returns the buffer
    /// as a [`Some(String)`]. To create a buffered context, see [`DiagCtx::new_buffered()`].
    pub fn buffer(&self) -> Option<String> {
        self.inner().buffer()
    }
}

unsafe impl Send for DiagCtx {}
unsafe impl Sync for DiagCtx {}

/// A handle to a parent [`DiagCtx`], which can be used in
/// distinct sequential "stages", where each stage can only progress
/// forward if no halting diagnostics were reporting in any of the previous
/// stages.
///
/// The handle acts as a mutable reference to it's parent [`DiagCtx`] instance,
/// but will drain all errors to the output, once it's been dropped or manually drained.
#[derive(Clone)]
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

    /// Creates a [`DiagCtx`] from the given handle, which serves the same
    /// output as the handle itself.
    pub fn to_context(self) -> DiagCtx {
        DiagCtx {
            inner: self.inner.clone(),
        }
    }

    /// Retrives the instance of the parent [`DiagCtxInner`], which
    /// is contained within the handle.
    fn handler(&self) -> MutexGuard<'_, DiagCtxInner> {
        self.inner.lock().unwrap()
    }

    /// Emits the given diagnostic to the context directly, without
    /// passing any handles or instances around.
    ///
    /// The diagnostic will not be drained from the handler immediately. To
    /// drain the diagnostic, see [`DiagCtxHandle::drain()`].
    #[track_caller]
    pub fn emit(&self, diag: error_snippet::Error) {
        self.handler().handler.report(diag);
    }

    /// Drains the currently reported errors in the context to the output buffer.
    ///
    /// # Errors
    ///
    /// If any reported diagnostics have a severity at or above [`error_snippet::Severity::Error`],
    /// they will be counted towards a [`error_snippet::DrainError::CompoundError`], which will be
    /// raised when draining has finished.
    pub fn drain(&mut self) -> Result<()> {
        self.handler().drain()
    }

    /// Determines whether the diagnostic context is tainted.
    ///
    /// For more information, read the documentation on [`DiagCtx::tainted()`].
    pub fn tainted(&self) -> bool {
        self.handler().tainted.load(Ordering::Acquire)
    }

    /// Create a handle for the diagnostic context, which can be
    /// used to emit diagnositcs to the inner context.
    fn handle(&self) -> DiagCtxHandle {
        DiagCtxHandle {
            inner: Arc::clone(&self.inner),
        }
    }

    /// Creates a new handle, which is only valid within the given closure,
    /// which is executed immediately. Upon finishing the closure, the handle is dropped
    /// and all diagnostics reporting within it are immediately drained to the inner handler.
    ///
    /// # Errors
    ///
    /// Returns `Err` if an error occured while executing the closure or if the closure itself
    /// returned `Err`.
    pub fn with_res<TReturn>(&self, f: impl FnOnce(DiagCtxHandle) -> TReturn) -> Result<TReturn> {
        let handle = self.handle();
        let res = f(handle);

        self.handler().drain()?;

        Ok(res)
    }

    /// Creates a new handle, which is only valid within the given closure,
    /// which is executed immediately. Upon finishing the closure, the handle is dropped
    /// and all diagnostics reporting within it are immediately drained to the inner handler.
    ///
    /// # Errors
    ///
    /// Returns `Err` if an error occured while executing the closure or if the closure itself
    /// returned `Err`.
    pub fn with<TReturn>(&self, f: impl FnOnce(DiagCtxHandle) -> Result<TReturn>) -> Result<TReturn> {
        let handle = self.handle();
        let res = f(handle);

        self.handler().drain()?;

        res
    }
}

unsafe impl Send for DiagCtxHandle {}
unsafe impl Sync for DiagCtxHandle {}

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

/// Defines a [`Handler`] which doesn't report any errors.
/// Instead, it saves the reported diagnostics into an internal buffer, so they
/// can be read back later.
#[derive(Default)]
pub struct StubHandler {
    /// Defines all diagnostics which have been reported, but not drained.
    reported: Vec<Box<dyn error_snippet::Diagnostic>>,

    /// Defines all diagnostics which have been reported and drained.
    drained: Vec<Box<dyn error_snippet::Diagnostic>>,
}

impl Handler for StubHandler {
    fn report(&mut self, diagnostic: Box<dyn error_snippet::Diagnostic>) {
        self.reported.push(diagnostic);
    }

    fn drain(&mut self) -> std::result::Result<(), error_snippet::DrainError> {
        self.drained.reserve(self.reported.len());
        self.drained.extend(std::mem::take(&mut self.reported));

        Ok(())
    }
}
