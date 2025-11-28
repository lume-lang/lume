#![allow(clippy::arc_with_non_send_sync)]

pub const ERROR_GUARANTEED_CODE: &str = "FINAL_ERROR";

pub extern crate error_snippet_derive;

use std::sync::{Arc, Mutex, MutexGuard};

pub use error_snippet::{Diagnostic, Error, GraphicalRenderer, IntoDiagnostic, Result, SimpleDiagnostic};
use error_snippet::{Renderer, Severity};
pub use error_snippet_derive::Diagnostic;

/// A context to deal with diagnostics, which is meant to
/// be used throughout the entire lifespan of the compiler / driver
/// process.
///
/// Certain diagnostics may cause a single stage within the compiler
/// to halt or exit early, where-as others might be more benign.
#[derive(Default)]
pub struct DiagCtxInner {
    /// Holding block for all the reported diagnostics.
    emitted: Vec<Error>,

    /// Tracks the location of where diagnostics are pushed from.
    track_diagnostics: bool,

    /// Treat all errors as bugs, causing a `panic!`.
    panic_on_error: bool,
}

impl DiagCtxInner {
    /// Renders all the stored diagnostics to the standard error output
    /// (`stderr`).
    fn render_stderr(&self, renderer: &mut impl Renderer) {
        if let Some(buffer) = self.render_buffer(renderer) {
            eprint!("{buffer}");
        }
    }

    /// Renders all the stored diagnostics into a [`String`]
    fn render_buffer(&self, renderer: &mut impl Renderer) -> Option<String> {
        if self.emitted.is_empty() {
            return None;
        }

        let buffer = self
            .iter()
            .map(|diagnostic| renderer.render(diagnostic.as_ref()).unwrap())
            .collect::<String>();

        Some(buffer)
    }

    /// Clears all the diagnostics from the context.
    fn clear(&mut self) {
        self.emitted.clear();
    }

    /// Pushes the given diagnostic to the context.
    #[track_caller]
    fn push(&mut self, diag: Error) {
        if diag.message().as_str() == ERROR_GUARANTEED_CODE {
            return;
        }

        if self.track_diagnostics {
            eprintln!("[track_diagnostics] pushed from {}", std::panic::Location::caller());
        }

        assert!(!self.panic_on_error, "error emitted with `panic_on_error` enabled: {}", diag.message());

        self.emitted.push(diag);
    }

    /// Iterates over all the diagnostics within the context.
    fn iter(&self) -> impl Iterator<Item = &Error> {
        self.emitted.iter()
    }

    /// Invokes the given closure with an iterator over all reported
    /// diagnostics.
    fn with_iter<F, R>(&self, f: F) -> R
    where
        F: for<'a> FnOnce(std::slice::Iter<'a, Error>) -> R,
    {
        f(self.emitted.iter())
    }

    /// Determines whether the diagnostic context has been tainted with
    /// one-or-more errors.
    fn is_tainted(&self) -> bool {
        self.emitted.iter().any(|diag| diag.severity() == Severity::Error)
    }
}

/// A context to deal with diagnostics, which is meant to
/// be used throughout the entire lifespan of the compiler / driver
/// process.
///
/// Certain diagnostics may cause a single stage within the compiler
/// to halt or exit early, where-as others might be more benign.
#[derive(Clone, Default)]
pub struct DiagCtx {
    /// The inner handler for diagnostics, which holds all the
    /// reporting diagnostics.
    inner: Arc<Mutex<DiagCtxInner>>,
}

impl DiagCtx {
    /// Creates a new [`DiagCtx`] instance using the given output format.
    pub fn new() -> Self {
        DiagCtx::default()
    }

    /// Retrives the instance of the parent [`DiagCtxInner`], which
    /// is contained within the context.
    ///
    /// # Panics
    ///
    /// Panics if the inner diagnostics context has been locked by another
    /// thread.
    fn inner(&self) -> MutexGuard<'_, DiagCtxInner> {
        self.inner.lock().unwrap()
    }

    /// Prints the location of where diagnostics are pushed to the context - the
    /// error does not have to be emitted.
    ///
    /// # Panics
    ///
    /// Panics if the handle has already been locked by another thread.
    pub fn track_diagnostics(&self) {
        self.inner().track_diagnostics = true;
    }

    /// Enables panicking whenever an error is pushed to the context - the error
    /// does not have to be emitted.
    ///
    /// # Panics
    ///
    /// Panics if the handle has already been locked by another thread.
    pub fn panic_on_error(&self) {
        self.inner().panic_on_error = true;
    }

    /// Emits the given diagnostic to the context directly, without
    /// passing any handles or instances around.
    ///
    /// # Panics
    ///
    /// Panics if the handle has already been locked by another thread.
    #[track_caller]
    pub fn emit(&self, diag: Error) {
        self.inner().push(diag);
    }

    /// Create a handle for the diagnostic context, which can be
    /// used to emit diagnositcs to the inner context.
    pub fn handle(&self) -> DiagCtxHandle {
        DiagCtxHandle {
            inner: Arc::clone(&self.inner),
            emitted: Arc::new(Mutex::new(Vec::new())),
        }
    }

    /// Invokes the given closure with an iterator over all reported
    /// diagnostics.
    ///
    /// # Panics
    ///
    /// Panics if the inner diagnostics context has been locked by another
    /// thread.
    pub fn with_iter<F, R>(&self, f: F) -> R
    where
        F: for<'a> FnOnce(std::slice::Iter<'a, Error>) -> R,
    {
        let guard = self.inner.lock().unwrap();

        guard.with_iter(f)
    }

    /// Determines whether the diagnostic context has been tainted with
    /// one-or-more errors.
    pub fn is_tainted(&self) -> bool {
        self.inner().is_tainted()
    }

    /// Ensure that the context is untainted.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the context is tainted with one-or-more errors.
    pub fn ensure_untainted(&self) -> Result<()> {
        if self.is_tainted() {
            Err(TaintedError(()).into())
        } else {
            Ok(())
        }
    }

    /// Renders all the stored diagnostics to the standard error output
    /// (`stderr`).
    pub fn render_stderr(&self, renderer: &mut impl Renderer) {
        self.inner().render_stderr(renderer);
    }

    /// Renders all the stored diagnostics into a [`String`]
    pub fn render_buffer(&self, renderer: &mut impl Renderer) -> Option<String> {
        self.inner().render_buffer(renderer)
    }

    /// Clears all the diagnostics from the context.
    pub fn clear(&self) {
        self.inner().clear();
    }

    /// Creates a new handle, which is only valid within the given closure,
    /// which is executed immediately. Upon finishing the closure, the handle is
    /// dropped and all diagnostics reporting within it are immediately
    /// pushed to the inner handler.
    pub fn with_none(&self, f: impl FnOnce(DiagCtxHandle)) {
        let handle = self.handle();
        f(handle.clone());

        handle.push();
    }

    /// Creates a new handle, which is only valid within the given closure,
    /// which is executed immediately. Upon finishing the closure, the handle is
    /// dropped and all diagnostics reporting within it are immediately
    /// pushed to the inner handler.
    ///
    /// # Errors
    ///
    /// Returns `Err` if an error occured while executing the closure or if the
    /// closure itself returned `Err`.
    pub fn with_res<TReturn>(&self, f: impl FnOnce(DiagCtxHandle) -> TReturn) -> Result<TReturn> {
        let handle = self.handle();
        let res = f(handle.clone());

        handle.push();

        self.ensure_untainted()?;

        Ok(res)
    }

    /// Creates a new handle, which is only valid within the given closure,
    /// which is executed immediately. Upon finishing the closure, the handle is
    /// dropped and all diagnostics reporting within it are immediately
    /// pushed to the inner handler.
    ///
    /// # Errors
    ///
    /// Returns `Err` if an error occured while executing the closure or if the
    /// closure itself returned `Err`.
    pub fn with<TReturn>(&self, f: impl FnOnce(DiagCtxHandle) -> Result<TReturn>) -> Result<TReturn> {
        let handle = self.handle();
        let res = f(handle.clone());

        handle.push();

        self.ensure_untainted()?;

        res
    }

    /// Creates a new handle, which is only valid within the given closure,
    /// which is executed immediately. Upon finishing the closure, the handle is
    /// dropped and all diagnostics reporting within it are immediately
    /// pushed to the inner handler.
    pub fn with_opt<TReturn>(&self, f: impl FnOnce(DiagCtxHandle) -> Result<TReturn>) -> Option<TReturn> {
        let handle = self.handle();

        match f(handle.clone()) {
            Ok(value) => Some(value),
            Err(err) => {
                handle.emit_and_push(err);
                None
            }
        }
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
/// but will drain all errors to the output, once it's been dropped or manually
/// drained.
#[derive(Clone)]
pub struct DiagCtxHandle {
    /// Contains the parent [`DiagCtxInner`] handler.
    inner: Arc<Mutex<DiagCtxInner>>,

    /// Holding block for all the reported diagnostics.
    emitted: Arc<Mutex<Vec<Error>>>,
}

impl DiagCtxHandle {
    /// Creates a new [`DiagCtxHandle`], functioning  similar to a shim. Mostly
    /// used for testing.
    pub fn shim() -> Self {
        DiagCtx::new().handle()
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
    fn inner(&self) -> MutexGuard<'_, DiagCtxInner> {
        self.inner.lock().unwrap()
    }

    /// Emits the given diagnostic to the context directly, without
    /// passing any handles or instances around.
    ///
    /// # Panics
    ///
    /// Panics if the handle has already been locked by another thread.
    #[track_caller]
    pub fn emit(&self, diag: Error) {
        if diag.message().as_str() == ERROR_GUARANTEED_CODE {
            return;
        }

        self.emitted.lock().unwrap().push(diag);
    }

    /// Emits the given diagnostic to the context directly and pushes
    /// it directly to the parent context.
    #[track_caller]
    pub fn emit_and_push(&self, diag: Error) {
        self.emit(diag);
        self.push();
    }

    /// Drains the currently reported errors in the context to the output
    /// buffer.
    ///
    /// # Errors
    ///
    /// If any reported diagnostics have a severity at or above
    /// [`error_snippet::Severity::Error`], they will be counted towards a
    /// [`error_snippet::DrainError::CompoundError`], which will be
    /// raised when draining has finished.
    ///
    /// # Panics
    ///
    /// Panics if the handle has already been locked by another thread.
    pub fn push(&self) {
        let mut emitted = self.emitted.lock().unwrap();

        self.inner().emitted.append(&mut emitted);
    }

    /// Create a handle for the diagnostic context, which can be
    /// used to emit diagnositcs to the inner context.
    #[must_use]
    pub fn handle(&self) -> DiagCtxHandle {
        DiagCtxHandle {
            inner: Arc::clone(&self.inner),
            emitted: Arc::new(Mutex::new(Vec::new())),
        }
    }

    /// Creates a new handle, which is only valid within the given closure,
    /// which is executed immediately. Upon finishing the closure, the handle is
    /// dropped and all diagnostics reporting within it are immediately
    /// pushed to the inner handler.
    ///
    /// # Errors
    ///
    /// Returns `Err` if an error occured while executing the closure or if the
    /// closure itself returned `Err`.
    pub fn with_res<TReturn>(&self, f: impl FnOnce(DiagCtxHandle) -> TReturn) -> Result<TReturn> {
        let res = f(self.clone());
        self.push();

        Ok(res)
    }

    /// Creates a new handle, which is only valid within the given closure,
    /// which is executed immediately. Upon finishing the closure, the handle is
    /// dropped and all diagnostics reporting within it are immediately
    /// pushed to the inner handler.
    ///
    /// # Errors
    ///
    /// Returns `Err` if an error occured while executing the closure or if the
    /// closure itself returned `Err`.
    pub fn with<TReturn>(&self, f: impl FnOnce(DiagCtxHandle) -> Result<TReturn>) -> Result<TReturn> {
        let res = f(self.clone());
        self.push();

        res
    }

    /// Ensure that the context is untainted.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the context is tainted with one-or-more errors.
    pub fn ensure_untainted(&self) -> Result<()> {
        if self.inner().is_tainted() {
            Err(TaintedError(()).into())
        } else {
            Ok(())
        }
    }
}

unsafe impl Send for DiagCtxHandle {}
unsafe impl Sync for DiagCtxHandle {}

#[derive(Debug, Clone)]
struct TaintedError(());

impl error_snippet::Diagnostic for TaintedError {
    fn message(&self) -> String {
        String::from(ERROR_GUARANTEED_CODE)
    }
}

pub trait MapDiagnostic<T> {
    /// If the instance is a [`std::result::Result::Err`], maps it into
    /// an instance of [`Diagnostic`] (via
    /// [`IntoDiagnostic::into_diagnostic`]).
    fn map_diagnostic(self) -> Result<T>;

    /// If the instance is a [`std::result::Result::Err`], declares it as a
    /// cause of a new [`Diagnostic`] with the given message.
    ///
    /// This method is effectively an alias of:
    /// ```rs
    /// self.map_err(|err| SimpleDiagnostic::new(message)
    ///     .add_cause(err.into_diagnostic())
    /// )
    /// ```
    fn map_cause(self, message: impl Into<String>) -> Result<T>;
}

impl<T, E: std::error::Error + Send + Sync> MapDiagnostic<T> for std::result::Result<T, E> {
    fn map_diagnostic(self) -> Result<T> {
        self.map_err(IntoDiagnostic::into_diagnostic)
    }

    fn map_cause(self, message: impl Into<String>) -> Result<T> {
        self.map_err(|err| {
            let diag = SimpleDiagnostic::new(message).add_cause(err.into_diagnostic());

            Box::new(diag) as Error
        })
    }
}
