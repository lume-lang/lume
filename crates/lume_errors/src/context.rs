#![allow(clippy::arc_with_non_send_sync)]

pub const ERROR_GUARANTEED_CODE: &str = "FINAL_ERROR";

use std::cell::RefCell;
use std::panic::RefUnwindSafe;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::{Error, IntoDiagnostic, Renderer, Result, Severity, SimpleDiagnostic};

/// A context to deal with diagnostics, which is meant to
/// be used throughout the entire lifespan of the compiler / driver
/// process.
///
/// Certain diagnostics may cause a single stage within the compiler
/// to halt or exit early, where-as others might be more benign.
#[derive(Default)]
struct DiagCtxInner {
    panic_on_error: AtomicBool,
    track_diagnostics: AtomicBool,

    emitted: RefCell<Vec<Error>>,
}

impl DiagCtxInner {
    /// Determines whether the diagnostic context has been tainted with
    /// one-or-more errors.
    #[inline]
    pub fn is_tainted(&self) -> bool {
        self.emitted
            .borrow()
            .iter()
            .any(|diag| diag.severity() >= Severity::Error)
    }
}

impl RefUnwindSafe for DiagCtxInner {}

/// A context to deal with diagnostics, which is meant to
/// be used throughout the entire lifespan of the compiler / driver
/// process.
///
/// Certain diagnostics may cause a single stage within the compiler
/// to halt or exit early, where-as others might be more benign.
#[derive(Default, Clone)]
pub struct DiagCtx {
    inner: Arc<DiagCtxInner>,
}

impl DiagCtx {
    /// Creates a new [`DiagCtx`] instance.
    pub fn new() -> Self {
        Self::default()
    }

    /// Prints the location of where diagnostics are pushed to the context - the
    /// error does not have to be emitted.
    ///
    /// # Panics
    ///
    /// Panics if the handle has already been locked by another thread.
    pub fn track_diagnostics(&self) {
        self.inner.track_diagnostics.store(true, Ordering::Relaxed);
    }

    /// Enables panicking whenever an error is pushed to the context - the error
    /// does not have to be emitted.
    ///
    /// # Panics
    ///
    /// Panics if the handle has already been locked by another thread.
    pub fn panic_on_error(&self) {
        self.inner.panic_on_error.store(true, Ordering::Relaxed);
    }

    /// Returns whether the context is empty, i.e. no diagnostics have been
    /// emitted.
    ///
    /// Note: this also includes non-errors!
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.inner.emitted.borrow().is_empty()
    }

    /// Returns the amount of *diagnostics* which can been emitted to the
    /// context.
    ///
    /// Note: this also includes non-errors!
    #[inline]
    pub fn len(&self) -> usize {
        self.inner.emitted.borrow().len()
    }

    /// Determines whether the diagnostic context has been tainted with
    /// one-or-more errors.
    pub fn is_tainted(&self) -> bool {
        self.inner.is_tainted()
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

    /// Iterates all the diagnostics which have been emitted to the diagnostics
    /// context.
    ///
    /// Note: this also includes non-errors!
    #[inline]
    pub fn take(&self) -> Vec<Error> {
        std::mem::take(&mut *self.inner.emitted.borrow_mut())
    }

    /// Clears all the diagnostics which have been emitted to the diagnostics
    /// context.
    #[inline]
    pub fn clear(&self) {
        self.inner.emitted.borrow_mut().clear();
    }

    /// Starts a new diagnostic transaction.
    ///
    /// When the transaction is dropped, the default behaviour is to ignore it,
    /// regardless of whether or not any errors were raised in the transaction.
    /// To change this behaviour, use [`Self::begin_transaction_with()`].
    pub fn begin_transaction(&self) -> Transaction<'_> {
        self.begin_transaction_with(Commit::default(), Rollback::default())
    }

    /// Starts a new diagnostic transaction, with the given operations for
    /// success and failure.
    ///
    /// When the transaction is dropped, the state of `on_success` and
    /// `on_failure` determines what happens to the transaction:
    ///
    /// **Commit:**
    /// - `Commit::Never`: the transaction is never automatically committed,
    /// - `Commit::OnSuccess`: the transaction is automatically committed if no
    ///   errors are raised in the `Transaction`,
    /// - `Commit::Always`: the transaction is committed when dropped
    ///   **(default)**
    ///
    /// **Rollback:**
    /// - `Rollback::Never`: the transaction is never automatically rolled back
    ///   **(default)**,
    /// - `Rollback::OnFailure`: the transaction is automatically rolled back if
    ///   one-or-more errors are raised in the `Transaction`,
    /// - `Rollback::Always`: the transaction is rolled back when the
    ///   transaction is dropped
    ///
    /// If either a call to [`Transaction::commit()`] or
    /// [`Transaction::rollback()`] occurs before the transaction is dropped,
    /// that call takes precedence.
    #[inline]
    pub fn begin_transaction_with(&self, on_success: Commit, on_failure: Rollback) -> Transaction<'_> {
        Transaction {
            dcx: Arc::clone(&self.inner),
            parent: TransactionParent::Context,
            emitted: RefCell::new(Vec::new()),
            on_success,
            on_failure,
        }
    }

    /// Runs the given closure inside of a transaction and returns the result of
    /// the closure.
    ///
    /// The transaction is passed to the closure, so it is the callers
    /// responsibility to commit or rollback the transaction. See
    /// [`Self::in_transaction_with()`] for more information.
    pub fn in_transaction<F, R>(&self, f: F) -> R
    where
        F: FnOnce(Transaction<'_>) -> R,
    {
        self.in_transaction_with(Commit::default(), Rollback::default(), f)
    }

    /// Runs the given closure inside of a transaction and returns the result of
    /// the closure.
    ///
    /// When the transaction is dropped, the state of `on_success` and
    /// `on_failure` determines what happens to the transaction:
    ///
    /// **Commit:**
    /// - `Commit::Never`: the transaction is never automatically committed,
    /// - `Commit::OnSuccess`: the transaction is automatically committed if no
    ///   errors are raised in the `Transaction`,
    /// - `Commit::Always`: the transaction is committed when dropped
    ///   **(default)**
    ///
    /// **Rollback:**
    /// - `Rollback::Never`: the transaction is never automatically rolled back
    ///   **(default)**,
    /// - `Rollback::OnFailure`: the transaction is automatically rolled back if
    ///   one-or-more errors are raised in the `Transaction`,
    /// - `Rollback::Always`: the transaction is rolled back when the
    ///   transaction is dropped
    ///
    /// If either a call to [`Transaction::commit()`] or
    /// [`Transaction::rollback()`] occurs before the transaction is dropped,
    /// that call takes precedence.
    #[inline]
    pub fn in_transaction_with<F, R>(&self, on_success: Commit, on_failure: Rollback, f: F) -> R
    where
        F: FnOnce(Transaction<'_>) -> R,
    {
        let transaction = self.begin_transaction_with(on_success, on_failure);
        f(transaction)
    }

    /// Emits the given diagnostic to the current transaction.
    ///
    /// # Note
    ///
    /// Since the diagnostic is emitted to the current transaction, it will only
    /// be pushed once the transaction is commited (via
    /// [`commit()`])
    ///
    /// [`commit()`]: Transaction::commit()
    #[track_caller]
    pub fn emit<E>(&self, diagnostic: E)
    where
        E: Into<Error>,
    {
        let diagnostic = diagnostic.into();

        if diagnostic.message().as_str() == ERROR_GUARANTEED_CODE {
            return;
        }

        #[allow(clippy::disallowed_macros, reason = "used for debugging")]
        if self.inner.track_diagnostics.load(Ordering::Relaxed) {
            eprintln!("[track_diagnostics] pushed from {}", std::panic::Location::caller());
        }

        assert!(
            !(diagnostic.severity() >= Severity::Error && self.inner.panic_on_error.load(Ordering::Relaxed)),
            "error emitted with `panic_on_error` enabled: {}",
            diagnostic.message()
        );

        self.inner.emitted.borrow_mut().push(diagnostic);
    }
}

impl DiagCtx {
    /// Renders all the stored diagnostics to the standard error output
    /// (`stderr`).
    pub fn render_stderr(&self, renderer: &mut impl Renderer) {
        if let Some(buffer) = self.render_buffer(renderer) {
            eprint!("{buffer}");
        }
    }

    /// Renders all the stored diagnostics into a [`String`]
    pub fn render_buffer(&self, renderer: &mut impl Renderer) -> Option<String> {
        if self.inner.emitted.borrow().is_empty() {
            return None;
        }

        let buffer = self
            .inner
            .emitted
            .borrow()
            .iter()
            .map(|diagnostic| renderer.render(diagnostic.as_ref()).unwrap())
            .collect::<String>();

        Some(buffer)
    }
}

unsafe impl Send for DiagCtx {}
unsafe impl Sync for DiagCtx {}

/// Defines when the transaction should commit.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Commit {
    /// Never automatically commit the transaction.
    Never,

    /// The transaction is automatically committed when no errors are raised.
    OnSuccess,

    /// Always commit the transaction.
    #[default]
    Always,
}

/// Defines when the transaction should roll back.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Rollback {
    /// Never automatically rollback the transaction.
    #[default]
    Never,

    /// The transaction is automatically rolled back if one-or-more errors are
    /// raised.
    OnFailure,

    /// Always rollback the transaction.
    Always,
}

/// Denotes where a [`Transaction`] should commit it's events to.
enum TransactionParent<'dcx> {
    /// When commited, events are applied to the parent [`DiagCtx`], defined in
    /// [`Transaction::dcx`].
    Context,

    /// When commited, events are applied to a parent [`Transaction`] instance.
    Transaction(&'dcx Transaction<'dcx>),
}

/// Represents a transaction.
///
/// Transactions allow for scoped operations, where all errors raised within the
/// scope can be atomically commited or rolled back. Until the transaction is
/// commited, no errors are applied to the parent diagnostics context.
///
/// Multiple transactions can exist at the same time and transactions can even
/// be created from other transactions.
pub struct Transaction<'dcx> {
    dcx: Arc<DiagCtxInner>,
    parent: TransactionParent<'dcx>,
    emitted: RefCell<Vec<Error>>,
    on_success: Commit,
    on_failure: Rollback,
}

impl Transaction<'_> {
    fn map_ref(&self) -> std::cell::RefMut<'_, Vec<Error>> {
        match self.parent {
            TransactionParent::Context => self.dcx.emitted.borrow_mut(),
            TransactionParent::Transaction(inner) => inner.emitted.borrow_mut(),
        }
    }

    /// Commits the transaction to the owning diagnostic context.
    pub fn commit(&mut self) {
        let emitted = std::mem::take(&mut *self.emitted.borrow_mut());

        for error in emitted {
            self.map_ref().push(error);
        }
    }

    /// Rolls back the transaction, discarding all the events which ocurred
    /// inside the transaction.
    pub fn rollback(&mut self) {
        self.emitted.borrow_mut().clear();
    }

    /// If the transaction is untainted with errors, commits the transaction.
    ///
    /// If the transaction is tainted, do nothing.
    pub fn commit_if_untainted(&mut self) {
        if !self.is_tainted() {
            self.commit();
        }
    }

    /// If the transaction is tainted with errors, roll back the transaction.
    ///
    /// If the transaction is not tainted, do nothing.
    pub fn rollback_if_tainted(&mut self) {
        if self.is_tainted() {
            self.rollback();
        }
    }

    /// Starts a new diagnostic sub-transaction, based on the current
    /// transaction.
    ///
    /// When the transaction is dropped, the default behaviour is to ignore it,
    /// regardless of whether or not any errors were raised in the transaction.
    /// To change this behaviour, use [`Self::begin_transaction_with()`].
    pub fn begin_transaction(&self) -> Transaction<'_> {
        self.begin_transaction_with(Commit::default(), Rollback::default())
    }

    /// Starts a new diagnostic transaction, with the given operations for
    /// success and failure.
    ///
    /// When the transaction is dropped, the state of `on_success` and
    /// `on_failure` determines what happens to the transaction:
    ///
    /// **Commit:**
    /// - `Commit::Never`: the transaction is never automatically committed,
    /// - `Commit::OnSuccess`: the transaction is automatically committed if no
    ///   errors are raised in the `Transaction`,
    /// - `Commit::Always`: the transaction is committed when dropped
    ///   **(default)**
    ///
    /// **Rollback:**
    /// - `Rollback::Never`: the transaction is never automatically rolled back
    ///   **(default)**,
    /// - `Rollback::OnFailure`: the transaction is automatically rolled back if
    ///   one-or-more errors are raised in the `Transaction`,
    /// - `Rollback::Always`: the transaction is rolled back when the
    ///   transaction is dropped
    ///
    /// If either a call to [`Transaction::commit()`] or
    /// [`Transaction::rollback()`] occurs before the transaction is dropped,
    /// that call takes precedence.
    #[inline]
    pub fn begin_transaction_with(&self, on_success: Commit, on_failure: Rollback) -> Transaction<'_> {
        Transaction {
            dcx: Arc::clone(&self.dcx),
            parent: TransactionParent::Transaction(self),
            emitted: RefCell::new(Vec::new()),
            on_success,
            on_failure,
        }
    }

    /// Runs the given closure inside of a transaction and returns the result of
    /// the closure.
    ///
    /// The transaction is passed to the closure, so it is the callers
    /// responsibility to commit or rollback the transaction. See
    /// [`Self::in_transaction_with()`] for more information.
    pub fn in_transaction<F, R>(&self, f: F) -> R
    where
        F: FnOnce(Transaction<'_>) -> R,
    {
        self.in_transaction_with(Commit::default(), Rollback::default(), f)
    }

    /// Runs the given closure inside of a transaction and returns the result of
    /// the closure.
    ///
    /// When the transaction is dropped, the state of `on_success` and
    /// `on_failure` determines what happens to the transaction:
    ///
    /// **Commit:**
    /// - `Commit::Never`: the transaction is never automatically committed,
    /// - `Commit::OnSuccess`: the transaction is automatically committed if no
    ///   errors are raised in the `Transaction`,
    /// - `Commit::Always`: the transaction is committed when dropped
    ///   **(default)**
    ///
    /// **Rollback:**
    /// - `Rollback::Never`: the transaction is never automatically rolled back
    ///   **(default)**,
    /// - `Rollback::OnFailure`: the transaction is automatically rolled back if
    ///   one-or-more errors are raised in the `Transaction`,
    /// - `Rollback::Always`: the transaction is rolled back when the
    ///   transaction is dropped
    ///
    /// If either a call to [`Transaction::commit()`] or
    /// [`Transaction::rollback()`] occurs before the transaction is dropped,
    /// that call takes precedence.
    #[inline]
    pub fn in_transaction_with<F, R>(&self, on_success: Commit, on_failure: Rollback, f: F) -> R
    where
        F: FnOnce(Transaction<'_>) -> R,
    {
        let transaction = self.begin_transaction_with(on_success, on_failure);
        f(transaction)
    }

    /// Returns whether the transaction is empty, i.e. no diagnostics have been
    /// emitted.
    ///
    /// Note: this also includes non-errors!
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.dcx.emitted.borrow().is_empty()
    }

    /// Returns the amount of *diagnostics* which can been emitted to the
    /// transaction.
    ///
    /// Note: this also includes non-errors!
    #[inline]
    pub fn len(&self) -> usize {
        self.dcx.emitted.borrow().len()
    }

    /// Determines whether the transaction has been tainted with
    /// one-or-more errors.
    #[inline]
    pub fn is_tainted(&self) -> bool {
        self.emitted
            .borrow()
            .iter()
            .any(|diag| diag.severity() >= Severity::Error)
    }

    /// Ensure that the transaction is untainted.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the transaction is tainted with one-or-more errors.
    pub fn ensure_untainted(&self) -> Result<()> {
        if self.is_tainted() {
            Err(TaintedError(()).into())
        } else {
            Ok(())
        }
    }

    /// Emits the given diagnostic to the current transaction.
    ///
    /// # Note
    ///
    /// Since the diagnostic is emitted to the current transaction, it will only
    /// be pushed once the transaction is commited (via
    /// [`commit()`])
    ///
    /// [`commit()`]: Transaction::commit()
    #[track_caller]
    pub fn emit<E>(&self, diagnostic: E)
    where
        E: Into<Error>,
    {
        let diagnostic = diagnostic.into();
        if diagnostic.message().as_str() == ERROR_GUARANTEED_CODE {
            return;
        }

        self.emitted.borrow_mut().push(diagnostic);
    }
}

impl Drop for Transaction<'_> {
    fn drop(&mut self) {
        let is_tainted = self.is_tainted();

        match self.on_failure {
            Rollback::Always => self.rollback(),
            Rollback::OnFailure if is_tainted => self.rollback(),
            _ => {}
        }

        match self.on_success {
            Commit::Always => self.commit(),
            Commit::OnSuccess if !is_tainted => self.commit(),
            _ => {}
        }
    }
}

#[derive(Debug, Clone)]
struct TaintedError(());

impl crate::Diagnostic for TaintedError {
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
            let diag = SimpleDiagnostic::new(message).add_related(err.into_diagnostic());

            Box::new(diag) as Error
        })
    }
}
