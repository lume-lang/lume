/// Creates a new synchronous task with the given message.
///
/// Tasks are meant to show an ongoing process to the user via a spinner in the
/// terminal, accompanied by the given message.
///
/// The task can subsequently be completed with a success or failure message
/// using [`Task::success`] or [`Task::fail`], respectively. To simply finish
/// the task without any message updates, use [`Task::finish`].
///
/// # Examples
///
/// For a simple task:
/// ```
/// use lume_cli_tools::new_task;
///
/// let task_clone_compiler = new_task("cloning 'lume-lang/lume' compiler...");
///
/// // Run the actual computation within the task
/// std::thread::sleep(std::time::Duration::from_millis(20));
///
/// task_clone_compiler.finish();
/// ```
///
/// To update the spinner message when the function returns, use the `Ok` and
/// `Err` branch patterns:
/// ```
/// use lume_cli_tools::new_task;
///
/// let task_clone_compiler = new_task("cloning 'lume-lang/lume' compiler...");
///
/// // Task content simulation
/// let task = || -> Result<(), String> {
///     std::thread::sleep(std::time::Duration::from_millis(20));
///
///     Ok(())
/// };
///
/// match task() {
///     Ok(()) => task_clone_compiler.success("cloned 'lume-lang/lume'"),
///     Err(err) => task_clone_compiler.fail(format!("failed to clone 'lume-lang/lume' ({err})")),
/// }
/// ```
pub fn new_task<S: AsRef<str>>(message: S) -> Task {
    let sp = crate::Spinner::new(message.as_ref(), None);

    Task(sp)
}

/// Represents an ongoing, synchronous task.
pub struct Task(crate::Spinner);

impl Task {
    /// Finish the task without updating the message.
    pub fn finish(mut self) {
        self.0.stop();
    }

    /// Finish the task with the given success message.
    pub fn success<S: Into<String>>(mut self, message: S) {
        self.0.stop_with_success(message);
    }

    /// Finish the task with the given failure message.
    pub fn fail<S: Into<String>>(mut self, message: S) {
        self.0.stop_with_error(message);
    }
}

impl Drop for Task {
    fn drop(&mut self) {
        self.0.stop();
    }
}
