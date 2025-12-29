/// Executes the function with a spinner, with the given message.
///
/// If the function returns an error, the spinner message will be updated with
/// the error message unless overwritten by an `Err` branch.
///
/// # Examples
///
/// For a simple task:
/// ```rs
/// let compiler_root = task! {
///     "cloning 'lume-lang/lume' compiler..." => {
///         crate::toolchain::clone_version(&version)
///     }
/// }?;
/// ```
///
/// To update the spinner message when the function returns, use the `Ok` and
/// `Err` branch patterns:
/// ```rs
/// let compiler_root = task! {
///     "cloning 'lume-lang/lume' compiler..." => {
///         crate::toolchain::clone_version(&version)
///     },
///     Ok(path) => {
///         format!("cloned 'lume-lang/lume' ({})", path.display())
///     },
///     Err(err) => {
///         format!("failed to clone 'lume-lang/lume' ({})", err.message())
///     }
/// }?;
/// ```
///
/// Note: the returned value from the branches must be able to cast into
/// [`String`] via [`Into`].
#[macro_export]
macro_rules! task {
    // Branchless variant
    ($message:literal => $task:expr) => {{
        let mut sp = spinoff::Spinner::new(spinoff::spinners::Star, $message, spinoff::Color::Blue);
        let result = $task;

        if let Err(err) = result.as_ref() {
            sp.fail(err.message().as_ref());
        } else {
            sp.success($message);
        }

        result
    }};

    // Branching variant
    (
        $message:literal => $task:expr,
        $(Ok($ok_pattern:pat) => $ok_branch:expr)?,
        $(Err($err_pattern:pat) => $err_branch:expr)?
    ) => {{
        let mut sp = spinoff::Spinner::new(spinoff::spinners::Star, $message, spinoff::Color::Blue);
        let result = $task;

        #[allow(unreachable_patterns, reason = "auto-generated")]
        match result.as_ref() {
            $(
                Ok($ok_pattern) => {
                    let success_txt: String = Into::<String>::into($ok_branch);
                    sp.success(success_txt.as_str());
                }
            )?
            $(
                Err($err_pattern) => {
                    let failure_txt: String = Into::<String>::into($err_branch);
                    sp.fail(failure_txt.as_str());
                }
            )?
            _ => {
                sp.success($message);
            }
        };

        result
    }};
}
